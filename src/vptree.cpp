#include "vptree.h"

DataPoint::DataPoint() : ptr(NULL), index(-1) {}
    
DataPoint::DataPoint (int i, const double* p) : ptr(p), index(i) {}

/***** Getter methods *****/

int VpTree::get_nobs() const { return reference.ncol(); }

int VpTree::get_ndims() const { return reference.nrow(); }

std::deque<size_t>& VpTree::get_neighbors () { return neighbors; }

std::deque<double>& VpTree::get_distances () { return distances; }

/***** Methods to build the VP tree *****/

VpTree::VpTree(Rcpp::NumericMatrix vals) : ndim(vals.nrow()) {
    const int nelements=vals.ncol();
    {
        items.reserve(nelements);
        const double * ptr = vals.begin();
        for (int i=0; i<nelements; ++i, ptr+=ndim) {
            items.push_back(DataPoint(i, ptr));
        }

        Rcpp::RNGScope rnger;
        buildFromPoints(0, nelements);
    }

    // Reordering points in the reference.
    reference=Rcpp::NumericMatrix(ndim, nelements);
    {
        double* ptr=reference.begin();
        for (auto& I : items) {
            std::copy(I.ptr, I.ptr+ndim, ptr);
            I.ptr=ptr;
            ptr+=ndim;
        }
    }
    return;
}

double euclidean_dist2(const double* x, const double* y, int d) {
    double dist=0;
    for (int i=0; i<d; ++i, ++x, ++y) {
        double diff=*x - *y;
        dist += diff * diff;
    }
    return dist;
}

// Euclidean distance comparator for use in std::nth_element
struct DistanceComparator
{
    const DataPoint& item;
    const int ndim;
    DistanceComparator(const DataPoint& item, int d) : item(item), ndim(d) {}
    bool operator()(const DataPoint& a, const DataPoint& b) {
        return euclidean_dist2(item.ptr, a.ptr, ndim) < euclidean_dist2(item.ptr, b.ptr, ndim);
    }
};

int VpTree::buildFromPoints( int lower, int upper )
{
    if (upper == lower) {     // indicates that we're done here!
        return LEAF_MARKER;
    }

    // Lower index is center of current node
    int pos=nodes.size();
    nodes.push_back(Node(lower));
    Node& node=nodes.back();

    if (upper - lower > 1) {      // if we did not arrive at leaf yet

        // Choose an arbitrary point and move it to the start
        int i = static_cast<int>(R::unif_rand() * static_cast<double>(upper - lower - 1)) + lower;
        std::swap(items[lower], items[i]);
        
        // Partition around the median distance
        int median = (upper + lower) / 2;
        std::nth_element(items.begin() + lower + 1,
                         items.begin() + median,
                         items.begin() + upper,
                         DistanceComparator(items[lower], ndim));
       
        // Threshold of the new node will be the distance to the median
        node.threshold = std::sqrt(euclidean_dist2(items[lower].ptr, items[median].ptr, ndim));
        
        // Recursively build tree
        node.left = buildFromPoints(lower + 1, median);
        node.right = buildFromPoints(median, upper);
    }
    
    // Return result
    return pos;
}

Rcpp::List VpTree::save() {
    // Saving item ordering.
    Rcpp::IntegerVector item_index(items.size());
    auto iiIt=item_index.begin();
    for (auto& I : items) { 
        (*iiIt)=I.index + 1; 
        ++iiIt;
    }

    // Saving node structure.
    auto Nnodes=nodes.size();
    Rcpp::IntegerVector node_index(Nnodes), node_left(Nnodes), node_right(Nnodes);
    Rcpp::NumericVector node_thresholds(Nnodes);
    auto niIt=node_index.begin(),
         nlIt=node_left.begin(),
         nrIt=node_right.begin();
    auto ntIt=node_thresholds.begin();

    for (auto& N : nodes) {
        (*niIt)=N.index;
        (*nlIt)=N.left;
        (*nrIt)=N.right;
        (*ntIt)=N.threshold;
        ++niIt; ++nlIt; ++nrIt; ++ntIt;
    }
    
    return Rcpp::List::create(reference, item_index, node_index, node_left, node_right, node_thresholds);
}

VpTree::VpTree(Rcpp::NumericMatrix vals, Rcpp::IntegerVector item_index, Rcpp::List node_data) : reference(vals), ndim(vals.nrow()) {
    const int nelements=reference.ncol();
    { // Filling the item index.
        if (nelements!=item_index.size()) {
            throw std::runtime_error("VP tree item index vector length is not equal to number of elements");
        }

        items.reserve(nelements);
        const double* ptr=reference.begin();
        for (auto i : item_index) {
            if (i < 0 || i>= nelements) {
                throw std::runtime_error("VP tree item indices out of range");
            }
            items.push_back(DataPoint(i-1, ptr));
            ptr+=ndim;
        }
    }

    { // Filling the node index.
        if (node_data.size()!=4) {
            throw std::runtime_error("VP tree index list must have 4 elements");
        }
    
        Rcpp::IntegerVector node_index=node_data[0];
        Rcpp::IntegerVector node_left=node_data[1];
        Rcpp::IntegerVector node_right=node_data[2];
        Rcpp::NumericVector node_thresholds=node_data[3];

        const int nnodes=node_index.size();
        if (node_left.size()!=nnodes || node_right.size()!=nnodes || node_thresholds.size()!=nnodes) {
            throw std::runtime_error("VP tree node index vector lengths are not consistent");
        }

        for (int i=0; i<nnodes; ++i) {
            nodes.push_back(Node(node_index[i]));
            Node& curnode=nodes.back();
            curnode.left=node_left[i];
            curnode.right=node_right[i];

            if (curnode.index < 0 || curnode.index >= nnodes ||
                    curnode.left < 0 || curnode.left >= nnodes ||
                    curnode.right < 0 || curnode.right >= nnodes) {
                throw std::runtime_error("VP tree node indices out of range");
            }

            curnode.threshold=node_thresholds[i];
        }
    }
    
    Rcpp::RNGScope rnger;
    buildFromPoints(0, nelements);
    return;
}

/***** Methods to search the VP tree for nearest neighbors *****/

void VpTree::find_nearest_neighbors (int cell, int k, bool index, bool dist) {
    if (cell >= reference.ncol()) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=reference.column(cell);
    search_nn(curcol.begin(), k+1, index, dist, true, cell);
    return;
}

void VpTree::find_nearest_neighbors (const double* current, int k, bool index, bool dist) {
    search_nn(current, k, index, dist, false, 0);
    return;
}


void VpTree::search_nn(const double* target, int k, bool index, bool dist, bool discard_self, int self) {
    // Use a priority queue to store intermediate results.
    std::priority_queue<HeapItem> heap;
    tau = DBL_MAX;
    search(0, target, k, heap);

    neighbors.clear(); 
    distances.clear();
    bool found_self=false;
    while(!heap.empty()) {
        if (discard_self && heap.top().index==self) {
            found_self=true;
            heap.pop();
            continue;
        }
        if (index) {
            neighbors.push_front(heap.top().index);
        }
        if (dist) {
            distances.push_front(heap.top().dist);
        }
	    heap.pop();
    }

    // Removing last element if we found self. 
    if (discard_self && !found_self) {
        if (index) { 
            neighbors.pop_back();
        }
        if (dist) {
            distances.pop_back();
        }
    }
    return;
}

void VpTree::search(int curnode_index, const double* target, int k, std::priority_queue<HeapItem>& heap) {
    if(curnode_index == LEAF_MARKER) { // indicates that we're done here
        return;
    }
    
    // Compute distance between target and current node
    const auto& curnode=nodes[curnode_index];
    double dist = std::sqrt(euclidean_dist2(items[curnode.index].ptr, target, ndim));

    // If current node within radius tau
    if (dist < tau) {
        if (heap.size() == k) {
            heap.pop(); // remove furthest node from result list (if we already have k results)
        }

        heap.push(HeapItem(curnode.index, dist)); // add current node to result list

        if (heap.size() == k) {
            tau = heap.top().dist; // update value of tau (farthest point in result list)
        }
    }
    
    // Return if we arrived at a leaf
    if (curnode.left == LEAF_MARKER && curnode.right == LEAF_MARKER) {
        return;
    }
    
    // If the target lies within the radius of ball
    if (dist < curnode.threshold) {
        if (dist - tau <= curnode.threshold) {         // if there can still be neighbors inside the ball, recursively search left child first
            search(curnode.left, target, k, heap);
        }
        
        if (dist + tau >= curnode.threshold) {         // if there can still be neighbors outside the ball, recursively search right child
            search(curnode.right, target, k, heap);
        }
    
    // If the target lies outsize the radius of the ball
    } else {
        if (dist + tau >= curnode.threshold) {         // if there can still be neighbors outside the ball, recursively search right child first
            search(curnode.right, target, k, heap);
        }
        
        if (dist - tau <= curnode.threshold) {         // if there can still be neighbors inside the ball, recursively search left child
            search(curnode.left, target, k, heap);
        }
    }
}
