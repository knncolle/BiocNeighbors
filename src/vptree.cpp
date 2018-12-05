#include "vptree.h"
#include "utils.h"

/***** Getter methods *****/

MatDim_t VpTree::get_nobs() const { return reference.ncol(); }

MatDim_t VpTree::get_ndims() const { return reference.nrow(); }

std::deque<CellIndex_t>& VpTree::get_neighbors () { return neighbors; }

std::deque<double>& VpTree::get_distances () { return distances; }

/***** Methods to build the VP tree *****/

VpTree::VpTree(Rcpp::NumericMatrix vals) : reference(vals), ndim(vals.nrow()) {
    const MatDim_t nelements=vals.ncol();
    items.reserve(nelements);
    const double * ptr = vals.begin();
    for (MatDim_t i=0; i<nelements; ++i, ptr+=ndim) {
        items.push_back(DataPoint(i, ptr));
    }

    Rcpp::RNGScope rnger;
    buildFromPoints(0, nelements);
    return;
}

double euclidean_dist2(const double* x, const double* y, MatDim_t d) {
    double dist=0;
    for (MatDim_t i=0; i<d; ++i, ++x, ++y) {
        const double diff=*x - *y;
        dist += diff * diff;
    }
    return dist;
}

// Euclidean distance comparator for use in std::nth_element
struct VpTree::DistanceComparator
{
    const DataPoint& item;
    const MatDim_t ndim;
    DistanceComparator(const DataPoint& item, MatDim_t d) : item(item), ndim(d) {}
    bool operator()(const DataPoint& a, const DataPoint& b) {
        return euclidean_dist2(item.second, a.second, ndim) < euclidean_dist2(item.second, b.second, ndim);
    }
};

VpTree::NodeIndex_t VpTree::buildFromPoints(NodeIndex_t lower, NodeIndex_t upper)
{
    if (upper == lower) {     // indicates that we're done here!
        return LEAF_MARKER;
    }

    // Lower index is center of current node
    NodeIndex_t pos=nodes.size();
    nodes.push_back(Node(lower));
    Node& node=nodes.back();
        
    int gap=upper - lower;
    if (gap > 1) {      // if we did not arrive at leaf yet

        // Choose an arbitrary point and move it to the start
        NodeIndex_t i = static_cast<NodeIndex_t>(R::unif_rand() * static_cast<double>(gap - 1)) + lower;
        std::swap(items[lower], items[i]);
        
        // Partition around the median distance
        NodeIndex_t median = lower + gap/2;
        std::nth_element(items.begin() + lower + 1,
                         items.begin() + median,
                         items.begin() + upper,
                         DistanceComparator(items[lower], ndim));
       
        // Threshold of the new node will be the distance to the median
        node.threshold = std::sqrt(euclidean_dist2(items[lower].second, items[median].second, ndim));
        
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
        (*iiIt)=I.first + 1; 
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
    
    return Rcpp::List::create(item_index, node_index, node_left, node_right, node_thresholds);
}

VpTree::VpTree(Rcpp::NumericMatrix vals, Rcpp::List node_data) : reference(vals), ndim(vals.nrow()) {
    const MatDim_t nelements=reference.ncol();

    { // Filling the item index.
        items.reserve(nelements);
        const double* ptr=reference.begin();
        for (MatDim_t i=0; i<nelements; ++i, ptr+=ndim) {
            items.push_back(DataPoint(i, ptr));
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

        if (node_left.size()!=node_index.size() || 
                node_right.size()!=node_index.size() || 
                node_thresholds.size()!=node_index.size()) {
            throw std::runtime_error("VP tree node index vector lengths are not consistent");
        }

        const NodeIndex_t nnodes=node_index.size();
        for (NodeIndex_t i=0; i<nnodes; ++i) {
            nodes.push_back(Node(node_index[i]));
            Node& curnode=nodes.back();
            curnode.left=node_left[i];
            curnode.right=node_right[i];

            if (curnode.index < 0 || curnode.index >= nnodes ||
                    (curnode.left != LEAF_MARKER && (curnode.left < 0 || curnode.left >= nnodes)) ||
                    (curnode.right != LEAF_MARKER && (curnode.right < 0 || curnode.right >= nnodes))
                    ) {
                throw std::runtime_error("VP tree node indices out of range");
            }

            curnode.threshold=node_thresholds[i];
        }
    }
    return;
}

/***** Methods to search the VP tree for nearest neighbors *****/

void VpTree::find_nearest_neighbors (CellIndex_t cell, NumNeighbors_t k, const bool index, const bool dist) {
    if (cell >= static_cast<CellIndex_t>(reference.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    tau = DBL_MAX;
    nearest.setup(k, true);
    auto curcol=reference.column(cell);
    search_nn(0, curcol.begin(), nearest);
    nearest.report(neighbors, distances, index, dist, false, cell);
    return;
}

void VpTree::find_nearest_neighbors (const double* current, NumNeighbors_t k, const bool index, const bool dist) {
    tau = DBL_MAX;
    nearest.setup(k, false);
    search_nn(0, current, nearest);
    nearest.report(neighbors, distances, index, dist, false);
    return;
}

void VpTree::search_nn(NodeIndex_t curnode_index, const double* target, neighbor_queue& nearest) { 
    // final argument is not strictly necessary but makes dependencies more obvious.

    if (curnode_index == LEAF_MARKER) { // indicates that we're done here
        return;
    }
    
    // Compute distance between target and current node
    const auto& curnode=nodes[curnode_index];
    double dist = std::sqrt(euclidean_dist2(items[curnode.index].second, target, ndim));

    // If current node within radius tau
    if (dist < tau) {
        nearest.add(curnode.index, dist);
        if (nearest.is_full()) {
            tau = nearest.limit(); // update value of tau (farthest point in result list)
        }
    }
    
    // Return if we arrived at a leaf
    if (curnode.left == LEAF_MARKER && curnode.right == LEAF_MARKER) {
        return;
    }
    
    // If the target lies within the radius of ball
    if (dist < curnode.threshold) {
        if (dist - tau <= curnode.threshold) {         // if there can still be neighbors inside the ball, recursively search left child first
            search_nn(curnode.left, target, nearest);
        }
        
        if (dist + tau >= curnode.threshold) {         // if there can still be neighbors outside the ball, recursively search right child
            search_nn(curnode.right, target, nearest);
        }
    
    // If the target lies outsize the radius of the ball
    } else {
        if (dist + tau >= curnode.threshold) {         // if there can still be neighbors outside the ball, recursively search right child first
            search_nn(curnode.right, target, nearest);
        }
        
        if (dist - tau <= curnode.threshold) {         // if there can still be neighbors inside the ball, recursively search left child
            search_nn(curnode.left, target, nearest);
        }
    }
}

/***** Methods to search the VP tree for all neighbors *****/

void VpTree::find_neighbors (CellIndex_t cell, double threshold, const bool index, const bool dist) {
    neighbors.clear();
    distances.clear();
    if (cell >= static_cast<CellIndex_t>(reference.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=reference.column(cell);
    if (!nodes.empty()) {
        search_all(0, curcol.begin(), threshold, index, dist);
    }
    return;
}

void VpTree::find_neighbors (const double* current, double threshold, const bool index, const bool dist) {
    neighbors.clear();
    distances.clear();
    if (!nodes.empty()) {
        search_all(0, current, threshold, index, dist);
    }
    return;
}

void VpTree::search_all(NodeIndex_t curnode_index, const double* target, double thresh, bool index, bool keepdist) {
    if(curnode_index == LEAF_MARKER) { // indicates that we're done here
        return;
    }
    
    // Compute distance between target and current node
    const auto& curnode=nodes[curnode_index];
    double dist = std::sqrt(euclidean_dist2(items[curnode.index].second, target, ndim));

    // If current node within radius thresh
    if (dist < thresh) {
        if (index) {
            neighbors.push_back(curnode.index);
        }
        if (keepdist) {
            distances.push_back(dist);
        }
    }
    
    // Return if we arrived at a leaf
    if (curnode.left == LEAF_MARKER && curnode.right == LEAF_MARKER) {
        return;
    }
    
    // If the target lies within the radius of ball
    if (dist < curnode.threshold) {
        if (dist - thresh <= curnode.threshold) {         // if there can still be neighbors inside the ball, recursively search left child first
            search_all(curnode.left, target, thresh, index, keepdist);
        }
        
        if (dist + thresh >= curnode.threshold) {         // if there can still be neighbors outside the ball, recursively search right child
            search_all(curnode.right, target, thresh, index, keepdist);
        }
    
    // If the target lies outsize the radius of the ball
    } else {
        if (dist + thresh >= curnode.threshold) {         // if there can still be neighbors outside the ball, recursively search right child first
            search_all(curnode.right, target, thresh, index, keepdist);
        }
        
        if (dist - thresh <= curnode.threshold) {         // if there can still be neighbors inside the ball, recursively search left child
            search_all(curnode.left, target, thresh, index, keepdist);
        }
    }
    return;
}
