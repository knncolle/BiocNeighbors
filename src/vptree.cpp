#include "vptree.h"

DataPoint::DataPoint() : ptr(NULL), index(-1) {}
    
DataPoint::DataPoint (int i, const double* p) : ptr(p), index(i) {}

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
            auto curcol=vals.column(I.index);
            std::copy(curcol.begin(), curcol.end(), ptr);
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
        return -1;
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
        (*iiIt)=I.index; 
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

VpTree::VpTree(Rcpp::List saved) {
    if (saved.size()!=6) {
        throw std::runtime_error("VP tree index list must have 6 elements");
    }
    
    reference=Rcpp::NumericMatrix(Rcpp::RObject(saved[0]));
    ndim=reference.nrow();
    const int nelements=reference.ncol();
   
    { // Filling the item index.
        Rcpp::IntegerVector item_index=saved[1];
        if (nelements!=item_index.size()) {
            throw std::runtime_error("VP tree item index vector length is not equal to number of elements");
        }

        items.reserve(nelements);
        const double* ptr=reference.begin();
        for (auto i : item_index) {
            if (i < 0 || i>= nelements) {
                throw std::runtime_error("VP tree item indices out of range");
            }
            items.push_back(DataPoint(i, ptr));
            ptr+=ndim;
        }
    }

    { // Filling the node index.
        Rcpp::IntegerVector node_index=saved[2];
        Rcpp::IntegerVector node_left=saved[3];
        Rcpp::IntegerVector node_right=saved[4];
        Rcpp::NumericVector node_thresholds=saved[5];

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

