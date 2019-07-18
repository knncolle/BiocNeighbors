#include "vptree.h"
#include "distances.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject build_vptree (Rcpp::NumericMatrix Mat, std::string dtype) {
    if (dtype=="Manhattan") {
        VpTree<BNManhattan> vp(Mat);
        return vp.save();
     } else {
        VpTree<BNEuclidean> vp(Mat);
        return vp.save();
    }
}
