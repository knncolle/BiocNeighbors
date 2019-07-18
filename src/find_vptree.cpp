#include "vptree.h"
#include "distances.h"
#include "find_knn.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_vptree(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix X, Rcpp::List nodes, 
    std::string dtype, int nn, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        VpTree<BNManhattan> nn_finder(X, nodes);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> nn_finder(X, nodes);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance);
     }
}
