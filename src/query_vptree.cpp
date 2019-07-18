#include "vptree.h"
#include "distances.h"
#include "query_knn.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject query_vptree(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix query, Rcpp::NumericMatrix X, Rcpp::List nodes, 
    std::string dtype, int nn, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        VpTree<BNManhattan> nn_finder(X, nodes);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> nn_finder(X, nodes);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
    }
}
