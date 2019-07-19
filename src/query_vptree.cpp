#include "vptree.h"
#include "distances.h"
#include "query_knn.h"
#include "query_dist_to_k.h"
#include "range_query.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject query_vptree(Rcpp::NumericMatrix query, Rcpp::NumericMatrix X, Rcpp::List nodes, 
    std::string dtype, int nn, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        VpTree<BNManhattan> nn_finder(X, nodes);
        return query_knn(nn_finder, query, nn, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> nn_finder(X, nodes);
        return query_knn(nn_finder, query, nn, get_index, get_distance);
    }
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject query_dist_to_vptree(Rcpp::NumericMatrix query, Rcpp::NumericMatrix X, Rcpp::List nodes, 
    std::string dtype, int nn)
{
    if (dtype=="Manhattan") {
        VpTree<BNManhattan> nn_finder(X, nodes);
        return query_dist_to_k(nn_finder, query, nn);
     } else {
        VpTree<BNEuclidean> nn_finder(X, nodes);
        return query_dist_to_k(nn_finder, query, nn);
    }
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject range_query_vptree(Rcpp::NumericMatrix query, Rcpp::NumericMatrix X, Rcpp::List nodes, 
    std::string dtype, Rcpp::NumericVector dist_thresh, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        VpTree<BNManhattan> finder(X, nodes);
        return range_query_exact(finder, query, dist_thresh, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> finder(X, nodes);
        return range_query_exact(finder, query, dist_thresh, get_index, get_distance);
    }
}
