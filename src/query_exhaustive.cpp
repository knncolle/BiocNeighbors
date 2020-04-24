#include "exhaustive.h"
#include "distances.h"
#include "query_knn.h"
#include "range_query.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject query_exhaustive(Rcpp::NumericMatrix query, Rcpp::NumericMatrix X,
    std::string dtype, int nn, bool get_index, bool get_distance, int last, bool warn_ties) 
{
    if (dtype=="Manhattan") {
        Exhaustive<BNManhattan> nn_finder(X, warn_ties);
        return query_knn(nn_finder, query, nn, get_index, get_distance, last);
     } else {
        Exhaustive<BNEuclidean> nn_finder(X, warn_ties);
        return query_knn(nn_finder, query, nn, get_index, get_distance, last);
    }
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject range_query_exhaustive(Rcpp::NumericMatrix query, Rcpp::NumericMatrix X, Rcpp::NumericMatrix clust_centers, Rcpp::List clust_info,
    std::string dtype, Rcpp::NumericVector dist_thresh, bool get_index, bool get_distance)
{
    if (dtype=="Manhattan") {
        Exhaustive<BNManhattan> n_finder(X);
        return range_query_exact(n_finder, query, dist_thresh, get_index, get_distance);
     } else {
        Exhaustive<BNEuclidean> n_finder(X);
        return range_query_exact(n_finder, query, dist_thresh, get_index, get_distance);
    }
}
