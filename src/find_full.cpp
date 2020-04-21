#include "full.h"
#include "distances.h"
#include "find_knn.h"
#include "range_find.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_full(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix X, 
    std::string dtype, int nn, bool get_index, bool get_distance, int last, bool warn_ties) 
{
    if (dtype=="Manhattan") {
        Full<BNManhattan> nn_finder(X, warn_ties);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance, last);
     } else {
        Full<BNEuclidean> nn_finder(X, warn_ties);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance, last);
    }
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject range_find_full(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix X,
    std::string dtype, Rcpp::NumericVector dist_thresh, bool get_index, bool get_distance)
{
    if (dtype=="Manhattan") {
        Full<BNManhattan> n_finder(X);
        return range_neighbors(n_finder, to_check, dist_thresh, get_index, get_distance);
    } else {
        Full<BNEuclidean> n_finder(X);
        return range_neighbors(n_finder, to_check, dist_thresh, get_index, get_distance);
    }
}
