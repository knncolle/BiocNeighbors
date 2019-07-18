#include "kmknn.h"
#include "distances.h"
#include "range_find_exact.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject range_find_kmknn(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix X, Rcpp::NumericMatrix clust_centers, Rcpp::List clust_info, 
    std::string dtype, Rcpp::NumericVector dist_thresh, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        Kmknn<BNManhattan> n_finder(X, clust_centers, clust_info);
        return range_neighbors(n_finder, to_check, dist_thresh, get_index, get_distance);
    } else {
        Kmknn<BNEuclidean> n_finder(X, clust_centers, clust_info);
        return range_neighbors(n_finder, to_check, dist_thresh, get_index, get_distance);
    }
}
