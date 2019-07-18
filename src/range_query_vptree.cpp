#include "vptree.h"
#include "distances.h"
#include "range_query_exact.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject range_query_vptree(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix query, Rcpp::NumericMatrix X, Rcpp::List nodes, 
    std::string dtype, Rcpp::NumericVector dist_thresh, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        VpTree<BNManhattan> finder(X, nodes);
        return range_query_exact(finder, to_check, dist_thresh, query, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> finder(X, nodes);
        return range_query_exact(finder, to_check, dist_thresh, query, get_index, get_distance);
    }
}
