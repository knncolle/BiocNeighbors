#include "kmknn.h"
#include "distances.h"
#include "find_knn.h"
#include "range_find.h"
#include "find_dist_to_k.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_kmknn(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix X, 
    Rcpp::NumericMatrix clust_centers, Rcpp::List clust_info, 
    std::string dtype, int nn, bool get_index, bool get_distance, int last) 
{
    if (dtype=="Manhattan") {
        Kmknn<BNManhattan> nn_finder(X, clust_centers, clust_info);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance, last);
     } else {
        Kmknn<BNEuclidean> nn_finder(X, clust_centers, clust_info);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance, last);
    }
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_dist_to_kmknn(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix X, 
    Rcpp::NumericMatrix clust_centers, Rcpp::List clust_info, 
    std::string dtype, int nn)
{
    if (dtype=="Manhattan") {
        Kmknn<BNManhattan> nn_finder(X, clust_centers, clust_info, false);
        return find_dist_to_k(nn_finder, to_check, nn);
     } else {
        Kmknn<BNEuclidean> nn_finder(X, clust_centers, clust_info, false);
        return find_dist_to_k(nn_finder, to_check, nn);
    }
}

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
