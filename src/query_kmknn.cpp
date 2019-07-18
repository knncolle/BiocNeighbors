#include "kmknn.h"
#include "distances.h"
#include "query_knn.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject query_kmknn(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix query, 
    Rcpp::NumericMatrix X, Rcpp::NumericMatrix clust_centers, Rcpp::List clust_info, 
    std::string dtype, int nn, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        Kmknn<BNManhattan> nn_finder(X, clust_centers, clust_info);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
     } else {
        Kmknn<BNEuclidean> nn_finder(X, clust_centers, clust_info);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
    }
}
