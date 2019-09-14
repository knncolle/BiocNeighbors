#include "annoy.h"
#include "query_knn.h"
#include "query_dist_to_k.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject query_annoy (Rcpp::NumericMatrix query, int ndims, std::string fname, double search_mult, 
    std::string dtype, int nn, bool get_index, bool get_distance, int last) 
{
    if (dtype=="Manhattan") {
        Annoy<Manhattan> nn_finder(ndims, fname, search_mult);
        return query_knn(nn_finder, query, nn, get_index, get_distance, last);
     } else {
        Annoy<Euclidean> nn_finder(ndims, fname, search_mult);
        return query_knn(nn_finder, query, nn, get_index, get_distance, last);
    }
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject query_dist_to_annoy (Rcpp::NumericMatrix query, int ndims, std::string fname, double search_mult, 
    std::string dtype, int nn)
{
    if (dtype=="Manhattan") {
        Annoy<Manhattan> nn_finder(ndims, fname, search_mult);
        return query_dist_to_k(nn_finder, query, nn);
     } else {
        Annoy<Euclidean> nn_finder(ndims, fname, search_mult);
        return query_dist_to_k(nn_finder, query, nn);
    }
}
