#include "annoy.h"
#include "query_knn.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject query_annoy (Rcpp::NumericMatrix query, int ndims, std::string fname, double search_mult, 
    std::string dtype, int nn, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        Annoy<Manhattan> nn_finder(ndims, fname, search_mult);
        return query_knn(nn_finder, query, nn, get_index, get_distance);
     } else {
        Annoy<Euclidean> nn_finder(ndims, fname, search_mult);
        return query_knn(nn_finder, query, nn, get_index, get_distance);
    }
}
