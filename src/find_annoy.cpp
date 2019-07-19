#include "annoy.h"
#include "find_knn.h"
#include "find_dist_to_k.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_annoy (Rcpp::IntegerVector to_check, int ndims, std::string fname, double search_mult, 
    std::string dtype, int nn, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        Annoy<Manhattan> searcher(ndims, fname, search_mult);
        return find_knn(searcher, to_check, nn, get_index, get_distance);
    } else {
        Annoy<Euclidean> searcher(ndims, fname, search_mult);
        return find_knn(searcher, to_check, nn, get_index, get_distance);
    }
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_dist_to_annoy (Rcpp::IntegerVector to_check, int ndims, std::string fname, double search_mult, 
    std::string dtype, int nn)
{
    if (dtype=="Manhattan") {
        Annoy<Manhattan> searcher(ndims, fname, search_mult);
        return find_dist_to_k(searcher, to_check, nn);
    } else {
        Annoy<Euclidean> searcher(ndims, fname, search_mult);
        return find_dist_to_k(searcher, to_check, nn);
    }
}
