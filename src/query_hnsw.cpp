#include "hnsw.h"
#include "query_knn.h"

// [[Rcpp::export(rng=false)]]
SEXP query_hnsw (Rcpp::NumericMatrix query, Rcpp::NumericMatrix vals, std::string fname, int ef_search,
    std::string dtype, int nn, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        Hnsw<L1Space> nn_finder(vals, fname, ef_search);
        return query_knn(nn_finder, query, nn, get_index, get_distance);
    } else {
        Hnsw<hnswlib::L2Space> nn_finder(vals, fname, ef_search);
        return query_knn(nn_finder, query, nn, get_index, get_distance);
     }
}
