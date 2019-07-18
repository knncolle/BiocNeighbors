#include "hnsw.h"
#include "find_knn.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_hnsw (Rcpp::IntegerVector to_check, Rcpp::NumericMatrix vals, std::string fname, int ef_search,
    std::string dtype, int nn, bool get_index, bool get_distance) 
{
    if (dtype=="Manhattan") {
        Hnsw<L1Space> searcher(vals, fname, ef_search);
        return find_knn(searcher, to_check, nn, get_index, get_distance);
     } else {
        Hnsw<hnswlib::L2Space> searcher(vals, fname, ef_search);
        return find_knn(searcher, to_check, nn, get_index, get_distance);
    }
}
