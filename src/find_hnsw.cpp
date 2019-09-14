#include "hnsw.h"
#include "find_knn.h"
#include "find_dist_to_k.h"

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_hnsw (Rcpp::IntegerVector to_check, Rcpp::NumericMatrix vals, std::string fname, int ef_search,
    std::string dtype, int nn, bool get_index, bool get_distance, int last) 
{
    if (dtype=="Manhattan") {
        Hnsw<L1Space> searcher(vals, fname, ef_search);
        return find_knn(searcher, to_check, nn, get_index, get_distance, last);
     } else {
        Hnsw<hnswlib::L2Space> searcher(vals, fname, ef_search);
        return find_knn(searcher, to_check, nn, get_index, get_distance, last);
    }
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject find_dist_to_hnsw(Rcpp::IntegerVector to_check, Rcpp::NumericMatrix vals, std::string fname, int ef_search,
    std::string dtype, int nn)
{
    if (dtype=="Manhattan") {
        Hnsw<L1Space> searcher(vals, fname, ef_search);
        return find_dist_to_k(searcher, to_check, nn);
     } else {
        Hnsw<hnswlib::L2Space> searcher(vals, fname, ef_search);
        return find_dist_to_k(searcher, to_check, nn);
    }
}
