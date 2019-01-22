#include "init.h"
#include "hnsw.h"
#include "find_knn.h"

SEXP find_hnsw (SEXP to_check, SEXP vals, SEXP fname, SEXP ef_search, SEXP dtype, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        Hnsw<L1Space> searcher(vals, fname, ef_search);
        return find_knn(searcher, to_check, nn, get_index, get_distance);
     } else {
        Hnsw<hnswlib::L2Space> searcher(vals, fname, ef_search);
        return find_knn(searcher, to_check, nn, get_index, get_distance);
    }
    END_RCPP
}
