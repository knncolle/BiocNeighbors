#include "init.h"
#include "hnsw.h"
#include "find_knn.h"

SEXP find_hnsw (SEXP to_check, SEXP vals, SEXP fname, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    Hnsw searcher(vals, fname);
    return find_knn(searcher, to_check, nn, get_index, get_distance);
    END_RCPP
}
