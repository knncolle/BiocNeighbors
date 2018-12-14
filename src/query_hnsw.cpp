#include "init.h"
#include "hnsw.h"
#include "query_knn.h"

SEXP query_hnsw (SEXP to_check, SEXP query, SEXP data, SEXP fname, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    Hnsw nn_finder(data, fname);
    return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
    END_RCPP
}
