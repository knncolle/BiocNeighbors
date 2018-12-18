#include "init.h"
#include "hnsw.h"
#include "query_knn.h"

SEXP query_hnsw (SEXP to_check, SEXP query, SEXP data, SEXP fname, SEXP dtype, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        Hnsw<L1Space> nn_finder(data, fname);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
    } else {
        Hnsw<hnswlib::L2Space> nn_finder(data, fname);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
     }
    END_RCPP
}
