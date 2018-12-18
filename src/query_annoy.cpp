#include "init.h"
#include "annoy.h"
#include "query_knn.h"

SEXP query_annoy (SEXP to_check, SEXP query, SEXP ndims, SEXP fname, SEXP dtype, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        Annoy<Manhattan> nn_finder(ndims, fname);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
     } else {
        Annoy<Euclidean> nn_finder(ndims, fname);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
    }
    END_RCPP
}
