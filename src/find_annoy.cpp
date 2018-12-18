#include "init.h"
#include "annoy.h"
#include "find_knn.h"

SEXP find_annoy (SEXP to_check, SEXP ndims, SEXP fname, SEXP dtype, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        Annoy<Manhattan> searcher(ndims, fname);
        return find_knn(searcher, to_check, nn, get_index, get_distance);
    } else {
        Annoy<Euclidean> searcher(ndims, fname);
        return find_knn(searcher, to_check, nn, get_index, get_distance);
    }
    END_RCPP
}
