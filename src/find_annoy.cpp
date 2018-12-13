#include "init.h"
#include "annoy.h"
#include "find_knn.h"

SEXP find_annoy (SEXP to_check, SEXP ndims, SEXP fname, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    Annoy searcher(ndims, fname);
    return find_knn(searcher, to_check, nn, get_index, get_distance);
    END_RCPP
}
