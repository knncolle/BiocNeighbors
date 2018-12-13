#include "init.h"
#include "annoy.h"
#include "find_exact.h"

SEXP find_annoy (SEXP to_check, SEXP ndims, SEXP fname, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    Annoy searcher(ndims, fname);
    return find_exact(searcher, to_check, nn, get_index, get_distance);
    END_RCPP
}
