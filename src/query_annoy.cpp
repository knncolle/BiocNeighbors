#include "init.h"
#include "annoy.h"
#include "query_exact.h"

SEXP query_annoy (SEXP to_check, SEXP query, SEXP ndims, SEXP fname, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    Annoy nn_finder(ndims, fname);
    return query_exact(nn_finder, to_check, nn, query, get_index, get_distance);
    END_RCPP
}
