#include "init.h"
#include "vptree.h"
#include "query_exact.h"

SEXP query_vptree(SEXP to_check, SEXP X, SEXP nodes, SEXP nn, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    VpTree nn_finder(X, nodes);
    return query_exact(nn_finder, to_check, nn, query, get_index, get_distance);
    END_RCPP
}
