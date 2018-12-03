#include "vptree.h"
#include "init.h"
#include "range_query_exact.h"

SEXP range_query_vptree(SEXP to_check, SEXP X, SEXP nodes, SEXP dist_thresh, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    VpTree finder(X, nodes);
    return range_query_exact(finder, to_check, dist_thresh, query, get_index, get_distance);
    END_RCPP
}

