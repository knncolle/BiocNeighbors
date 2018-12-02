#include "init.h"
#include "vptree.h"
#include "template_find_exact.h"

SEXP find_vptree(SEXP to_check, SEXP X, SEXP ordering, SEXP nodes, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    VpTree nn_finder(X, ordering, nodes);
    return template_find_exact(nn_finder, to_check, nn, get_index, get_distance);
    END_RCPP
}
