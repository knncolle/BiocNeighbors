#include "init.h"
#include "vptree.h"
#include "find_knn.h"

SEXP find_vptree(SEXP to_check, SEXP X, SEXP nodes, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    VpTree nn_finder(X, nodes);
    return find_knn(nn_finder, to_check, nn, get_index, get_distance);
    END_RCPP
}
