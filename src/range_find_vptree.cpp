#include "init.h"
#include "vptree.h"
#include "range_find_exact.h"

SEXP range_find_vptree(SEXP to_check, SEXP X, SEXP nodes, SEXP dist_thresh, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP    
    VpTree n_finder(X, nodes);
    return range_neighbors(n_finder, to_check, dist_thresh, get_index, get_distance);
    END_RCPP
}
