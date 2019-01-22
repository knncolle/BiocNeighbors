#include "init.h"
#include "vptree.h"
#include "distances.h"
#include "range_find_exact.h"

SEXP range_find_vptree(SEXP to_check, SEXP X, SEXP nodes, SEXP dtype, SEXP dist_thresh, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP    
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        VpTree<BNManhattan> n_finder(X, nodes);
        return range_neighbors(n_finder, to_check, dist_thresh, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> n_finder(X, nodes);
        return range_neighbors(n_finder, to_check, dist_thresh, get_index, get_distance);
    }
    END_RCPP
}
