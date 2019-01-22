#include "vptree.h"
#include "init.h"
#include "distances.h"
#include "range_query_exact.h"

SEXP range_query_vptree(SEXP to_check, SEXP X, SEXP nodes, SEXP dtype, SEXP dist_thresh, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        VpTree<BNManhattan> finder(X, nodes);
        return range_query_exact(finder, to_check, dist_thresh, query, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> finder(X, nodes);
        return range_query_exact(finder, to_check, dist_thresh, query, get_index, get_distance);
    }
    END_RCPP
}

