#include "init.h"
#include "vptree.h"
#include "distances.h"
#include "find_knn.h"

SEXP find_vptree(SEXP to_check, SEXP X, SEXP nodes, SEXP dtype, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        VpTree<BNManhattan> nn_finder(X, nodes);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> nn_finder(X, nodes);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance);
     }
    END_RCPP
}
