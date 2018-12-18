#include "init.h"
#include "vptree.h"
#include "distances.h"
#include "query_knn.h"

SEXP query_vptree(SEXP to_check, SEXP X, SEXP nodes, SEXP dtype, SEXP nn, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        VpTree<BNManhattan> nn_finder(X, nodes);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
     } else {
        VpTree<BNEuclidean> nn_finder(X, nodes);
        return query_knn(nn_finder, to_check, nn, query, get_index, get_distance);
    }
    END_RCPP
}
