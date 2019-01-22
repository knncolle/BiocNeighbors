#include "init.h"
#include "kmknn.h"
#include "distances.h"
#include "find_knn.h"

SEXP find_kmknn(SEXP to_check, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP dtype, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        Kmknn<BNManhattan> nn_finder(X, clust_centers, clust_info);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance);
     } else {
        Kmknn<BNEuclidean> nn_finder(X, clust_centers, clust_info);
        return find_knn(nn_finder, to_check, nn, get_index, get_distance);
    }
    END_RCPP
}
