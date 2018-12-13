#include "init.h"
#include "kmknn.h"
#include "find_knn.h"

SEXP find_kmknn(SEXP to_check, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    Kmknn nn_finder(X, clust_centers, clust_info);
    return find_knn(nn_finder, to_check, nn, get_index, get_distance);
    END_RCPP
}
