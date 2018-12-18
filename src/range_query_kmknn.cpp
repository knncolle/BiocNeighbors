#include "init.h"
#include "kmknn.h"
#include "distances.h"
#include "range_query_exact.h"

SEXP range_query_kmknn(SEXP to_check, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP dtype, SEXP dist_thresh, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        Kmknn<BNManhattan> n_finder(X, clust_centers, clust_info);
        return range_query_exact(n_finder, to_check, dist_thresh, query, get_index, get_distance);
     } else {
        Kmknn<BNEuclidean> n_finder(X, clust_centers, clust_info);
        return range_query_exact(n_finder, to_check, dist_thresh, query, get_index, get_distance);
    }
    END_RCPP
}

