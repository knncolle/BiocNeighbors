#include "init.h"
#include "kmknn.h"
#include "range_find_exact.h"

SEXP range_find_kmknn(SEXP to_check, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP dist_thresh, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP    
    searcher n_finder(X, clust_centers, clust_info);
    return range_neighbors(n_finder, to_check, dist_thresh, get_index, get_distance);
    END_RCPP
}
