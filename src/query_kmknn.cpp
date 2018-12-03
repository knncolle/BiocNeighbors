#include "kmknn.h"
#include "init.h"
#include "query_exact.h"

SEXP query_kmknn(SEXP to_check, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP nn, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    searcher nn_finder(X, clust_centers, clust_info);
    return query_exact(nn_finder, to_check, nn, query, get_index, get_distance);
    END_RCPP
}
