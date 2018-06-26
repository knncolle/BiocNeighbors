#include "objects.h"
#include "utils.h"

SEXP query_knn(SEXP to_check, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP nn, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    auto searcher=generate_holder(X, clust_centers, clust_info);
    const size_t ndim=searcher->get_ndims();
    const size_t NN=check_k(nn);

    // Examining the query matrix and checking it against the subset indices.
    Rcpp::NumericMatrix Query(query);
    if (size_t(Query.nrow())!=ndim) {
        throw std::runtime_error("'query' and 'X' have different dimensionality");
    }

    const Rcpp::IntegerVector points=check_indices(to_check, Query.ncol());
    const size_t nobs=points.size();

    // Getting the output mode.
    const bool store_neighbors=check_logical_scalar(get_index, "'get.index'");
    const bool store_distances=check_logical_scalar(get_distance, "'get.distance'");

    Rcpp::NumericMatrix out_dist;
    if (store_distances) { 
        out_dist=Rcpp::NumericMatrix(NN, nobs);
    }
    auto odIt=out_dist.begin();

    Rcpp::IntegerMatrix out_idx;
    if (store_neighbors) {
        out_idx=Rcpp::IntegerMatrix(NN, nobs);
    }
    auto oiIt=out_idx.begin();
        
    // Iterating across cells, finding NNs and storing distances or neighbors.
    for (auto h : points) {
        searcher->find_nearest_neighbors(Query.begin() + ndim * h, NN, store_distances); 
        const std::deque<double>& distances=searcher->get_distances();
        const std::deque<size_t>& neighbors=searcher->get_neighbors();

        if (store_distances) {
            std::copy(distances.begin(), distances.end(), odIt);
            odIt+=NN;
        }
        if (store_neighbors) {
            std::copy(neighbors.begin(), neighbors.end(), oiIt);
            for (size_t i=0; i<NN; ++i, ++oiIt) {
                ++(*oiIt); // getting back to 1-indexed.
            }
        }
    }

    Rcpp::List output(2, R_NilValue);
    if (store_neighbors) {
        output[0]=out_idx;
    }   
    if (store_distances) {
        output[1]=out_dist;
    }
    return output;
    END_RCPP
}
