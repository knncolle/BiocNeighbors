#include "objects.h"
#include "utils.h"

SEXP find_knn(SEXP to_check, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP

    const int NN0=check_integer_scalar(nn, "'k'");
    if (NN0<1) { 
        throw std::runtime_error("'k' must be positive");
    }
    const size_t NN=NN0;

    auto searcher=generate_holder(X, clust_centers, clust_info);

    // Figuring out which indices we're using.
    const Rcpp::IntegerVector points=check_indices(to_check, searcher->get_nobs());
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
        searcher->find_nearest_neighbors(h, NN, store_distances); 
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
