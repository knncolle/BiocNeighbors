#ifndef FIND_DIST_TO_K_H
#define FIND_DIST_TO_K_H
#include "utils.h"

template <class Searcher>
SEXP find_dist_to_k(Searcher& finder, SEXP to_check, SEXP nn, SEXP get_index, SEXP get_distance) {
    // Checking NN's and indices.
    const NumNeighbors_t NN=check_k(nn);
    const Rcpp::IntegerVector points=check_indices(to_check, finder.get_nobs());
    const VecSize_t nobs=points.size();

    Rcpp::NumericVector out_dist(nobs);
    auto odIt=out_dist.begin();

    // Iterating across cells, finding NNs and storing distances. 
    for (auto h : points) {
        finder.find_nearest_neighbors(h, NN, false, true);
        const auto& distances=finder.get_distances();
        *odIt=distances[NN-1];
    }

    return out_dist;
}

#endif
