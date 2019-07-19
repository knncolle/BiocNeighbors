#ifndef QUERY_DIST_TO_K_H
#define QUERY_DIST_TO_K_H
#include "utils.h"

template <class Searcher>
SEXP query_dist_to_k(Searcher& finder, Rcpp::NumericMatrix query, int nn) {
    // Checking NN's and indices.
    const NumNeighbors_t NN=check_k(nn);

    const MatDim_t ndim=query.nrow();
    if (query.nrow()!=ndim) {
        throw std::runtime_error("'query' and 'X' have different dimensionality");
    }
    const MatDim_t nobs=query.ncol();

    Rcpp::NumericVector out_dist(nobs);
    auto odIt=out_dist.begin();

    // Iterating across cells, finding NNs and storing distances. 
    for (auto qIt=query.begin(); qIt!=query.end(); qIt+=ndim, ++odIt) {
        finder.find_nearest_neighbors(qIt, NN, false, true);
        const auto& distances=finder.get_distances();
        *odIt=distances[NN-1];
    }

    return out_dist;
}

#endif
