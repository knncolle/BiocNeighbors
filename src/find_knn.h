#ifndef FIND_KNN_H
#define FIND_KNN_H
#include "utils.h"

template <class Searcher>
SEXP find_knn(Searcher& finder, Rcpp::IntegerVector to_check, int nn, bool store_neighbors, bool store_distances, int last) {
    // Checking NN's and indices.
    const NumNeighbors_t NN=check_k(nn);
    const Rcpp::IntegerVector points=check_indices(to_check, finder.get_nobs());
    const VecSize_t nobs=points.size();
    const NumNeighbors_t offset=NN - last;

    Rcpp::NumericMatrix out_dist;
    if (store_distances) {
        out_dist=Rcpp::NumericMatrix(last, nobs);
    }
    auto odIt=out_dist.begin();

    Rcpp::IntegerMatrix out_idx;
    if (store_neighbors) {
        out_idx=Rcpp::IntegerMatrix(last, nobs);
    }
    auto oiIt=out_idx.begin();

    // Iterating across cells, finding NNs and storing distances or neighbors.
    for (auto h : points) {
        finder.find_nearest_neighbors(h, NN, store_neighbors, store_distances);
        const auto& distances=finder.get_distances();
        const auto& neighbors=finder.get_neighbors();

        if (store_distances) {
            std::copy(distances.begin() + offset, distances.end(), odIt);
            odIt+=last;
        }
        if (store_neighbors) {
            std::copy(neighbors.begin() + offset, neighbors.end(), oiIt);
            for (NumNeighbors_t i=0; i<last; ++i, ++oiIt) {
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
}

#endif
