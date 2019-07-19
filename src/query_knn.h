#ifndef QUERY_KNN_H
#define QUERY_KNN_H
#include "utils.h"

template <class Searcher>
SEXP query_knn(Searcher& finder, Rcpp::NumericMatrix query, int nn, bool store_neighbors, bool store_distances) {
    const MatDim_t ndim=finder.get_ndims();
    const NumNeighbors_t NN=check_k(nn);

    // Examining the query matrix and checking it against the subset indices.
    Rcpp::NumericMatrix Query(query);
    if (Query.nrow()!=ndim) {
        throw std::runtime_error("'query' and 'X' have different dimensionality");
    }
    const MatDim_t nobs=Query.ncol();

    // Getting the output mode.
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
    // Don't use qIt in range, as it fails for zero-dimension matrices.
    auto qIt=Query.begin();
    for (MatDim_t i=0; i<nobs; ++i, qIt+=ndim) {
        finder.find_nearest_neighbors(qIt, NN, store_neighbors, store_distances); 
        const auto& distances=finder.get_distances();
        const auto& neighbors=finder.get_neighbors();

        if (store_distances) {
            std::copy(distances.begin(), distances.end(), odIt);
            odIt+=NN;
        }
        if (store_neighbors) {
            std::copy(neighbors.begin(), neighbors.end(), oiIt);
            for (NumNeighbors_t i=0; i<NN; ++i, ++oiIt) {
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
