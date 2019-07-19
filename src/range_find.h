#ifndef RANGE_NEIGHBORS_H
#define RANGE_NEIGHBORS_H
#include "utils.h"

template<class Searcher>
SEXP range_neighbors(Searcher& finder, Rcpp::IntegerVector to_check, Rcpp::NumericVector dist_thresh, bool store_neighbors, bool store_distances) {
    // Figuring out which indices we're using.
    const Rcpp::IntegerVector points=check_indices(to_check, finder.get_nobs());
    const VecSize_t nobs=points.size();
    const Rcpp::NumericVector thresholds=check_distances(dist_thresh, nobs);

    // Getting the output mode.
    Rcpp::List out_dist;
    if (store_distances) {
        out_dist=Rcpp::List(nobs);
    }

    Rcpp::List out_idx;
    if (store_neighbors) {
        out_idx=Rcpp::List(nobs);
    }

    // Iterating across cells, finding NNs and storing distances or neighbors.
    for (VecSize_t ix=0; ix<nobs; ++ix) {
        finder.find_neighbors(points[ix], thresholds[ix], store_neighbors, store_distances);

        if (store_neighbors) {
            const auto& neighbors=finder.get_neighbors();
            Rcpp::IntegerVector output(neighbors.begin(), neighbors.end());
            for (auto& o : output) { ++o; } // getting back to 1-based indexing.
            out_idx[ix]=output;
        }

        if (store_distances) {
            const auto& distances=finder.get_distances();
            out_dist[ix]=Rcpp::NumericVector(distances.begin(), distances.end());
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
