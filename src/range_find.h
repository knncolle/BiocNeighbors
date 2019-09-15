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
    bool get_counts=!store_neighbors && !store_distances;

    Rcpp::List out_dist;
    if (store_distances) {
        out_dist=Rcpp::List(nobs);
    }

    Rcpp::List out_idx;
    if (store_neighbors) {
        out_idx=Rcpp::List(nobs);
    }

    Rcpp::IntegerVector out_counts;
    if (get_counts) {
        out_counts=Rcpp::IntegerVector(nobs);
    }

    // Iterating across cells, finding NNs and storing distances and/or neighbors,
    // or just the number of neighbors if both store_distances and store_neighbors are FALSE.
    for (VecSize_t ix=0; ix<nobs; ++ix) {
        finder.find_neighbors(points[ix], thresholds[ix], store_neighbors || get_counts, store_distances);

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

        if (get_counts) {
            const auto& neighbors=finder.get_neighbors();
            out_counts[ix]=neighbors.size();
        }
    }

    if (get_counts) {
        return out_counts;
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
