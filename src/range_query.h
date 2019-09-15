#ifndef RANGE_QUERY_EXACT_H
#define RANGE_QUERY_EXACT_H
#include "utils.h"

template<class Searcher>
Rcpp::RObject range_query_exact(Searcher& finder, Rcpp::NumericMatrix query, 
    Rcpp::NumericVector dist_thresh, bool store_neighbors, bool store_distances) 
{
    const MatDim_t ndim=finder.get_ndims();

    // Examining the query matrix and checking it against the subset indices.
    Rcpp::NumericMatrix Query(query);
    if (Query.nrow()!=ndim) {
        throw std::runtime_error("'query' and 'X' have different dimensionality");
    }
    const VecSize_t nobs=Query.ncol();
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

    // Iterating across cells, finding NNs and storing distances or neighbors.
    // Don't use for range ver qIt, as this fails for zero-dimension inputs.
    auto qIt=Query.begin(); 
    for (VecSize_t ix=0; ix<nobs; ++ix, qIt+=ndim) {
        finder.find_neighbors(qIt, thresholds[ix], store_neighbors || get_counts, store_distances); 

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
