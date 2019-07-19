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
    Rcpp::List out_dist;
    if (store_distances) {
        out_dist=Rcpp::List(nobs);
    }

    Rcpp::List out_idx;
    if (store_neighbors) {
        out_idx=Rcpp::List(nobs);
    }
        
    // Iterating across cells, finding NNs and storing distances or neighbors.
    size_t ix=0;
    for (auto qIt=Query.begin(); qIt!=Query.end(); qIt+=ndim, ++ix) {
        finder.find_neighbors(qIt, thresholds[ix], store_neighbors, store_distances); 

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