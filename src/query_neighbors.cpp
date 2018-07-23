#include "objects.h"
#include "utils.h"

SEXP query_neighbors(SEXP to_check, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP dist_thresh, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    searcher n_finder(X, clust_centers, clust_info);
    const double threshold=check_distance(dist_thresh);
    const size_t ndim=n_finder.get_ndims();

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
    for (auto h : points) {
        n_finder.find_neighbors(Query.begin() + ndim * h, threshold, store_neighbors, store_distances); 

        if (store_neighbors) {
            const std::deque<size_t>& neighbors=n_finder.get_neighbors();
            Rcpp::IntegerVector output(neighbors.begin(), neighbors.end());
            for (auto& o : output) { ++o; } // getting back to 1-based indexing.
            out_idx[ix]=output;
        }

        if (store_distances) {
            const std::deque<double>& distances=n_finder.get_distances();
            out_dist[ix]=Rcpp::NumericVector(distances.begin(), distances.end());
        }
        ++ix;
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
