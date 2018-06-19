#include "objects.h"
#include "utils.h"

SEXP find_knn(SEXP start, SEXP stop, SEXP X, SEXP clust_centers, SEXP clust_info, SEXP nn, SEXP query, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP

    const size_t NN=check_integer_scalar(nn, "'k'");
    if (NN<1) { 
        throw std::runtime_error("'k' must be positive");
    }
    auto searcher=generate_holder(X, clust_centers, clust_info);
    const size_t ndim=searcher->get_ndims();

    // Just iterating across itself, if there is no query matrix.
    const bool self_cycle=(query==R_NilValue);
    Rcpp::NumericMatrix Query;
    if (self_cycle) { 
        Query=Rcpp::NumericMatrix(X);
    } else {
        Query=Rcpp::NumericMatrix(query);
        if (Query.nrow()!=ndim) {
            throw std::runtime_error("'query' and 'X' have different dimensionality");
        }
    }

    // Figuring out what we're looking at.
    const size_t total_obs=Query.ncol();
    const int Start=check_integer_scalar(start, "job start index");
    const int Stop=check_integer_scalar(stop, "job end index");
    if (Start < 0 || Stop > total_obs) {
        throw std::runtime_error("job indices out of range");
    }
    const size_t nobs=Stop-Start;

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
    auto qIt=Query.begin();
    for (int h=Start; h<Stop; ++h) { 
        if (self_cycle) { 
            searcher->find_nearest_neighbors(h, NN, store_distances); 
        } else {
            searcher->find_nearest_neighbors(qIt, NN, store_distances); 
            qIt+=ndim;
        }
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
    if (store_distances) {
        output[1]=out_dist;
    }
    if (store_neighbors) {
        output[0]=out_idx;
    }   
    return output;
    END_RCPP
}


