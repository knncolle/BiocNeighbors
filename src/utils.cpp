#include "utils.h"

Rcpp::IntegerVector check_indices(Rcpp::IntegerVector incoming, int total_obs) {
    for (auto h : incoming) {
        if (h==NA_INTEGER || h < 0 || h >= total_obs) {
            throw std::runtime_error("job indices out of range");
        }
    }
    return incoming;
}

NumNeighbors_t check_k(int NN) {
    if (NN<1){
        throw std::runtime_error("'k' must be positive");
    }
    return NN;
}

Rcpp::NumericVector check_distances(Rcpp::NumericVector incoming, VecSize_t N) {
    if (incoming.size()!=N) {
        throw std::runtime_error("length of distance vector should be equal to number of points");
    }
    for (auto threshold : incoming) {
        if (threshold <= 0) {
            throw std::runtime_error("threshold should be positive");
        }
    }
    return incoming;
}
