#include "generics.h"
#include "knncolle/knncolle.hpp"

//[[Rcpp::export(rng=false)]]
SEXP build_vptree(Rcpp::NumericMatrix data, std::string distance) {
    if (distance == "Manhattan") {
        knncolle::VptreeBuilder<knncolle::ManhattanDistance, WrappedMatrix, double> builder;
        return generic_build(builder, data);
    } else if (distance == "Euclidean") {
        knncolle::VptreeBuilder<knncolle::EuclideanDistance, WrappedMatrix, double> builder;
        return generic_build(builder, data);
    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
