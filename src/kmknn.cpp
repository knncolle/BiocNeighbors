#include "generics.h"
#include "l2norm.h"
#include "knncolle/knncolle.hpp"

//[[Rcpp::export(rng=false)]]
SEXP build_kmknn(Rcpp::NumericMatrix data, std::string distance) {
    if (distance == "Manhattan") {
        knncolle::KmknnBuilder<knncolle::ManhattanDistance, WrappedMatrix, double> builder;
        return generic_build(builder, data);

    } else if (distance == "Euclidean") {
        knncolle::KmknnBuilder<knncolle::EuclideanDistance, WrappedMatrix, double> builder;
        return generic_build(builder, data);

    } else if (distance == "Cosine") {
        knncolle::KmknnBuilder<knncolle::EuclideanDistance, WrappedMatrix, double> builder;
        auto out = generic_build(builder, l2norm(data));
        out->cosine = true;
        return out;

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
