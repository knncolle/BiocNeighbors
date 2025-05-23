#include "Rcpp.h"
#include "BiocNeighbors.h"
#include "knncolle_kmknn/knncolle_kmknn.hpp"

//[[Rcpp::export(rng=false)]]
SEXP kmknn_builder(std::string distance) {
    knncolle_kmknn::KmknnOptions<int, double, double> opt;

    if (distance == "Manhattan") {
        auto distfun = std::make_shared<knncolle_kmknn::ManhattanDistance<double, double> >();
        return BiocNeighbors::BuilderPointer(new knncolle_kmknn::KmknnBuilder<int, double, double>(std::move(distfun), opt));

    } else if (distance == "Euclidean") {
        auto distfun = std::make_shared<knncolle_kmknn::EuclideanDistance<double, double> >();
        return BiocNeighbors::BuilderPointer(new knncolle_kmknn::KmknnBuilder<int, double, double>(std::move(distfun), opt));

    } else if (distance == "Cosine") {
        auto distfun = std::make_shared<knncolle_kmknn::EuclideanDistance<double, double> >();
        return BiocNeighbors::BuilderPointer(
            new knncolle::L2NormalizedBuilder<int, double, double, double>(
                std::make_shared<knncolle_kmknn::KmknnBuilder<int, double, double> >(std::move(distfun), opt)
            )
        );

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
