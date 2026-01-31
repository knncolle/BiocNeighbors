#include "Rcpp.h"
#include "BiocNeighbors.h"

#include <string>
#include <memory>
#include <stdexcept>

#include "knncolle_kmknn/knncolle_kmknn.hpp"

//[[Rcpp::export(rng=false)]]
SEXP kmknn_builder(std::string distance) {
    knncolle_kmknn::KmknnOptions<int, double, double> opt;

    if (distance == "Manhattan") {
        auto distfun = std::make_shared<knncolle::ManhattanDistance<double, double> >();
        auto copy = distfun;
        return BiocNeighbors::BuilderPointer(new knncolle_kmknn::KmknnBuilder<int, double, double>(std::move(distfun), std::move(copy), opt));

    } else if (distance == "Euclidean") {
        auto distfun = std::make_shared<knncolle::EuclideanDistance<double, double> >();
        auto copy = distfun;
        return BiocNeighbors::BuilderPointer(new knncolle_kmknn::KmknnBuilder<int, double, double>(std::move(distfun), std::move(copy), opt));

    } else if (distance == "Cosine") {
        auto distfun = std::make_shared<knncolle::EuclideanDistance<double, double> >();
        auto copy = distfun;
        return BiocNeighbors::BuilderPointer(
            new knncolle::L2NormalizedBuilder<int, double, double, double>(
                std::make_shared<knncolle_kmknn::KmknnBuilder<int, double, double> >(std::move(distfun), std::move(copy), opt)
            )
        );

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
