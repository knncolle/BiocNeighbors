#include "Rcpp.h"
#include "BiocNeighbors.h"

#include <string>
#include <memory>
#include <stdexcept>

#include "knncolle/knncolle.hpp"

//[[Rcpp::export(rng=false)]]
SEXP exhaustive_builder(std::string distance) {
    if (distance == "Manhattan") {
        auto distfun = std::make_shared<knncolle::ManhattanDistance<double, double> >();
        return BiocNeighbors::BuilderPointer(new knncolle::BruteforceBuilder<int, double, double>(std::move(distfun)));

    } else if (distance == "Euclidean") {
        auto distfun = std::make_shared<knncolle::EuclideanDistance<double, double> >();
        return BiocNeighbors::BuilderPointer(new knncolle::BruteforceBuilder<int, double, double>(std::move(distfun)));

    } else if (distance == "Cosine") {
        auto distfun = std::make_shared<knncolle::EuclideanDistance<double, double> >();
        return BiocNeighbors::BuilderPointer(
            new knncolle::L2NormalizedBuilder<int, double, double, double>(
                std::make_shared<knncolle::BruteforceBuilder<int, double, double> >(std::move(distfun))
            )
        );

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
