#include "Rcpp.h"
#include "BiocNeighbors.h"
#include "knncolle/knncolle.hpp"

//[[Rcpp::export(rng=false)]]
SEXP vptree_builder(std::string distance) {
    if (distance == "Manhattan") {
        return BiocNeighbors::BuilderPointer(new knncolle::VptreeBuilder<knncolle::ManhattanDistance, BiocNeighbors::SimpleMatrix, double>);

    } else if (distance == "Euclidean") {
        return BiocNeighbors::BuilderPointer(new knncolle::VptreeBuilder<knncolle::EuclideanDistance, BiocNeighbors::SimpleMatrix, double>);

    } else if (distance == "Cosine") {
        return BiocNeighbors::BuilderPointer(
            new knncolle::L2NormalizedBuilder(
                new knncolle::VptreeBuilder<
                    knncolle::EuclideanDistance,
                    knncolle::L2NormalizedMatrix<BiocNeighbors::SimpleMatrix>,
                    double
                >
            )
        );

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
