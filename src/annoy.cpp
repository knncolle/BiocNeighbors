#include "Rcpp.h"
#include "BiocNeighbors.h"

#include <string>
#include <memory>
#include <stdexcept>

#include "annoy.h"

//[[Rcpp::export(rng=false)]]
SEXP annoy_builder(int num_trees, double search_mult, std::string distance) {
    knncolle_annoy::AnnoyOptions opt;
    opt.num_trees = num_trees;
    opt.search_mult = search_mult;

    if (distance == "Manhattan") {
        return BiocNeighbors::BuilderPointer(new knncolle_annoy::AnnoyBuilder<int, double, double, Annoy::Manhattan>(opt));

    } else if (distance == "Euclidean") {
        return BiocNeighbors::BuilderPointer(new knncolle_annoy::AnnoyBuilder<int, double, double, Annoy::Euclidean>(opt));

    } else if (distance == "Cosine") {
        return BiocNeighbors::BuilderPointer(
            new knncolle::L2NormalizedBuilder<int, double, double, double>(
                std::make_shared<knncolle_annoy::AnnoyBuilder<int, double, double, Annoy::Euclidean> >(opt)
            )
        );

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
