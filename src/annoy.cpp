#include "Rcpp.h"
#include "BiocNeighbors.h"

// Turn off manual vectorization always, to avoid small inconsistencies in
// distance calculations across otherwise-compliant machines. 
#define NO_MANUAL_VECTORIZATION 1

// Copied from RcppAnnoy's RcppAnnoy.h:
#if defined(__MINGW32__)
#undef Realloc
#undef Free
#endif
#define __ERROR_PRINTER_OVERRIDE__  REprintf

#include "knncolle_annoy/knncolle_annoy.hpp"

//[[Rcpp::export(rng=false)]]
SEXP annoy_builder(int num_trees, double search_mult, std::string distance) {
    knncolle_annoy::AnnoyOptions opt;
    opt.num_trees = num_trees;
    opt.search_mult = search_mult;

    if (distance == "Manhattan") {
        return BiocNeighbors::BuilderPointer(new knncolle_annoy::AnnoyBuilder<Annoy::Manhattan, BiocNeighbors::SimpleMatrix, double>(opt), true);

    } else if (distance == "Euclidean") {
        return BiocNeighbors::BuilderPointer(new knncolle_annoy::AnnoyBuilder<Annoy::Euclidean, BiocNeighbors::SimpleMatrix, double>(opt), true);

    } else if (distance == "Cosine") {
        return BiocNeighbors::BuilderPointer(
            new knncolle::L2NormalizedBuilder<BiocNeighbors::SimpleMatrix, double>(
                new knncolle_annoy::AnnoyBuilder<
                    Annoy::Euclidean,
                    knncolle::L2NormalizedMatrix<BiocNeighbors::SimpleMatrix>,
                    double
                >(opt)
            ),
            true
        );

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
