#include "Rcpp.h"
#include "BiocNeighbors.h"

// define R's REprintf as the 'local' error print method. 
#define __ERROR_PRINTER_OVERRIDE__  REprintf

// Turn off manual vectorization always, to avoid small inconsistencies in
// distance calculations across otherwise-compliant machines. 
#define NO_MANUAL_VECTORIZATION

// Avoid CRAN problems with std::cerr.
#define HNSWLIB_ERR_OVERRIDE Rcpp::Rcerr

// Avoid compilation problems on Windows.
#define STRICT_R_HEADERS

#include "knncolle_hnsw/knncolle_hnsw.hpp"

//[[Rcpp::export(rng=false)]]
SEXP hnsw_builder(int nlinks, int ef_construct, int ef_search, std::string distance) {
    knncolle_hnsw::HnswOptions opt;
    opt.num_links = nlinks;
    opt.ef_construction = ef_construct;
    opt.ef_search = ef_search;

    if (distance == "Manhattan") {
        return BiocNeighbors::BuilderPointer(static_cast<BiocNeighbors::Builder*>(new knncolle_hnsw::HnswBuilder<int, double, double>(knncolle_hnsw::makeManhattanDistanceConfig(), opt)));

    } else if (distance == "Euclidean") {
        return BiocNeighbors::BuilderPointer(new knncolle_hnsw::HnswBuilder<int, double, double>(knncolle_hnsw::makeEuclideanDistanceConfig(), opt));

    } else if (distance == "Cosine") {
        return BiocNeighbors::BuilderPointer(
            new knncolle::L2NormalizedBuilder<int, double, double, double>(
                std::make_shared<knncolle_hnsw::HnswBuilder<int, double, double> >(knncolle_hnsw::makeEuclideanDistanceConfig(), opt)
            )
        );

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
