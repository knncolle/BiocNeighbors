#include "generics.h"
#include "l2norm.h"

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
SEXP build_hnsw(Rcpp::NumericMatrix data, int nlinks, int ef_construct, int ef_search, std::string distance) {
    knncolle_hnsw::HnswOptions<int, float> opt;
    opt.num_links = nlinks;
    opt.ef_construction = ef_construct;
    opt.ef_search = ef_search;

    if (distance == "Manhattan") {
        opt.distance_options.create = [&](int dim) -> hnswlib::SpaceInterface<float>* {
            return new knncolle_hnsw::ManhattanDistance<float>(dim);
        };
        knncolle_hnsw::HnswBuilder<WrappedMatrix, double> builder(opt);
        return generic_build(builder, data);

    } else if (distance == "Euclidean") {
        knncolle_hnsw::HnswBuilder<WrappedMatrix, double> builder(opt);
        return generic_build(builder, data);

    } else if (distance == "Euclidean") {
        knncolle_hnsw::HnswBuilder<WrappedMatrix, double> builder(opt);
        auto out = generic_build(builder, l2norm(data));
        out->cosine = true;
        return out;

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
