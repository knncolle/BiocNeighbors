#include "generics.h"
#include "l2norm.h"

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
SEXP build_annoy(Rcpp::NumericMatrix data, int num_trees, double search_mult, std::string distance) {
    knncolle_annoy::AnnoyOptions opt;
    opt.num_trees = num_trees;
    opt.search_mult = search_mult;

    if (distance == "Manhattan") {
        knncolle_annoy::AnnoyBuilder<Annoy::Manhattan, WrappedMatrix, double> builder(opt);
        return generic_build(builder, data);

    } else if (distance == "Euclidean") {
        knncolle_annoy::AnnoyBuilder<Annoy::Euclidean, WrappedMatrix, double> builder(opt);
        return generic_build(builder, data);

    } else if (distance == "Cosine") {
        knncolle_annoy::AnnoyBuilder<Annoy::Euclidean, WrappedMatrix, double> builder(opt);
        auto out = generic_build(builder, l2norm(data));
        out->cosine = true;
        return out;

    } else {
        throw std::runtime_error("unknown distance type '" + distance + "'");
        return R_NilValue;
    }
}
