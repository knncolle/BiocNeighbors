#include "init.h"
#include "hnsw.h"

template<class Space>
SEXP build_hnsw_internal (SEXP mat, SEXP nlinks, SEXP ef_construct, SEXP fname) {
    Rcpp::NumericMatrix Mat(mat);
    const int ndim=Mat.nrow();
    const int ncells=Mat.ncol();

    Space space(ndim);
    typename Hnsw<Space>::_index obj(&space, ncells, 
        check_integer_scalar(nlinks, "number of bidirectional links"),
        check_integer_scalar(ef_construct, "size of dynamic list")
    );

    std::vector<typename Hnsw<Space>::Data_t> tmp(ndim);
    auto mIt=Mat.begin();
    for (int i=0; i<ncells; ++i, mIt+=ndim) {
        std::copy(mIt, mIt+ndim, tmp.begin());
        obj.addPoint(tmp.data(), i);
    }

    auto Fname=check_string(fname, "index file name");
    obj.saveIndex(Fname);

    return R_NilValue;
}

SEXP build_hnsw(SEXP mat, SEXP nlinks, SEXP ef_construct, SEXP fname, SEXP dtype) {
    BEGIN_RCPP
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        return build_hnsw_internal<L1Space>(mat, nlinks, ef_construct, fname);
    } else {
        return build_hnsw_internal<hnswlib::L2Space>(mat, nlinks, ef_construct, fname);
    }
    END_RCPP
}

