#include "init.h"
#include "hnsw.h"

SEXP build_hnsw (SEXP mat, SEXP nlinks, SEXP ef_construct, SEXP fname) {
    BEGIN_RCPP
    Rcpp::NumericMatrix Mat(mat);
    const int ndim=Mat.nrow();
    const int ncells=Mat.ncol();

    hnswlib::L2Space space(ndim);
    Hnsw::_index obj(&space, ncells, 
        check_integer_scalar(nlinks, "number of bidirectional links"),
        check_integer_scalar(ef_construct, "size of dynamic list")
    );

    std::vector<Hnsw::Data_t> tmp(ndim);
    auto mIt=Mat.begin();
    for (int i=0; i<ncells; ++i, mIt+=ndim) {
        std::copy(mIt, mIt+ndim, tmp.begin());
        obj.addPoint(tmp.data(), i);
    }

    auto Fname=check_string(fname, "index file name");
    obj.saveIndex(Fname);

    return R_NilValue;
    END_RCPP
}
