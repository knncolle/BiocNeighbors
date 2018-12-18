#include "init.h"
#include "vptree.h"
#include "distances.h"

SEXP build_vptree (SEXP mat) {
    BEGIN_RCPP
    Rcpp::NumericMatrix Mat(mat);
    VpTree<BNEuclidean> vp(Mat);
    return vp.save();
    END_RCPP
}
