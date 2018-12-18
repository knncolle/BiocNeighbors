#include "init.h"
#include "vptree.h"
#include "distances.h"

SEXP build_vptree (SEXP mat, SEXP dtype) {
    BEGIN_RCPP
    Rcpp::NumericMatrix Mat(mat);
    auto Mode=check_string(dtype, "distance type");
    if (Mode=="Manhattan") {
        VpTree<BNManhattan> vp(Mat);
        return vp.save();
     } else {
        VpTree<BNEuclidean> vp(Mat);
        return vp.save();
    }
    END_RCPP
}
