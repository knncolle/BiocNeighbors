#ifndef HNSW_H
#define HNSW_H
#include "utils.h"
#include <deque>
#include <vector>

// define R's REprintf as the 'local' error print method for Annoy
#define __ERROR_PRINTER_OVERRIDE__  REprintf

// Avoid discrepancies with AVX.
#define HNSW_PORTABLE 1
#include "hnswlib.h"

class Hnsw {
public:
    Hnsw(SEXP, SEXP);

    void find_nearest_neighbors(CellIndex_t, NumNeighbors_t, const bool, const bool);
    void find_nearest_neighbors(const double*, NumNeighbors_t, const bool, const bool);

    MatDim_t get_nobs() const;
    MatDim_t get_ndims() const;

    typedef int32_t Index_t;
    typedef float Data_t;
    typedef hnswlib::HierarchicalNSW<Data_t> _index;

    const std::deque<CellIndex_t>& get_neighbors () const;
    const std::deque<double>& get_distances () const;
private:
    Rcpp::NumericMatrix data;
    hnswlib::L2Space space;   
    _index obj;

    std::deque<CellIndex_t> kept_idx;
    std::deque<double> kept_dist;
    std::vector<Data_t> holding;
};

#endif
