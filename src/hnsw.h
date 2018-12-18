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

template<class Space>
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
    Space space;
    _index obj;

    static double normalize(double);
    std::deque<CellIndex_t> kept_idx;
    std::deque<double> kept_dist;
    std::vector<Data_t> holding;
};

class L1Space : public hnswlib::SpaceInterface<float> {
    size_t data_size_;
    size_t dim_;
public:
    L1Space(size_t dim);
    size_t get_data_size();
    hnswlib::DISTFUNC<float> get_dist_func();
    void *get_dist_func_param();

    static float L1(const void*, const void*, const void*);
};

#endif
