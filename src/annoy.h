#ifndef ANNOY_H
#define ANNOY_H
#include "utils.h"

// Stolen from RcppAnnoy's annoy.cpp, to get it to compile on Win32.
#if defined(__MINGW32__)
#undef Realloc
#undef Free
#endif

// define R's REprintf as the 'local' error print method for Annoy
#define __ERROR_PRINTER_OVERRIDE__  REprintf

// turn off AVX always, to avoid small inconsistencies in distance calculations.
#define NO_MANUAL_VECTORIZATION 1

#include "annoylib.h"
#include "kissrandom.h"

class Annoy {
public:
    Annoy(SEXP, SEXP);

    void find_nearest_neighbors(CellIndex_t, NumNeighbors_t, const bool, const bool);
    void find_nearest_neighbors(const double*, NumNeighbors_t, const bool, const bool);

    MatDim_t get_nobs() const;
    MatDim_t get_ndims() const;

    std::deque<CellIndex_t>& get_neighbors ();
    std::deque<double>& get_distances ();

    typedef int32_t Index_t;
    typedef float Data_t;
    typedef AnnoyIndex<Index_t, Data_t, Euclidean, Kiss64Random> _index;
private:
    MatDim_t NDims;
    _index obj;

    std::vector<Index_t> kept_idx;
    std::vector<Data_t> kept_dist, holding;

    std::deque<CellIndex_t> neighbors;
    std::deque<double> distances;
};

#endif
