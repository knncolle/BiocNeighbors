#include "exhaustive.h"
#include "distances.h"
#include "utils.h"

#include <stdexcept>
#include <algorithm>

#define USE_UPPER 0

/****************** Constructor *********************/

template<class Distance>
Exhaustive<Distance>::Exhaustive(Rcpp::NumericMatrix ex, bool warn_ties) : 
    exprs(ex), nearest(warn_ties)
{
    return;
}

/****************** Visible methods *********************/

template<class Distance>
MatDim_t Exhaustive<Distance>::get_nobs() const { return exprs.ncol(); }

template<class Distance>
MatDim_t Exhaustive<Distance>::get_ndims() const { return exprs.nrow(); }

template<class Distance>
std::deque<CellIndex_t>& Exhaustive<Distance>::get_neighbors () { return neighbors; }

template<class Distance>
std::deque<double>& Exhaustive<Distance>::get_distances () { return distances; }

template<class Distance>
void Exhaustive<Distance>::find_neighbors (CellIndex_t cell, double threshold, const bool index, const bool dist) {
    if (cell >= static_cast<CellIndex_t>(exprs.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=exprs.column(cell);
    search_all(curcol.begin(), threshold, index, dist);
    return;
}

template<class Distance>
void Exhaustive<Distance>::find_neighbors (const double* current, double threshold, const bool index, const bool dist) {
    search_all(current, threshold, index, dist);
    return;
}

template<class Distance>
void Exhaustive<Distance>::find_nearest_neighbors (CellIndex_t cell, NumNeighbors_t nn, const bool index, const bool dist) {
    if (cell >= static_cast<CellIndex_t>(exprs.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=exprs.column(cell);
    nearest.setup(nn, cell);
    search_nn(curcol.begin(), nearest);
    nearest.report<Distance>(neighbors, distances, index, dist, true);
    return;
}

template<class Distance>
void Exhaustive<Distance>::find_nearest_neighbors (const double* current, NumNeighbors_t nn, const bool index, const bool dist) {
    nearest.setup(nn);
    search_nn(current, nearest);
    nearest.report<Distance>(neighbors, distances, index, dist, true);
    return;
}

template<class Distance>
void Exhaustive<Distance>::search_all (const double* current, double threshold, const bool index, const bool dist) {
    neighbors.clear();
    distances.clear();
    const MatDim_t ndims=exprs.nrow();
    const MatDim_t nobs=exprs.ncol();
    const double threshold_raw=Distance::unnormalize(threshold);

    const double* other_cell=exprs.begin();
    for (CellIndex_t celldex=0; celldex<nobs; ++celldex, other_cell+=ndims) {
	const double dist2cell_raw=Distance::raw_distance(current, other_cell, ndims);
        if (dist2cell_raw <= threshold_raw) {
	    if (index) {
		neighbors.push_back(celldex);
	    }
	    if (dist) {
		distances.push_back(Distance::normalize(dist2cell_raw));
	    }
        }
    }
    return;
}

template<class Distance>
void Exhaustive<Distance>::search_nn(const double* current, neighbor_queue& nearest) { 
    // final argument is not strictly necessary but makes dependencies more obvious.

    const MatDim_t ndims=exprs.nrow();
    const MatDim_t nobs=exprs.ncol();
    const double* other_cell=exprs.begin(); 

    // Loop through all cells.
    for (CellIndex_t celldex=0; celldex<nobs; ++celldex, other_cell+=ndims) {
	const double dist2cell_raw=Distance::raw_distance(current, other_cell, ndims);
	nearest.add(celldex, dist2cell_raw);
    }
    return;
}

/****************** Template realizations *********************/

template class Exhaustive<BNManhattan>;
template class Exhaustive<BNEuclidean>;
