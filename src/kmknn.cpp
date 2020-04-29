#include "kmknn.h"
#include "distances.h"
#include "utils.h"

#include <stdexcept>
#include <algorithm>

#define USE_UPPER 0

/****************** Constructor *********************/

template<class Distance>
Kmknn<Distance>::Kmknn(Rcpp::NumericMatrix ex, Rcpp::NumericMatrix cen, Rcpp::List info, bool warn_ties) : 
    exprs(ex), nearest(warn_ties), centers(cen)
{
    const MatDim_t ncenters=centers.ncol();
    clust_start.reserve(ncenters);
    clust_dist.reserve(ncenters);
    clust_nobs.reserve(ncenters);

    for (MatDim_t i=0; i<ncenters; ++i) {
        Rcpp::List current(info[i]);
        if (current.size()!=2) {
            throw std::runtime_error("cluster information list elements must be of length 2");
        }

        Rcpp::IntegerVector starting=current[0];
        if (starting.size()!=1) {
            throw std::runtime_error("starting ID must be an integer scalar");
        }
        clust_start.push_back(starting[0]);

        const Rcpp::NumericVector distances(current[1]);
        clust_dist.push_back(distances);
        clust_nobs.push_back(distances.size());
    }
    return;
}

/****************** Visible methods *********************/

template<class Distance>
MatDim_t Kmknn<Distance>::get_nobs() const { return exprs.ncol(); }

template<class Distance>
MatDim_t Kmknn<Distance>::get_ndims() const { return exprs.nrow(); }

template<class Distance>
std::deque<CellIndex_t>& Kmknn<Distance>::get_neighbors () { return neighbors; }

template<class Distance>
std::deque<double>& Kmknn<Distance>::get_distances () { return distances; }

template<class Distance>
void Kmknn<Distance>::find_neighbors (CellIndex_t cell, double threshold, const bool index, const bool dist) {
    if (cell >= static_cast<CellIndex_t>(exprs.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=exprs.column(cell);
    search_all(curcol.begin(), threshold, index, dist);
    return;
}

template<class Distance>
void Kmknn<Distance>::find_neighbors (const double* current, double threshold, const bool index, const bool dist) {
    search_all(current, threshold, index, dist);
    return;
}

template<class Distance>
void Kmknn<Distance>::find_nearest_neighbors (CellIndex_t cell, NumNeighbors_t nn, const bool index, const bool dist) {
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
void Kmknn<Distance>::find_nearest_neighbors (const double* current, NumNeighbors_t nn, const bool index, const bool dist) {
    nearest.setup(nn);
    search_nn(current, nearest);
    nearest.report<Distance>(neighbors, distances, index, dist, true);
    return;
}

/****************** Convex search methods *********************/

template<class Distance>
void Kmknn<Distance>::search_all (const double* current, double threshold, const bool index, const bool dist) {
    neighbors.clear();
    distances.clear();
    const MatDim_t ndims=exprs.nrow();
    const MatDim_t ncenters=centers.ncol();
    const double* center_ptr=centers.begin();
    const double threshold_raw=Distance::unnormalize(threshold);

    // Computing the distance to each center, and deciding whether to proceed for each cluster.
    for (MatDim_t center=0; center<ncenters; ++center, center_ptr+=ndims) {
        const CellIndex_t cur_nobs=clust_nobs[center];
        if (!cur_nobs) { continue; }

        const double dist2center=Distance::distance(current, center_ptr, ndims);
        auto dIt=clust_dist[center].begin();
        const double maxdist=*(dIt + cur_nobs - 1);
        if (threshold + maxdist < dist2center) { continue; }

        /* Cells within this cluster are potentially countable; jumping to the first countable cell,
         * according to the triangle inequality. We could also define the last countable cell by the
         * reverse triangle inequality, but the clusters are too compact for that to come into play.
         */
        const double lower_bd=dist2center-threshold;
        const CellIndex_t firstcell=std::lower_bound(dIt, dIt + cur_nobs, lower_bd) - dIt;
#if USE_UPPER
        const double upper_bd=dist2center + threshold;
#endif

        const CellIndex_t cur_start=clust_start[center];
        const double* other_cell=exprs.begin() + ndims * (cur_start + firstcell);
        for (CellIndex_t celldex=firstcell; celldex<cur_nobs; ++celldex, other_cell+=ndims) {
#if USE_UPPER
            if (*(dIt + celldex) > upper_bd) {
                break;
            }
#endif

            const double dist2cell_raw=Distance::raw_distance(current, other_cell, ndims);
            if (dist2cell_raw <= threshold_raw) {
                if (index) {
                    neighbors.push_back(cur_start + celldex);
                }
                if (dist) {
                    distances.push_back(Distance::normalize(dist2cell_raw));
                }
            }
        }
    }
    return;
}

template<class Distance>
void Kmknn<Distance>::search_nn(const double* current, neighbor_queue& nearest) { 
    // final argument is not strictly necessary but makes dependencies more obvious.

    const MatDim_t ndims=exprs.nrow();
    const MatDim_t ncenters=centers.ncol();
    const double* center_ptr=centers.begin();
    double threshold_raw = R_PosInf;

    /* Computing distances to all centers and sorting them.
     * The aim is to go through the nearest centers first, to get the shortest 'threshold' possible.
     */
    std::deque<std::pair<double, MatDim_t> > center_order(ncenters);
    for (MatDim_t center=0; center<ncenters; ++center, center_ptr+=ndims) {
        center_order[center].first=Distance::distance(current, center_ptr, ndims);
        center_order[center].second=center;
    }
    std::sort(center_order.begin(), center_order.end());

    // Computing the distance to each center, and deciding whether to proceed for each cluster.
    for (const auto& curcent : center_order) {
        const MatDim_t center=curcent.second;
        const double dist2center=curcent.first;

        const auto cur_nobs=clust_nobs[center];
        if (!cur_nobs) { continue; }
        const double* dIt=clust_dist[center].begin();
        const double maxdist=*(dIt + cur_nobs-1);

        CellIndex_t firstcell=0;
#if USE_UPPER
        double upper_bd=R_PosInf;
#endif
        if (R_FINITE(threshold_raw)) {
            const double threshold=Distance::normalize(threshold_raw);

            /* The conditional expression below exploits the triangle inequality; it is equivalent to asking whether:
             *     threshold + maxdist < dist2center
             * All points (if any) within this cluster with distances above 'lower_bd' are potentially countable.
             */
            const double lower_bd=dist2center - threshold;
            if (maxdist < lower_bd) {
                continue;
            }
            firstcell=std::lower_bound(dIt, dIt+cur_nobs, lower_bd)-dIt;
#if USE_UPPER
            /* This exploits the reverse triangle inequality, to ignore points where:
             *     threshold + dist2center < point-to-center distance
             */
            upper_bd = threshold + dist2center;
#endif
        }

        const CellIndex_t cur_start=clust_start[center];
        const double* other_cell=exprs.begin() + ndims * (cur_start + firstcell);
        for (CellIndex_t celldex=firstcell; celldex<cur_nobs; ++celldex, other_cell+=ndims) {
#if USE_UPPER
            if (*(dIt + celldex) > upper_bd) {
                break;
            }
#endif

            const double dist2cell_raw=Distance::raw_distance(current, other_cell, ndims);
            nearest.add(cur_start + celldex, dist2cell_raw);
            if (nearest.is_full()) {
                threshold_raw=nearest.limit(); // Shrinking the threshold, if an earlier NN has been found.
#if USE_UPPER
                upper_bd=std::sqrt(threshold_raw) + dist2center; 
#endif
            }
        }
    }
    return;
}

/****************** Template realizations *********************/

template class Kmknn<BNManhattan>;
template class Kmknn<BNEuclidean>;
