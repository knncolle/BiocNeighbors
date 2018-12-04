#include "kmknn.h"
#include "utils.h"
#include "queue2deque.h"

#define USE_UPPER 0

/****************** Constructor *********************/

Kmknn::Kmknn(SEXP ex, SEXP cen, SEXP info) : exprs(ex), centers(cen), diagnose_ties(true) {
    const size_t& ncenters=centers.ncol();

    Rcpp::List Info(info);
    for (size_t i=0; i<ncenters; ++i) {
        Rcpp::List current(Info[i]);
        if (current.size()!=2) {
            throw std::runtime_error("cluster information list elements must be of length 2");
        }

        clust_start.push_back(check_integer_scalar(current[0], "starting ID"));

        const Rcpp::NumericVector distances(current[1]);
        clust_dist.push_back(distances);
        clust_nobs.push_back(distances.size());
    }
    return;
}

/****************** Visible methods *********************/

size_t Kmknn::get_nobs() const { return exprs.ncol(); }

size_t Kmknn::get_ndims() const { return exprs.nrow(); }

std::deque<size_t>& Kmknn::get_neighbors () { return neighbors; }

std::deque<double>& Kmknn::get_distances () { return distances; }

void Kmknn::find_neighbors (size_t cell, double threshold, const bool index, const bool dist) {
    if (cell >= size_t(exprs.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=exprs.column(cell);
    search_all(curcol.begin(), threshold, index, dist);
    return;
}

void Kmknn::find_neighbors (const double* current, double threshold, const bool index, const bool dist) {
    search_all(current, threshold, index, dist);
    return;
}

void Kmknn::find_nearest_neighbors (size_t cell, size_t nn, const bool index, const bool dist) {
    if (cell >= size_t(exprs.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=exprs.column(cell);
    search_nn(curcol.begin(), nn + 1 + diagnose_ties);
    
    queue2deque(nearest, neighbors, distances, index, dist || diagnose_ties, true, cell);
    for (auto& d : distances) { d=std::sqrt(d); } // distances in 'nearest' are squared.

    check_ties(diagnose_ties, neighbors, distances, nn);
    return;
}

void Kmknn::find_nearest_neighbors (const double* current, size_t nn, const bool index, const bool dist) {
    search_nn(current, nn + diagnose_ties);
    
    queue2deque(nearest, neighbors, distances, index, dist || diagnose_ties, false, size_t(0));
    for (auto& d : distances) { d=std::sqrt(d); } // distances in 'nearest' are squared.

    check_ties(diagnose_ties, neighbors, distances, nn);
    return;
}

double Kmknn::compute_sqdist(const double* x, const double* y) const {
    double out=0;
    const size_t NR=exprs.nrow();
    for (size_t m=0; m<NR; ++m) {
        const double tmp=x[m]-y[m];
        out+=tmp*tmp;
    }
    return out;
}

/****************** Convex search methods *********************/

void Kmknn::search_all (const double* current, double threshold, const bool index, const bool dist) {
    neighbors.clear();
    distances.clear();
    const size_t& ndims=exprs.nrow();
    const size_t& ncenters=centers.ncol();
    const double* center_ptr=centers.begin();
    const double threshold2=threshold*threshold; // squaring.

    // Computing the distance to each center, and deciding whether to proceed for each cluster.
    for (size_t center=0; center<ncenters; ++center, center_ptr+=ndims) {
        const int& cur_nobs=clust_nobs[center];
        if (!cur_nobs) { continue; }

        const double dist2center=std::sqrt(compute_sqdist(current, center_ptr));
        auto dIt=clust_dist[center].begin();
        const double& maxdist=*(dIt + cur_nobs - 1);
        if (threshold + maxdist < dist2center) { continue; }

        /* Cells within this cluster are potentially countable; jumping to the first countable cell,
         * according to the triangle inequality. We could also define the last countable cell by the
         * reverse triangle inequality, but the clusters are too compact for that to come into play.
         */
        const double lower_bd=dist2center-threshold;
        const int firstcell=std::lower_bound(dIt, dIt + cur_nobs, lower_bd) - dIt;
#if USE_UPPER
        const double upper_bd=dist2center + threshold;
#endif

        const int& cur_start=clust_start[center];
        const double* other_cell=exprs.begin() + ndims * (cur_start + firstcell);
        for (int celldex=firstcell; celldex<cur_nobs; ++celldex, other_cell+=ndims) {
#if USE_UPPER
            if (*(dIt + celldex) > upper_bd) {
                break;
            }
#endif

            const double dist2cell2=compute_sqdist(current, other_cell);
            if (dist2cell2 <= threshold2) {
                if (index) {
                    neighbors.push_back(cur_start + celldex);
                }
                if (dist) {
                    distances.push_back(std::sqrt(dist2cell2));
                }
            }
        }
    }
    return;
}

void Kmknn::search_nn(const double* current, size_t nn) {
    const size_t& ndims=exprs.nrow();
    const size_t& ncenters=centers.ncol();
    const double* center_ptr=centers.begin();
    double threshold2 = R_PosInf;

    /* Computing distances to all centers and sorting them.
     * The aim is to go through the nearest centers first, to get the shortest 'threshold' possible.
     */
    std::deque<std::pair<double, size_t> > center_order(ncenters);
    for (size_t center=0; center<ncenters; ++center, center_ptr+=ndims) {
        center_order[center].first=std::sqrt(compute_sqdist(current, center_ptr));
        center_order[center].second=center;
    }
    std::sort(center_order.begin(), center_order.end());

    // Computing the distance to each center, and deciding whether to proceed for each cluster.
    for (const auto& curcent : center_order) {
        const size_t& center=curcent.second;
        const double& dist2center=curcent.first;

        const int& cur_nobs=clust_nobs[center];
        if (!cur_nobs) { continue; }
        const double* dIt=clust_dist[center].begin();
        const double& maxdist=*(dIt + cur_nobs-1);

        int firstcell=0;
#if USE_UPPER
        double upper_bd=R_PosInf;
#endif
        if (R_FINITE(threshold2)) {
            const double threshold=std::sqrt(threshold2);

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

        const int& cur_start=clust_start[center];
        const double* other_cell=exprs.begin() + ndims * (cur_start + firstcell);
        for (int celldex=firstcell; celldex<cur_nobs; ++celldex, other_cell+=ndims) {
#if USE_UPPER
            if (*(dIt + celldex) > upper_bd) {
                break;
            }
#endif

            const double dist2cell2=compute_sqdist(current, other_cell);
            if (nearest.size() < nn || dist2cell2 < threshold2) {
                nearest.push(std::make_pair(dist2cell2, cur_start + celldex));
                if (nearest.size() > nn) {
                    nearest.pop();
                }
                if (nearest.size()==nn) {
                    threshold2=nearest.top().first; // Shrinking the threshold, if an earlier NN has been found.
#if USE_UPPER
                    upper_bd=std::sqrt(threshold2) + dist2center; 
#endif
                }
            }
        }
    }
    return;
}
