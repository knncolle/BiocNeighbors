#include "objects.h"
#include "utils.h"

/****************** Naive search object *********************/

naive_holder::naive_holder (SEXP ex) : exprs(ex), last_distance2(R_NaReal), tie_warned(false) {}

naive_holder::~naive_holder() { }

size_t naive_holder::get_nobs() const { return exprs.ncol(); }

size_t naive_holder::get_ndims() const { return exprs.nrow(); }

std::deque<size_t>& naive_holder::get_neighbors () { return neighbors; }

std::deque<double>& naive_holder::get_distances () { return distances; }

void naive_holder::find_neighbors (size_t cell, double threshold, const bool index, const bool dist) {
    if (cell >= size_t(exprs.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=exprs.column(cell);
    search_all(curcol.begin(), threshold, index, dist);
    return;
}

void naive_holder::find_neighbors (const double* current, double threshold, const bool index, const bool dist) {
    search_all(current, threshold, index, dist);
    return;
}

void naive_holder::find_nearest_neighbors (size_t cell, size_t nn, const bool index, const bool dist) {
    if (cell >= size_t(exprs.ncol())) {
        throw std::runtime_error("cell index out of range");
    }
    auto curcol=exprs.column(cell);
    search_nn(curcol.begin(), nn+1);

    // Converts information to neighbors/distances. Also clears 'nearest'.
    pqueue2deque(index, dist, true, cell);
    return;
}

void naive_holder::find_nearest_neighbors (const double* current, size_t nn, const bool index, const bool dist) {
    search_nn(current, nn);
    pqueue2deque(index, dist); 
    return;
}

double naive_holder::compute_sqdist(const double* x, const double* y) const {
    double out=0;
    const size_t NR=exprs.nrow();
    for (size_t m=0; m<NR; ++m) {
        const double tmp=x[m]-y[m];
        out+=tmp*tmp;
    }
    return out;
}

constexpr double TOLERANCE=1.0000000001;

void naive_holder::pqueue2deque(const bool index, const bool dist, bool discard_self, size_t self)
/* Converts the nearest-neighbor queue into user-visible deque outputs.
 * Also checks for ties via the reported 'last_distance2'.
 */
{
    neighbors.clear();
    distances.clear();
    if (current_nearest.empty()) {
        return;
    }

    // Distance needs to be square rooted as it is stored as a square.
    // If NA, we tking any value larger than the largest in 'current_nearest', if last_distance2 is NA.
    double lastdist=(ISNA(last_distance2) ? std::sqrt(current_nearest.top().first) + 1 : std::sqrt(last_distance2));
    bool found_self=false;

    while (!current_nearest.empty()) {
        if (discard_self && size_t(current_nearest.top().second)==self) {
            current_nearest.pop();
            found_self=true;
            continue;
        }

        // Deciding what to store. Distances need to be rooted as they are stored as square roots.
        if (index) {
            neighbors.push_front(current_nearest.top().second);
        }
        const double curdist=std::sqrt(current_nearest.top().first);
        if (dist) {
            distances.push_front(curdist);
        }

        // Checking for ties with the last distance, using a relative TOLERANCE on the smaller distance.
        // Distances should always be decreasing, so we know that 'curdist' is always the smaller.
        // We use '<=' to handle cases where both 'curdist' and 'lastdist' are zero.
        if (!tie_warned && lastdist <= curdist * TOLERANCE) {
            tie_warned=true;
            Rcpp::warning("tied distances detected in nearest-neighbor calculation");
        } else {
            lastdist=curdist;
        }

        current_nearest.pop();
    }

    // Getting rid of the last entry to get the 'k' nearest neighbors, if 'self' was not in the queue.
    if (discard_self && !found_self) {
        if (index) { 
            neighbors.pop_back();
        }
        if (dist) {
            distances.pop_back();
        }
    }
    return;
}

void naive_holder::search_all(const double* current, double threshold, const bool index, const bool dist) {
    neighbors.clear();
    distances.clear();

    const size_t& ndims=exprs.nrow();
    const size_t& nobs=exprs.ncol();
    const double* other=exprs.begin(); // iterator coerced to pointer.
    const double threshold2=threshold*threshold; // squaring.

    for (size_t c=0; c<nobs; ++c, other+=ndims) {
        const double curdist2=compute_sqdist(current, other);
        if (curdist2 <= threshold2) {
            if (index) {
                neighbors.push_back(c);
            }
            if (dist) {
                distances.push_back(std::sqrt(curdist2));
            }
        }
    }
    return;
}

void naive_holder::search_nn (const double* current, size_t nn) {
    const size_t& ndims=exprs.nrow();
    const size_t& nobs=exprs.ncol();
    const double* other=exprs.begin(); // iterator coerced to pointer.
    last_distance2=R_NaReal;

    for (size_t c=0; c<nobs; ++c, other+=ndims) {
        const double curdist2=compute_sqdist(current, other);
        if (current_nearest.size() < nn || curdist2 < current_nearest.top().first) {
            current_nearest.push(std::make_pair(curdist2, c));
            if (current_nearest.size() > nn) {
                last_distance2=current_nearest.top().first;
                current_nearest.pop();
            }
        } else if (ISNA(last_distance2) || curdist2 < last_distance2) {
            last_distance2=curdist2;
        }
    }
    return;
}

/****************** Convex search object *********************/

#define USE_UPPER 0

convex_holder::convex_holder(SEXP ex, SEXP cen, SEXP info) : naive_holder(ex), centers(cen) {
    const size_t& ncenters=centers.ncol();

    Rcpp::List _info(info);
    for (size_t i=0; i<ncenters; ++i) {
        Rcpp::List current(_info[i]);
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

convex_holder::~convex_holder() { }

void convex_holder::search_all (const double* current, double threshold, const bool index, const bool dist) {
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

void convex_holder::search_nn(const double* current, size_t nn) {
    const size_t& ndims=exprs.nrow();
    const size_t& ncenters=centers.ncol();
    const double* center_ptr=centers.begin();
    double threshold2 = R_PosInf;
    last_distance2=R_NaReal;

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
             * where the TOLERANCE allows the condition to be 'false' upon ties with some numerical imprecision.
             * All points (if any) within this cluster with distances above 'lower_bd' are potentially countable.
             *
             * Multiplication of 'threshold' by 'TOLERANCE' is only necessary for tie detection.
             * Any extra points that are retained will not enter the n-nearest queue (aside from oddities due to numerical precision).
             * These extra points may, however, update 'last_distance2', which is why we need to consider them.
             *
             * The multiplication itself mirrors the multiplication of 'curdist' above.
             * 'threshold' is equivalent to the smaller distance, and we are looking for equal or larger distances in 'dIt' (hence, 'lower_bound').
             * We use '<' to skip in the _absence_ of equality, which mirrors the use of '<=' above and avoids skipping distances of zero.
             */
            const double lower_bd=dist2center - (threshold * TOLERANCE);
            if (maxdist < lower_bd) {
                continue;
            }
            firstcell=std::lower_bound(dIt, dIt+cur_nobs, lower_bd)-dIt;
#if USE_UPPER
            /* This exploits the reverse triangle inequality, to ignore points where:
             *     threshold + dist2center < point-to-center distance
             * with TOLERANCE to capture points that are equal with some numeric precision.
             */
            upper_bd = (threshold * TOLERANCE) + dist2center;
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
            if (current_nearest.size() < nn || dist2cell2 < threshold2) {
                current_nearest.push(std::make_pair(dist2cell2, cur_start + celldex));
                if (current_nearest.size() > nn) {
                    last_distance2=current_nearest.top().first;
                    current_nearest.pop();
                }
                if (current_nearest.size()==nn) {
                    threshold2=current_nearest.top().first; // Shrinking the threshold, if an earlier NN has been found.
#if USE_UPPER
                    upper_bd=(std::sqrt(threshold2) * TOLERANCE) + dist2center; // see above for use of TOLERANCE.
#endif
                }
            } else if (ISNA(last_distance2) || dist2cell2 < last_distance2) {
                last_distance2=dist2cell2;
            }
        }
    }
    return;
}

/****************** Finder *********************/

std::unique_ptr<naive_holder> generate_holder(SEXP coords, SEXP centers, SEXP clust_info) {
    if (centers==R_NilValue || clust_info==R_NilValue) {
        return std::unique_ptr<naive_holder>(new naive_holder(coords));
    } else {
        return std::unique_ptr<naive_holder>(new convex_holder(coords, centers, clust_info));
    }
}

