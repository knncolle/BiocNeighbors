#include "neighbor_queue.h"
#include "distances.h"

template<class Distance>
void neighbor_queue<Distance>::base_setup(NumNeighbors_t k) {
    n_neighbors=k;
    check_k=n_neighbors + self + ties;
    full=(check_k==0);
    return;
}

template<class Distance>
void neighbor_queue<Distance>::setup(NumNeighbors_t k) {
    self=false;
    base_setup(k);
    return;
}

template<class Distance>
void neighbor_queue<Distance>::setup(NumNeighbors_t k, CellIndex_t s) {
    self=true;
    self_dex=s;
    base_setup(k);
    return;
}

template<class Distance>
void neighbor_queue<Distance>::add(CellIndex_t i, double d) {
    if (!full) {
        nearest.push(NeighborPoint(d, i));
        if (static_cast<NumNeighbors_t>(nearest.size())==check_k) {
            full=true;
        }
    } else if (d < nearest.top().first) {
        nearest.push(NeighborPoint(d, i));
        nearest.pop();
    }
    return;
}

template<class Distance>
bool neighbor_queue<Distance>::is_full() const {
    return full;
}

template<class Distance>
double neighbor_queue<Distance>::limit() const {
    return nearest.top().first;
}

// Converts information to neighbors/distances. Also clears 'nearest'.
template<class Distance>
void neighbor_queue<Distance>::report(std::deque<CellIndex_t>& neighbors, std::deque<double>& distances, bool index, bool dist, bool normalize) {
    neighbors.clear();
    distances.clear();
    if (nearest.empty()) {
        return;
    }

    // If 'self=false', then it never enters the !found_self clause below, which is the correct behaviour.
    bool found_self=!self; 

    while (!nearest.empty()) {
        if (!found_self && nearest.top().second==self_dex) {
            nearest.pop();
            found_self=true;
            continue;
        }

        if (index) {
            neighbors.push_front(nearest.top().second);
        }
        if (dist || ties) {
            distances.push_front(nearest.top().first);
        }

        nearest.pop();
    }

    // Getting rid of the last entry to get the 'k' nearest neighbors, if 'self' was not in the queue.
    if (self && !found_self) {
        if (!neighbors.empty()) { 
            neighbors.pop_back();
        }
        if (!distances.empty()) {
            distances.pop_back();
        }
    }

    // Square rooting if the distances were squared.
    if (normalize && !distances.empty()) {
        for (auto& d : distances) { d=Distance::normalize(d); }
    }

    if (ties) {
        constexpr double TOLERANCE=1.00000001;
        for (size_t d=1; d<distances.size(); ++d) {
            if (distances[d-1] * TOLERANCE >= distances[d]) {
                // Setting ties to false, as we've found the first instance.
                ties=false;
                Rcpp::warning("tied distances detected in nearest-neighbor calculation");
                break;
            }
        }
    
        // We assume that the NN search was conducted with an extra neighbor if diagnose_ties=true upon entry.
        // This is necessary to allow the above code to check for whether there is a tie at the boundary of the set.
        // It is now time to remove this extra neighbor which should lie at the end of the set. The exception
        // is when we never actually fill up the queue, in which case we shouldn't do any popping.
        if (static_cast<NumNeighbors_t>(neighbors.size()) > n_neighbors) {
            neighbors.pop_back();
        }
        if (!dist) {
            distances.clear();
        } else if (static_cast<NumNeighbors_t>(distances.size()) > n_neighbors) {
            distances.pop_back();
        }
    }

    return;
}

template class neighbor_queue<BNManhattan>;
template class neighbor_queue<BNEuclidean>;

