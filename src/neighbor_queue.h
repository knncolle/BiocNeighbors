#ifndef NEIGHBOR_QUEUE_H
#define NEIGHBOR_QUEUE_H

#include <queue>
#include <deque>
#include "Rcpp.h"
#include "utils.h"

/* The neighbor_queue class is a priority queue that contains indices and
 * distances in decreasing order from the top of the queue. Existing elements
 * are displaced by incoming elements that have shorter distances, thus making
 * it a useful data structure for retaining the k-nearest neighbors.
 *
 * We augment a normal priority queue with some extra features to:
 *
 * - remove self-matches for kNN searches. In such cases, the size of the queue
 *   is set to k+1 during the search, and any self-match is removed when the
 *   neighbors are reported to yield exactly k neighbors.  We cannot simply
 *   remove the closest neighbor in case of duplicates.
 * - warn about ties for exact searches. In such cases, the size of the queue
 *   is set to k+1 search. Reporting will check for tied distances among queue
 *   elements and emit one R warning per lifetime of the queue (to avoid
 *   saturating the warning counter in practical settings).
 *
 * Both of these options can be applied together, in which case the size of the
 * queue is set to k+2, any self-matches are removed, and distances are
 * searched for ties.
 */

class neighbor_queue {
public:
    neighbor_queue(bool t) : ties(t) {}

    void setup(NumNeighbors_t k) {
        self=false;
        base_setup(k);
        return;
    }

    void setup(NumNeighbors_t k, CellIndex_t s) {
        self=true;
        self_dex=s;
        base_setup(k);
        return;
    }

    void add(CellIndex_t i, double d) {
        if (!full) {
            nearest.push(NeighborPoint(d, i));
            if (static_cast<NumNeighbors_t>(nearest.size())==check_k) {
                full=true;
            }
        } else if (d < limit()) {
            nearest.push(NeighborPoint(d, i));
            nearest.pop();
        }
        return;
    }

    bool is_full() const {
        return full;
    }

    double limit() const {
        return nearest.top().first;
    }

    template<class Distance>
    void report(std::deque<CellIndex_t>& neighbors, std::deque<double>& distances, bool index, bool dist, bool normalize=false) {
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
            for (size_t d=1; d<distances.size(); ++d) {
                if (distances[d-1] >= distances[d]) {
                    // Setting ties to false, as we've found the first instance.
                    ties=false;
                    Rcpp::warning("detected tied distances to neighbors, see ?'BiocNeighbors-ties'");
                    break;
                }
            }
        
            // We assume that the NN search was conducted with an extra neighbor if ties=true upon entry.
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
private:
    bool ties;
    bool self=false;
    CellIndex_t self_dex=0;
    NumNeighbors_t n_neighbors=0, check_k=1;
    bool full=false;

    void base_setup(NumNeighbors_t k) {
        n_neighbors=k;
        check_k=n_neighbors + self + ties;
        full=(check_k==0);
        return;
    }

    typedef std::pair<double, CellIndex_t> NeighborPoint;
    std::priority_queue<NeighborPoint> nearest;
};

#endif
