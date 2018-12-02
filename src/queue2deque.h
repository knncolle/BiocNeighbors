#ifndef QUEUE2DEQUE_H
#define QUEUE2DEQUE_H
#include <queue>
#include <deque>

// Converts information to neighbors/distances. Also clears 'nearest'.

template<typename T, typename M>
void queue2deque(std::priority_queue<std::pair<double, T> >& nearest,
    std::deque<M>& neighbors, std::deque<double>& distances, 
    const bool index, const bool dist, 
    const bool discard_self, M self)
{
    neighbors.clear();
    distances.clear();
    if (nearest.empty()) {
        return;
    }

    bool found_self=false;
    while (!nearest.empty()) {
        if (discard_self && nearest.top().second==self) {
            nearest.pop();
            found_self=true;
            continue;
        }

        if (index) {
            neighbors.push_front(nearest.top().second);
        }
        if (dist) {
            distances.push_front(nearest.top().first);
        }

        nearest.pop();
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
    
#endif
