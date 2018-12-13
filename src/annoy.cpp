#include "annoy.h"

Annoy::Annoy(SEXP ndim, SEXP fname) : NDims(check_integer_scalar(ndim, "number of dimensions")), obj(NDims), holding(NDims) {
    auto Fname=check_string(fname, "index file name");
    obj.load(Fname.c_str());
    return;
}

MatDim_t Annoy::get_nobs() const {
    return obj.get_n_items();
}

MatDim_t Annoy::get_ndims() const {
    return NDims;
}

std::deque<CellIndex_t>& Annoy::get_neighbors () {
    return neighbors;
}

std::deque<double>& Annoy::get_distances () {
    return distances;
}

void Annoy::find_nearest_neighbors(CellIndex_t c, NumNeighbors_t K, const bool index, const bool distance) {
    std::vector<Data_t>* dptr=(distance ? &kept_dist: NULL);
    obj.get_nns_by_item(c, K + 1, -1, &kept_idx, dptr); // +1, as it forgets to discard 'self'.

    neighbors.clear();
    distances.clear();
    NumNeighbors_t counter=0;
    for (size_t idx=0; idx<kept_idx.size() && counter < K; ++idx) { // protect against API returning more/less NNs.
        if (static_cast<CellIndex_t>(kept_idx[idx])!=c) {
            if (index) {
                neighbors.push_back(kept_idx[idx]); 
            }
            if (distance) {
                distances.push_back(kept_dist[idx]);
            }
            ++counter;
        }
    }

    kept_idx.clear();
    kept_dist.clear();
    return;
}


void Annoy::find_nearest_neighbors(const double* query, NumNeighbors_t K, const bool index, const bool distance) {
    std::vector<Data_t>* dptr=(distance ? &kept_dist: NULL);
    std::copy(query, query+NDims, holding.begin());
    obj.get_nns_by_vector(holding.data(), K, -1, &kept_idx, dptr);

    const NumNeighbors_t limit=std::min(K, static_cast<NumNeighbors_t>(kept_idx.size())); // as the API can yield < K elements.

    if (index) {
        neighbors.resize(limit);
        std::copy(kept_idx.begin(), kept_idx.begin() + limit, neighbors.begin());
    }
    kept_idx.clear();

    if (distance) {
        distances.resize(limit);
        std::copy(kept_dist.begin(), kept_dist.begin() + limit, distances.begin());
        kept_dist.clear();
    }

    return;
}

