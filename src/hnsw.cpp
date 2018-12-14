#include "hnsw.h"

Hnsw::Hnsw(SEXP dat, SEXP fname) : data(dat), space(data.nrow()), obj(&space, check_string(fname, "index file name")), holding(data.nrow()) {}

MatDim_t Hnsw::get_nobs() const { 
    return data.ncol();
}

MatDim_t Hnsw::get_ndims() const {
    return data.nrow();
}

const std::deque<CellIndex_t>& Hnsw::get_neighbors () const {
    return kept_idx;
}

const std::deque<double>& Hnsw::get_distances () const {
    return kept_dist;
}

void Hnsw::find_nearest_neighbors(CellIndex_t c, NumNeighbors_t K, const bool index, const bool distance) {
    auto curcol=data.column(c);
    std::copy(curcol.begin(), curcol.end(), holding.begin());
    auto Q=obj.searchKnn(holding.data(), K+1);

    // Auto-filter self.
    bool found_self=false;
    kept_idx.clear();
    kept_dist.clear();

    while (!Q.empty()) {
        const auto& current=Q.top();
        if (static_cast<CellIndex_t>(current.second)!=c) {
            if (index) {
                kept_idx.push_front(current.second);
            } 
            if (distance) {
                kept_dist.push_front(std::sqrt(current.first));
            }
            found_self=true;
        }
        Q.pop();
    }

    if (!found_self) {
        if (!kept_idx.empty()) {
            kept_idx.pop_back();
        }
        if (!kept_dist.empty()) {
            kept_dist.pop_back();
        }
    }

    return;
}

void Hnsw::find_nearest_neighbors(const double* query, NumNeighbors_t K, const bool index, const bool distance) {
    std::copy(query, query+holding.size(), holding.begin());
    auto Q=obj.searchKnn(holding.data(), K);

    while (!Q.empty()) {
        const auto& current=Q.top();
        if (index) {
            kept_idx.push_front(current.second);
        }
        if (distance) {
            kept_dist.push_front(std::sqrt(current.first));
        }
        Q.pop();
    }

    return;
}

