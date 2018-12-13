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

const std::vector<Annoy::Index_t>& Annoy::get_neighbors () const {
    return kept_idx;
}

const std::vector<Annoy::Data_t>& Annoy::get_distances () const {
    return kept_dist;
}

void Annoy::find_nearest_neighbors(CellIndex_t c, NumNeighbors_t K, const bool index, const bool distance) {
    kept_idx.clear();
    kept_dist.clear();
    std::vector<Data_t>* dptr=(distance ? &kept_dist : NULL);
    obj.get_nns_by_item(c, K + 1, -1, &kept_idx, dptr); // +1, as it forgets to discard 'self'.
    
    bool self_found=false;
    for (size_t idx=0; idx<kept_idx.size(); ++idx) { 
        if (static_cast<CellIndex_t>(kept_idx[idx])==c) {
            if (index) {
                kept_idx.erase(kept_idx.begin() + idx);
            }
            if (distance) {
                kept_dist.erase(kept_dist.begin() + idx);
            }
            self_found=true;
            break;
        }
    }

    // Just in case we're full of ties at duplicate points, such that 'c' is not in the set.
    // Note that, if self_found=false, we must have at least 'K+2' points for 'c' to not 
    // be detected as its own neighbor. Thus there is no need to worry whether we are popping 
    // off a non-'c' element at the end of the vector.
    if (!self_found) {
        if (index) {
            kept_idx.pop_back();
        }
        if (distance) {
            kept_dist.pop_back();
        }
    }

    // For consistency.
    if (!index) {
        kept_idx.clear();
    }
    return;
}


void Annoy::find_nearest_neighbors(const double* query, NumNeighbors_t K, const bool index, const bool distance) {
    kept_idx.clear();
    kept_dist.clear();
    std::vector<Data_t>* dptr=(distance ? &kept_dist: NULL);
    std::copy(query, query+NDims, holding.begin());
    obj.get_nns_by_vector(holding.data(), K, -1, &kept_idx, dptr);
    if (!index) {
        kept_idx.clear();
    }
    return;
}

