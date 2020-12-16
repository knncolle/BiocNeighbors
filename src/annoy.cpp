#include "annoy.h"

template<class Distance>
Annoy<Distance>::Annoy(int ndim, const std::string& fname, double mult) : 
    NDims(ndim), obj(NDims), holding(NDims), search_mult(mult)
{
    obj.load(fname.c_str());
    if (search_mult <= 1) {
        throw std::runtime_error("search multiplier should be greater than 1");
    }
    return;
}

template<class Distance>
MatDim_t Annoy<Distance>::get_nobs() const {
    return obj.get_n_items();
}

template<class Distance>
MatDim_t Annoy<Distance>::get_ndims() const {
    return NDims;
}

template<class Distance>
const std::vector<typename Annoy<Distance>::Index_t>& Annoy<Distance>::get_neighbors () const {
    return kept_idx;
}

template<class Distance>
const std::vector<typename Annoy<Distance>::Data_t>& Annoy<Distance>::get_distances () const {
    return kept_dist;
}

template<class Distance>
NumNeighbors_t Annoy<Distance>::get_search_k(NumNeighbors_t K) {
    return search_mult * K + 0.5; // rounded up.
}

template<class Distance>
void Annoy<Distance>::find_nearest_neighbors(CellIndex_t c, NumNeighbors_t K, const bool index, const bool distance) {
    kept_idx.clear();
    kept_dist.clear();
    std::vector<Data_t>* dptr=(distance ? &kept_dist : NULL);
    obj.get_nns_by_item(c, K + 1, get_search_k(K + 1), &kept_idx, dptr); // +1, as it forgets to discard 'self'.
    
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

template<class Distance>
void Annoy<Distance>::find_nearest_neighbors(const double* query, NumNeighbors_t K, const bool index, const bool distance) {
    kept_idx.clear();
    kept_dist.clear();
    std::vector<Data_t>* dptr=(distance ? &kept_dist: NULL);
    std::copy(query, query+NDims, holding.begin());
    obj.get_nns_by_vector(holding.data(), K, get_search_k(K), &kept_idx, dptr);
    if (!index) {
        kept_idx.clear();
    }
    return;
}

template class Annoy<Manhattan>;
template class Annoy<Euclidean>;

// [[Rcpp::export(rng=false)]]
Rcpp::IntegerVector annoy_version() {
    return Rcpp::IntegerVector::create(Rcpp::Named("major")=RCPPANNOY_VERSION_MAJOR,
                                       Rcpp::Named("minor")=RCPPANNOY_VERSION_MINOR,
                                       Rcpp::Named("patch")=RCPPANNOY_VERSION_PATCH);
}
