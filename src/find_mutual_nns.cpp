#include "Rcpp.h"
#include <algorithm>
#include <vector>

//[[Rcpp::export(rng=false)]]
Rcpp::List find_mutual_nns (Rcpp::IntegerMatrix left, Rcpp::IntegerMatrix right) {
    size_t right_obs = right.ncol();
    size_t right_k = right.nrow();
    std::vector<int> sortedR(right.begin(), right.end());

    // Sorting the elements on the right.
    auto sIt = sortedR.begin();
    for (size_t r = 0; r < right_obs; ++r) {
        auto sEnd = sIt;
        for (size_t i = 0; i < right_k; ++i) {
            --(*sEnd); // make it 0-indexed.
            ++sEnd;
        }
        std::sort(sIt, sEnd);
        sIt = sEnd;
    }

    std::vector<int> mutualL, mutualR;
    size_t left_obs = left.ncol();
    std::vector<size_t> unsearched_offset(right_obs);

    // Running through the elements on the left, and doing a binary search for
    // the presence of the left neighbor in each of its right neighbor's
    // nearest lists.
    for (size_t l = 0; l < left_obs ; ++l) {
        auto curcol = left.column(l);

        for (auto curval0 : curcol) {
            int curval = curval0 - 1; // make it 0-indexed.
            size_t& already_searched = unsearched_offset[curval];
            if (already_searched == right_k) {
                continue;
            }

            auto startIt = sortedR.begin() + right_k * static_cast<size_t>(curval); // cast to size_t to avoid overflow.
            auto endIt = startIt + right_k;
            auto closest = std::lower_bound(startIt + already_searched, endIt, l); // '+ already_searched' allows us to skip the neighbors processed by earlier 'l'.

            if (closest != endIt && *closest == l) { 
                mutualL.push_back(l + 1); // restoring to 1-indexing for output.
                mutualR.push_back(curval0);
            }

            // Note that we can always move 'unsearched_offset' forward because
            // each right neighbor's list is sorted (as above) and we're also
            // iterating through the left matrix in order; we'll never be
            // searching for a lower 'l' again, because we already did that in
            // a previous iteration of the outer loop.
            already_searched = closest - startIt;
        }
    }

    return Rcpp::List::create(
        Rcpp::IntegerVector(mutualL.begin(), mutualL.end()),
        Rcpp::IntegerVector(mutualR.begin(), mutualR.end())
    );
}
