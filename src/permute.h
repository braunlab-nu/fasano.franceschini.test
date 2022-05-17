// Utility functions for performing a permutation test

#ifndef PERMUTE_H
#define PERMUTE_H

#include <vector>
#include <random>
#include <algorithm>

// Return the sequence 0,1,...,n-1
// @param n length of the sequence
// @return the vector 0,1,...,n-1
std::vector<std::size_t> seq(std::size_t n) {
    std::vector<std::size_t> ix(n);
    std::iota(ix.begin(), ix.end(), 0);
    return ix;
}

// Create a permutation of the indices 0,1,...,n-1
//
// @param n length of permutation
// @param prng a pseudorandom number generator
// @return a permutation of {0,1,...,n-1}
std::vector<std::size_t> permutation(std::size_t n,
                                     std::mt19937& prng) {
    std::vector<std::size_t> ix = seq(n);
    std::shuffle(ix.begin(), ix.end(), prng);
    return ix;
}

#endif //PERMUTE_H
