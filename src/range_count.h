// Functions to perform range counting

#ifndef RANGECOUNT_H
#define RANGECOUNT_H

#include <vector>
#include <limits>
#include "RangeTree.h"
#include "matrix_util.h"

typedef RangeTree::RangeTree<double,int> RTree;

// Build range trees for two samples
//
// @param M matrix of two data samples row binded
// @param r1 number of points in first sample
// @param r2 number of points in second sample
// @param s a permutation of {0,1,...,(r1+r2-1)} which indicates which points belong to each sample
// @return range trees built on each sample
template<typename MatrixT>
std::vector<RTree> buildRangeTrees(const MatrixT M,
                                   std::size_t r1,
                                   std::size_t r2,
                                   const std::vector<std::size_t>& s) {
    std::vector<RangeTree::Point<double,int> > pts1;
    pts1.reserve(r1);
    std::vector<RangeTree::Point<double,int> > pts2;
    pts2.reserve(r2);

    // Extract points from first sample
    for (std::size_t i = 0; i < r1; ++i) {
        pts1.push_back(RangeTree::Point<double,int>(getRow<MatrixT>(M, s[i]), 0));
    }

    // Extract points from second sample
    for (std::size_t i = 0; i < r2; ++i) {
        pts2.push_back(RangeTree::Point<double,int>(getRow<MatrixT>(M, s[i+r1]), 0));
    }

    // Construct range trees
    return {RTree(pts1), RTree(pts2)};
}

// Count the number of points in each axis-aligned orthant defined by the origin.
// Uses range tree method.
//
// @param rtree range tree
// @param origin vector
// @return vector counting points in each orthant
std::vector<double> rangeCountTree(const RTree& rtree,
                                   const std::vector<double>& origin) {
    std::size_t ndim = origin.size();
    std::size_t noct = 1<<ndim;
    std::vector<double> res(noct);
    std::vector<bool> strict(ndim, false);
    double inf = std::numeric_limits<double>::infinity();

    // Count the number of points in each axis-aligned orthant using specified origin
    for (std::size_t i = 0; i < noct; ++i) {
        // Determine the upper and lower bounds on each dimension for the given orthant.
        // That is, the orthant can be represented as the Cartesian product of the intervals
        // (lowerLims[j],upperLims[j]) for j=0,1,...,ndim-1.
        std::vector<double> lowerLims(ndim), upperLims(ndim);
        for (std::size_t j = 0; j < ndim; ++j) {
            // The orthants in R^d can be enumerated as all length d combinations of < and >.
            // Representing < as 0 and > as 1, these combinations are given by the rows of the
            // (2^d)xd matrix whose i-th row is the integer i in binary. The (i,j) element of
            // this matrix is given by (i & (1 << (d-1-j))).
            bool s = i & (1 << (ndim-1-j));
            lowerLims[j] = (s ? -inf : origin[j]);
            upperLims[j] = (s ? origin[j] : inf);
        }
        res[i] = rtree.countInRange(lowerLims, upperLims, strict, strict);
    }
    return res;
}

// Count the number of points in each axis-aligned orthant defined by the origin.
// Uses brute force method.
//
// @param M a matrix of type NumericMatrix or RMatrix<double>
// @param npts how many points are in the sample
// @param offset where to start counting the points
// @param shuffle whether to shuffle the samples
// @param s a permutation of {0,1,...,(r1+r2-1)} which indicates which points belong to each sample
// @param origin vector
// @return vector counting points in each orthant
template<typename MatrixT>
std::vector<double> rangeCountBrute(const MatrixT M,
                                    std::size_t npts,
                                    std::size_t offset,
                                    const std::vector<std::size_t>& s,
                                    const std::vector<double>& origin) {
    std::size_t ndim = origin.size();
    std::size_t noct = 1<<ndim;
    std::vector<double> counts(noct);

    for (std::size_t i = 0; i < npts; ++i) {
        std::vector<double> pt = getRow<MatrixT>(M, s[i + offset]);
        for (std::size_t j = 0; j < noct; ++j) {
            bool in_oct = true;
            for (std::size_t k = 0; k < ndim; ++k) {
                // Whether bound is < (0) or > (1)
                bool s = j & (1 << (ndim-1-k));
                // Whether pt satisfies the correct bound in the k-th coordinate
                bool check = s ? (pt[k] < origin[k]) : (pt[k] > origin[k]);
                in_oct = in_oct && check;
            }
            if (in_oct) {
                // The orthant containing pt has been found
                ++counts[j];
                break;
            }
        }
    }
    return counts;
}

#endif //RANGECOUNT_H
