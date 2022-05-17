// Functions to perform range counting

#ifndef RANGECOUNT_H
#define RANGECOUNT_H

#include <Rcpp.h>
#include <vector>
#include <limits>
#include "RangeTree.h"
#include "matrix_util.h"

using namespace Rcpp;

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
    std::vector<RangeTree::Point<double,int> > pts1(r1);
    std::vector<RangeTree::Point<double,int> > pts2(r2);

    // Extract points from first sample
    for (std::size_t i = 0; i < r1; ++i) {
        pts1[i] = RangeTree::Point<double,int>(getRow<MatrixT>(M, s[i]), 0);
    }

    // Extract points from second sample
    for (std::size_t i = 0; i < r2; ++i) {
        pts2[i] = RangeTree::Point<double,int>(getRow<MatrixT>(M, s[i+r1]), 0);
    }

    // Construct range trees
    return {RTree(pts1), RTree(pts2)};
}

// Count the number of points in each axis-aligned orthant defined by the origin
//
// @param rtree range tree
// @param origin vector
// @return vector counting points in each orthant
std::vector<double> rangeCount(const RTree& rtree,
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

#endif //RANGECOUNT_H
