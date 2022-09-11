// Functions to compute the distance used to compute the Fasano-Franceschini test statistic

#ifndef DISTANCE_H
#define DISTANCE_H

#include <vector>
#include <limits>
#include "RangeTree.h"
#include "matrix_util.h"

typedef RangeTree::RangeTree<double, int> RTree;
typedef RangeTree::Point<double, int> Point;

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
    std::vector<Point> pts1;
    std::vector<Point> pts2;
    pts1.reserve(r1);
    pts2.reserve(r2);

    // Extract points from first sample
    for (std::size_t i = 0; i < r1; ++i) {
        pts1.push_back(Point(getRow<MatrixT>(M, s[i]), 0));
    }

    // Extract points from second sample
    for (std::size_t i = 0; i < r2; ++i) {
        pts2.push_back(Point(getRow<MatrixT>(M, s[i + r1]), 0));
    }

    // Construct range trees
    return {RTree(pts1), RTree(pts2)};
}

// Evaluate the distance function using the range tree method.
//
// @param rtree1 range tree for the first sample
// @param rtree2 range tree for the second sample
// @param n1 the number of points in the first sample
// @param n2 the number of points in the second sample
// @param origin vector
// @return double
double rangeDistance(const RTree& rtree1,
                     const RTree& rtree2,
                     const int n1,
                     const int n2,
                     const std::vector<double>& origin) {
    std::size_t ndim = origin.size();
    std::vector<bool> strict(ndim, false);
    double inf = std::numeric_limits<double>::infinity();

    double d = -1;
    for (std::size_t i = 0; i < (1 << ndim); ++i) {
        // Determine the upper and lower bounds on each dimension for the given orthant.
        // That is, the orthant can be represented as the Cartesian product of the intervals
        // (lowerLims[j], upperLims[j]) for j = 0, 1,..., ndim-1.
        std::vector<double> lowerLims(ndim), upperLims(ndim);
        for (std::size_t j = 0; j < ndim; ++j) {
            // The orthants in R^d can be enumerated as all length d combinations of < and >.
            // Representing < as 0 and > as 1, these combinations are given by the rows of the
            // (2^d)xd matrix whose i-th row is the integer i in binary. The (i,j) element of
            // this matrix is given by (i & (1 << (d-1-j))).
            bool less = (i & (1 << (ndim - 1 - j)));
            lowerLims[j] = less ? -inf : origin[j];
            upperLims[j] = less ? origin[j] : inf;
        }
        double res1 = rtree1.countInRange(lowerLims, upperLims, strict, strict);
        double res2 = rtree2.countInRange(lowerLims, upperLims, strict, strict);
        d = std::max(d, abs(res1 / n1 - res2 / n2));
    }
    return d;
}


// Find the octant relative to the specified origin that a point lies in.
//
// @param pt the point
// @param origin the origin
// @return the octant the point lies in. If there is a tie along one coordinate, returns -1.
int findOct(const std::vector<double>& pt,
            const std::vector<double>& origin) {
    std::size_t ndim = pt.size();
    int oct = 0;
    for (std::size_t i = 0; i < ndim; ++i) {
        if (pt[i] > origin[i]) {
            oct += 1 << (ndim - i - 1);
        } else if (pt[i] == origin[i]) {
            // Ties are not counted
            return -1;
        }
    }
    return oct;
}


// Evaluate the distance function using the brute force method.
//
// @param M a matrix of type NumericMatrix or RMatrix<double>
// @param n1 the number of points in the first sample
// @param n2 the number of points in the second sample
// @param s a permutation of {0,1,...,(r1+r2-1)} which indicates which points belong to each sample
// @param origin_ix row of M to use as the origin
// @return double
template<typename MatrixT>
double bruteDistance(const MatrixT M,
                     const std::size_t n1,
                     const std::size_t n2,
                     const std::vector<std::size_t>& s,
                     const std::size_t origin_ix) {
    std::vector<double> origin = getRow<MatrixT>(M, s[origin_ix]);

    // Although a std::vector would be much easier to use, memory limits can be hit for large
    // dimensions since the vector would be of length 2^d. Assuming that often many octants
    // will be empty, we can reduce memory by using a sparse structure instead.
    std::unordered_map<int, double> counts1;
    std::unordered_map<int, double> counts2;

    // Determine which octant each point in the first sample lies in
    for (std::size_t i = 0; i < n1; ++i) {
        int oct = findOct(getRow<MatrixT>(M, s[i]), origin);
        counts1[oct] = 1 + ((counts1.find(oct) == counts1.end()) ? 0 : counts1[oct]);
    }

    // Determine which octant each point in the second sample lies in
    for (std::size_t i = 0; i < n2; ++i) {
        int oct = findOct(getRow<MatrixT>(M, s[i + n1]), origin);
        counts2[oct] = 1 + ((counts2.find(oct) == counts2.end()) ? 0 : counts2[oct]);
    }

    // Divide every value in counts1 by n1
    for (auto itr = counts1.begin(); itr != counts1.end(); ++itr) {
        itr->second /= static_cast<double>(n1);
    }

    // Subtract counts2/n2 from counts1/n1
    for (const auto& kv : counts2) {
        int key = kv.first;
        double val = -kv.second / static_cast<double>(n2);
        counts1[key] = val + ((counts1.find(key) == counts1.end()) ? 0 : counts1[key]);
    }

    // Return the maximum absolute difference
    double d = 0;
    for (const auto& kv : counts1) {
        // Note that oct = -1 is just used as a sink for uncounted points
        d = (kv.first != -1) ? std::max(d, abs(kv.second)) : d;
    }
    return d;
}

#endif //DISTANCE_H
