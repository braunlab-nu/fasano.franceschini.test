#include <Rcpp.h>
#include <RcppParallel.h>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>
#include <numeric>
#include "RangeTree.h"
#include "matrix_util.h"
#include "distance.h"
#include "ProgressBar.h"

using namespace Rcpp;

/********** These are functions for the computing the test statistic. **********/

// Compute FF test statistics
//
// @param M a matrix of type NumericMatrix or RMatrix<double>
// @param n1 number of rows in first sample
// @param n2 number of rows in second sample
// @param shuffle whether to shuffle the samples
// @param prng a pseudorandom number generator (unused if shuffle == false)
// @param method what method to use to compute the test statistic
// @return FF test statistics
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT& M,
                                   std::size_t n1,
                                   std::size_t n2,
                                   bool shuffle,
                                   std::mt19937& prng,
                                   char method) {
    // Build range trees
    std::vector<std::size_t> s(n1 + n2);
    std::iota(s.begin(), s.end(), 0);
    if (shuffle) {
        std::shuffle(s.begin(), s.end(), prng);
    }

    double d1 = 0;
    double d2 = 0;
    if (method == 'r') {
        // Build range trees
        std::vector<RTree> trees = buildRangeTrees<MatrixT>(M, n1, n2, s);

        // Compute test statistic using first sample as origins
        for (std::size_t i = 0; i < n1; ++i) {
            d1 = std::max(d1, rangeDistance(trees[0], trees[1], n1, n2, getRow<MatrixT>(M, s[i])));
        }

        // Compute test statistic using second sample as origins
        for (std::size_t i = 0; i < n2; ++i) {
            d2 = std::max(d2, rangeDistance(trees[0], trees[1], n1, n2, getRow<MatrixT>(M, s[i + n1])));
        }
    } else {
        // Compute test statistic using first sample as origins
        for (std::size_t i = 0; i < n1; ++i) {
            d1 = std::max(d1, bruteDistance(M, n1, n2, s, i));
        }

        // Compute test statistic using second sample as origins
        for (std::size_t i = 0; i < n2; ++i) {
            d2 = std::max(d2, bruteDistance(M, n1, n2, s, i + n1));
        }
    }
    return {d1, d2, sqrt(static_cast<double>(n1 * n2)/(n1 + n2)) * (d1 + d2)/2.0};
}

// Compute FF test statistics. Used only for seeded parallel test.
//
// @param M a matrix of type NumericMatrix or RMatrix<double>
// @param n1 number of rows in first sample
// @param n2 number of rows in second sample
// @param s a permutation of (n1 + n2)
// @param method what method to use to compute the test statistic
// @return FF test statistics
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT& M,
                                   std::size_t n1,
                                   std::size_t n2,
                                   const std::vector<std::size_t>& s,
                                   char method) {
    double d1 = 0;
    double d2 = 0;
    if (method == 'r') {
        // Build range trees
        std::vector<RTree> trees = buildRangeTrees<MatrixT>(M, n1, n2, s);

        // Compute test statistic using first sample as origins
        for (std::size_t i = 0; i < n1; ++i) {
            d1 = std::max(d1, rangeDistance(trees[0], trees[1], n1, n2, getRow<MatrixT>(M, s[i])));
        }

        // Compute test statistic using second sample as origins
        for (std::size_t i = 0; i < n2; ++i) {
            d2 = std::max(d2, rangeDistance(trees[0], trees[1], n1, n2, getRow<MatrixT>(M, s[i + n1])));
        }
    } else {
        // Compute test statistic using first sample as origins
        for (std::size_t i = 0; i < n1; ++i) {
            d1 = std::max(d1, bruteDistance(M, n1, n2, s, i));
        }

        // Compute test statistic using second sample as origins
        for (std::size_t i = 0; i < n2; ++i) {
            d2 = std::max(d2, bruteDistance(M, n1, n2, s, i + n1));
        }
    }
    return {d1, d2, sqrt(static_cast<double>(n1 * n2)/(n1 + n2)) * (d1 + d2)/2.0};
}

// Simplified wrapper for testStatistics without shuffling
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT& M,
                                   std::size_t r1,
                                   std::size_t r2,
                                   char method) {
    // prng is unused, just a place holder
    std::mt19937 prng;
    return testStatistics<MatrixT>(M, r1, r2, false, prng, method);
}

// Simplified wrapper for testStatistics with shuffling
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT& M,
                                   std::size_t r1,
                                   std::size_t r2,
                                   std::mt19937& prng,
                                   char method) {
    return testStatistics<MatrixT>(M, r1, r2, true, prng, method);
}

// Compute FF test statistic (callable from R)
//
// @param S1 first sample
// @param S2 second sample
// @param method what method to use to compute the test statistic
// @return FF test statistics
// [[Rcpp::export(ffTestStatistic)]]
NumericVector ffTestStatistic(const NumericMatrix& S1,
                              const NumericMatrix& S2,
                              char method) {
    return wrap(testStatistics<NumericMatrix>(rbind(S1, S2), S1.nrow(), S2.nrow(), method));
}

/**************************************************************************/


/********** These are functions for the serial permutation test. **********/

// Perform serial permutation test to compute empirical p-value
//
// @param S1 first sample
// @param S2 second sample
// @param nPermute number of iterations to perform
// @param verbose whether to display a progress bar
// @param prng a pseudorandom number generator for permuting the samples
// @param method what method to use to compute the test statistic
// @return empirical p-value
unsigned int permutationTest(const NumericMatrix& S1,
                             const NumericMatrix& S2,
                             int nPermute,
                             bool verbose,
                             std::mt19937& prng,
                             char method) {
    std::size_t r1 = S1.nrow();
    std::size_t r2 = S2.nrow();
    NumericMatrix S = rbind(S1, S2);

    // Compute test statistic for original data
    double Z = testStatistics<NumericMatrix>(S, r1, r2, method)[2];

    // Permute data 'nPermute' times and compute test statistic in each case
    unsigned int pval = 0;
    ProgressBar p(nPermute, verbose);
    for (int i = 0; i < nPermute; ++i) {
        double z = testStatistics<NumericMatrix>(S, r1, r2, prng, method)[2];
        pval += (z >= Z);
        p.step();
    }
    p.finalize();
    return pval;
}

// Seeded version of permutationTest (callable from R)
// [[Rcpp::export(permutationTestSeeded)]]
unsigned int permutationTestSeeded(const NumericMatrix& S1,
                                   const NumericMatrix& S2,
                                   int nPermute,
                                   bool verbose,
                                   char method,
                                   int seed) {
    std::mt19937 prng(seed);
    return permutationTest(S1, S2, nPermute, verbose, prng, method);
}

// Unseeded version of permutationTest (callable from R)
// [[Rcpp::export(permutationTest)]]
unsigned int permutationTest(const NumericMatrix& S1,
                             const NumericMatrix& S2,
                             int nPermute,
                             bool verbose,
                             char method) {
    std::mt19937 prng(std::random_device{}());
    return permutationTest(S1, S2, nPermute, verbose, prng, method);
}

/****************************************************************************/


/********** These are functions for the parallel permutation test. **********/

// Worker for parallel permutation test
struct PermutationTest : public RcppParallel::Worker {
    /* Inputs */
    // Pooled samples
    const RcppParallel::RMatrix<double> S;
    // Number of points in first sample
    const std::size_t r1;
    // Number of points in second sample
    const std::size_t r2;
    // Test statistic for original data
    const double Z;
    // The method to use to compute the test statistic
    char method;

    /* Output */
    unsigned int pval;

    // Constructors
    PermutationTest(const NumericMatrix& S, std::size_t r1, std::size_t r2, double Z, char method) :
        S(S), r1(r1), r2(r2), Z(Z), method(method), pval(0) {}
    PermutationTest(const PermutationTest& p, RcppParallel::Split) :
        S(p.S), r1(p.r1), r2(p.r2), Z(p.Z), method(p.method), pval(0) {}

    // Call operator, compute test statistic for (end - begin) permuted samples
    void operator()(std::size_t begin, std::size_t end) {
        std::mt19937 prng(std::random_device{}());
        for (std::size_t i = begin; i < end; ++i) {
            double z = testStatistics<RcppParallel::RMatrix<double> >(S, r1, r2, prng, method)[2];
            pval += (z >= Z);
        }
    }

    // Join operator
    void join(const PermutationTest& rhs) {
        pval += rhs.pval;
    }
};

// Worker for seeded parallel permutation test
struct PermutationTestSeeded : public RcppParallel::Worker {
    /* Inputs */
    // Pooled samples
    const RcppParallel::RMatrix<double> S;
    // Number of points in first sample
    const std::size_t r1;
    // Number of points in second sample
    const std::size_t r2;
    // Test statistic for original data
    const double Z;
    // The method to use to compute the test statistic
    char method;
    // List of permutations
    std::vector<std::vector<std::size_t> > shuffles;

    /* Output */
    unsigned int pval;

    // Constructors
    PermutationTestSeeded(const NumericMatrix& S, std::size_t r1, std::size_t r2, double Z, char method,
                          const std::vector<std::vector<std::size_t> >& shuffles) :
        S(S), r1(r1), r2(r2), Z(Z), method(method), shuffles(shuffles), pval(0) {}
    PermutationTestSeeded(const PermutationTestSeeded& p, RcppParallel::Split) :
        S(p.S), r1(p.r1), r2(p.r2), Z(p.Z), method(p.method), shuffles(p.shuffles), pval(0) {}

    // Call operator, compute test statistic for (end - begin) permuted samples
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            std::vector<std::size_t> shuffle = shuffles[i];
            double z = testStatistics<RcppParallel::RMatrix<double> >(S, r1, r2, shuffle, method)[2];
            pval += (z >= Z);
        }
    }

    // Join operator
    void join(const PermutationTestSeeded& rhs) {
        pval += rhs.pval;
    }
};

// Perform parallel permutation test to compute empirical p-value
//
// @param S1 first sample
// @param S2 second sample
// @param nPermute number of iterations to perform
// @param method what method to use to compute the test statistic
// @return empirical p-value
// [[Rcpp::export(permutationTestParallel)]]
unsigned int permutationTestParallel(const NumericMatrix& S1,
                                     const NumericMatrix& S2,
                                     int nPermute,
                                     char method) {
    std::size_t r1 = S1.nrow();
    std::size_t r2 = S2.nrow();
    NumericMatrix S = rbind(S1, S2);

    // Compute test statistic for original data
    double Z = testStatistics<NumericMatrix>(S, r1, r2, method)[2];

    // Permute data 'nPermute' times and compute test statistic in each case
    PermutationTest pt(S, r1, r2, Z, method);
    parallelReduce(0, nPermute, pt);
    return pt.pval;
}

// Perform parallel permutation test to compute empirical p-value
//
// @param S1 first sample
// @param S2 second sample
// @param nPermute number of iterations to perform
// @param method what method to use to compute the test statistic
// @return empirical p-value
// [[Rcpp::export(permutationTestParallelSeeded)]]
unsigned int permutationTestParallelSeeded(const NumericMatrix& S1,
                                           const NumericMatrix& S2,
                                           int nPermute,
                                           char method,
                                           int seed) {
    std::size_t r1 = S1.nrow();
    std::size_t r2 = S2.nrow();
    NumericMatrix S = rbind(S1, S2);

    // Permutations have to generated and stored beforehand
    std::mt19937 prng(seed);
    std::vector<std::size_t> s(r1 + r2);
    std::iota(s.begin(), s.end(), 0);
    std::vector<std::vector<std::size_t> > shuffles;
    for (int i = 0; i < nPermute; ++i) {
        std::vector<std::size_t> s2 = s;
        std::shuffle(s2.begin(), s2.end(), prng);
        shuffles.push_back(s2);
    }

    // Compute test statistic for original data
    double Z = testStatistics<NumericMatrix>(S, r1, r2, method)[2];

    // Permute data 'nPermute' times and compute test statistic in each case
    PermutationTestSeeded pt(S, r1, r2, Z, method, shuffles);
    parallelReduce(0, nPermute, pt);
    return pt.pval;
}

/****************************************************************************/
