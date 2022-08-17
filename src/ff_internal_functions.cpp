#include <Rcpp.h>
#include <RcppParallel.h>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>
#include <numeric>
#include "RangeTree.h"
#include "matrix_util.h"
#include "range_count.h"
#include "ProgressBar.h"

using namespace Rcpp;

// Helper function for maximizing difference statistics
double propDiff(const std::vector<double>& c1,
                const std::vector<double>& c2,
                std::size_t n1,
                std::size_t n2) {
    double d = -1;
    for (std::size_t i = 0; i < c1.size(); ++i) {
        d = std::max(d, abs(c1[i]/n1 - c2[i]/n2));
    }
    return d;
}

// Compute FF test statistics
//
// @param M a matrix of type NumericMatrix or RMatrix<double>
// @param r1 number of rows in first sample
// @param r2 number of rows in second sample
// @param shuffle whether to shuffle the samples
// @param prng a pseudorandom number generator (unused if shuffle == false)
// @param method what method to use to compute the test statistic
// @return FF test statistics
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT M,
                                   std::size_t r1,
                                   std::size_t r2,
                                   bool shuffle,
                                   std::mt19937& prng,
                                   char method) {
    double n1 = static_cast<double>(r1);
    double n2 = static_cast<double>(r2);

    // Build range trees
    std::vector<std::size_t> s(r1 + r2);
    std::iota(s.begin(), s.end(), 0);
    if (shuffle) {
        std::shuffle(s.begin(), s.end(), prng);
    }

    double d1 = -1;
    double d2 = -1;
    if (method == 'r') {
        /* Use range tree method */
        // Build range trees
        std::vector<RTree> trees = buildRangeTrees<MatrixT>(M, r1, r2, s);

        // Compute test statistic using first sample as origins
        for (std::size_t i = 0; i < r1; ++i) {
            std::vector<double> org = getRow<MatrixT>(M, s[i]);
            d1 = std::max(d1, propDiff(rangeCountTree(trees[0], org),
                                       rangeCountTree(trees[1], org),
                                       n1, n2));
        }

        // Compute test statistic using second sample as origins
        for (std::size_t i = 0; i < r2; ++i) {
            std::vector<double> org = getRow<MatrixT>(M, s[i+r1]);
            d2 = std::max(d2, propDiff(rangeCountTree(trees[0], org),
                                       rangeCountTree(trees[1], org),
                                       n1, n2));
        }
    } else {
        /* Use brute force method */
        // Compute test statistic using first sample as origins
        for (std::size_t i = 0; i < r1; ++i) {
            std::vector<double> org = getRow<MatrixT>(M, s[i]);
            d1 = std::max(d1, propDiff(rangeCountBrute(M, r1, 0, s, org),
                                       rangeCountBrute(M, r2, r1, s, org),
                                       n1, n2));
        }

        // Compute test statistic using second sample as origins
        for (std::size_t i = 0; i < r2; ++i) {
            std::vector<double> org = getRow<MatrixT>(M, s[i+r1]);
            d2 = std::max(d2, propDiff(rangeCountBrute(M, r1, 0, s, org),
                                       rangeCountBrute(M, r2, r1, s, org),
                                       n1, n2));
        }
    }
    return {d1, d2, sqrt(n1*n2/(n1+n2)) * (d1+d2)/2.0};
}

// Simplified wrapper for testStatistics without shuffling
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT M,
                                   std::size_t r1,
                                   std::size_t r2,
                                   char method) {
    // prng is unused, just a place holder
    std::mt19937 prng;
    return testStatistics<MatrixT>(M, r1, r2, false, prng, method);
}

// Simplified wrapper for testStatistics with shuffling
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT M,
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
NumericVector ffTestStatistic(const NumericMatrix S1,
                              const NumericMatrix S2,
                              char method) {
    return wrap(testStatistics<NumericMatrix>(rbind(S1, S2), S1.nrow(), S2.nrow(), method));
}

// Perform serial permutation test to compute empirical p-value
//
// @param S1 first sample
// @param S2 second sample
// @param nPermute number of iterations to perform
// @param verbose whether to display a progress bar
// @param prng a pseudorandom number generator for permuting the samples
// @param method what method to use to compute the test statistic
// @return empirical p-value
unsigned int permutationTest(const NumericMatrix S1,
                             const NumericMatrix S2,
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
        pval += (z > Z) ? 1 : 0;
        p.step();
    }
    p.finalize();
    return pval;
}

// Seeded version of permutationTest
// [[Rcpp::export(permutationTestSeeded)]]
unsigned int permutationTestSeeded(const NumericMatrix S1,
                                   const NumericMatrix S2,
                                   int nPermute,
                                   bool verbose,
                                   int seed,
                                   char method) {
    std::mt19937 prng(seed);
    return permutationTest(S1, S2, nPermute, verbose, prng, method);
}

// Unseeded version of permutationTest
// [[Rcpp::export(permutationTest)]]
unsigned int permutationTest(const NumericMatrix S1,
                             const NumericMatrix S2,
                             int nPermute,
                             bool verbose,
                             char method) {
    std::mt19937 prng(std::random_device{}());
    return permutationTest(S1, S2, nPermute, verbose, prng, method);
}

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
    PermutationTest(const NumericMatrix S, std::size_t r1, std::size_t r2, double Z, char method) :
        S(S), r1(r1), r2(r2), Z(Z), method(method), pval(0) {}
    PermutationTest(const PermutationTest& p, RcppParallel::Split) :
        S(p.S), r1(p.r1), r2(p.r2), Z(p.Z), method(p.method), pval(0) {}

    // Call operator, compute test statistic for (end-begin) permuted samples
    void operator()(std::size_t begin, std::size_t end) {
        std::mt19937 prng(std::random_device{}());
        for (std::size_t i = begin; i < end; ++i) {
            double z = testStatistics<RcppParallel::RMatrix<double> >(S, r1, r2, prng, method)[2];
            pval += (z > Z);
        }
    }

    void join(const PermutationTest& rhs) {
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
unsigned int permutationTestParallel(const NumericMatrix S1,
                                     const NumericMatrix S2,
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
