#include <Rcpp.h>
#include <RcppParallel.h>
#include <vector>
#include <random>
#include <cmath>
#include "RangeTree.h"
#include "matrix_util.h"
#include "permute.h"
#include "range_count.h"
#include "ProgressBar.h"

using namespace Rcpp;

// Compute FF test statistics
//
// @param M a matrix of type NumericMatrix or RMatrix<double>
// @param r1 number of rows in first sample
// @param r2 number of rows in second sample
// @param shuffle whether to shuffle the samples
// @param prng a pseudorandom number generator (unused if shuffle == false)
// @return FF test statistics
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT M,
                                   std::size_t r1,
                                   std::size_t r2,
                                   bool shuffle,
                                   std::mt19937& prng) {
    double n1 = static_cast<double>(r1);
    double n2 = static_cast<double>(r2);

    // Build range trees
    std::vector<std::size_t> s;
    if (shuffle) {
        s = permutation(r1 + r2, prng);
    } else {
        s = seq(r1 + r2);
    }
    std::vector<RTree> trees = buildRangeTrees<MatrixT>(M, r1, r2, s);

    // Compute test statistic using first sample as origins
    double d1 = -1;
    for (std::size_t i = 0; i < r1; ++i) {
        std::vector<double> org = getRow<MatrixT>(M, s[i]);
        std::vector<double> c1 = rangeCount(trees[0], org);
        std::vector<double> c2 = rangeCount(trees[1], org);
        for (std::size_t j = 0; j < c1.size(); ++j) {
            d1 = std::max(d1, abs(c1[j]/n1 - c2[j]/n2));
        }
    }

    // Compute test statistic using second sample as origins
    double d2 = -1;
    for (std::size_t i = 0; i < r2; ++i) {
        std::vector<double> org = getRow<MatrixT>(M, s[i+r1]);
        std::vector<double> c1 = rangeCount(trees[0], org);
        std::vector<double> c2 = rangeCount(trees[1], org);
        for (std::size_t j = 0; j < c1.size(); ++j) {
            d2 = std::max(d2, abs(c1[j]/n1 - c2[j]/n2));
        }
    }

    return {d1, d2, sqrt(n1*n2/(n1+n2))*(d1+d2)/2.0};
}

// Simplified wrapper for testStatistics without shuffling
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT M,
                                   std::size_t r1,
                                   std::size_t r2) {
    // prng is unnused, just a place holder
    std::mt19937 prng;
    return testStatistics<MatrixT>(M, r1, r2, false, prng);
}

// Simplified wrapper for testStatistics with shuffling
template<typename MatrixT>
std::vector<double> testStatistics(const MatrixT M,
                                   std::size_t r1,
                                   std::size_t r2,
                                   std::mt19937& prng) {
    return testStatistics<MatrixT>(M, r1, r2, true, prng);
}

// Worker for parallel permutation test
struct PermutationTest : public RcppParallel::Worker {
    // Inputs //
    // Pooled samples
    const RcppParallel::RMatrix<double> S;
    // Number of points in first sample
    const std::size_t r1;
    // Number of points in second sample
    const std::size_t r2;
    // Test statistic for original data
    const double Z;

    // Output
    unsigned int pval;

    // Constructors
    PermutationTest(const NumericMatrix S, std::size_t r1, std::size_t r2, double Z) :
        S(S), r1(r1), r2(r2), Z(Z), pval(0) {}
    PermutationTest(const PermutationTest& p, RcppParallel::Split) :
        S(p.S), r1(p.r1), r2(p.r2), Z(p.Z), pval(0) {}

    // Call operator, compute test statistic for (end-begin) permuted samples
    void operator()(std::size_t begin, std::size_t end) {
        std::mt19937 prng(std::random_device{}());
        for (std::size_t i = begin; i < end; ++i) {
            double z = testStatistics<RcppParallel::RMatrix<double> >(S, r1, r2, prng)[2];
            pval += (z > Z) ? 1 : 0;
        }
    }

    void join(const PermutationTest& rhs) {
        pval += rhs.pval;
    }
};

// Compute FF test statistic (callable from R)
//
// @param S1 first sample
// @param S2 second sample
// @return FF test statistics
// [[Rcpp::export(ffTestStatistic)]]
NumericVector ffTestStatistic(const NumericMatrix S1,
                              const NumericMatrix S2) {
    return wrap(testStatistics<NumericMatrix>(rbind(S1, S2), S1.nrow(), S2.nrow()));
}

// Perform serial permutation test to compute empirical p-value
//
// @param S1 first sample
// @param S2 second sample
// @param nPermute number of iterations to perform
// @param verbose whether to display a progress bar
// @param prng a pseudorandom number generator for permuting the samples
// @return empirical p-value
unsigned int permutationTest(const NumericMatrix S1,
                             const NumericMatrix S2,
                             int nPermute,
                             bool verbose,
                             std::mt19937& prng) {
    std::size_t r1 = S1.nrow();
    std::size_t r2 = S2.nrow();
    NumericMatrix S = rbind(S1, S2);

    // Compute test statistic for original data
    double Z = testStatistics<NumericMatrix>(S, r1, r2)[2];

    // Permute data 'nPermute' times and compute test statistic in each case
    unsigned int pval = 0;
    ProgressBar p(nPermute, verbose);
    for (int i = 0; i < nPermute; ++i) {
        double z = testStatistics<NumericMatrix>(S, r1, r2, prng)[2];
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
                                   int seed) {
    std::mt19937 prng(seed);
    return permutationTest(S1, S2, nPermute, verbose, prng);
}

// Unseeded version of permutationTest
// [[Rcpp::export(permutationTest)]]
unsigned int permutationTest(const NumericMatrix S1,
                             const NumericMatrix S2,
                             int nPermute,
                             bool verbose) {
    std::mt19937 prng(std::random_device{}());
    return permutationTest(S1, S2, nPermute, verbose, prng);
}

// Perform parallel permutation test to compute empirical p-value
//
// @param S1 first sample
// @param S2 second sample
// @param nPermute number of iterations to perform
// @return empirical p-value
// [[Rcpp::export(permutationTestParallel)]]
unsigned int permutationTestParallel(const NumericMatrix S1,
                                     const NumericMatrix S2,
                                     int nPermute) {
    std::size_t r1 = S1.nrow();
    std::size_t r2 = S2.nrow();
    NumericMatrix S = rbind(S1, S2);

    // Compute test statistic for original data
    double Z = testStatistics<NumericMatrix>(S, r1, r2)[2];

    // Permute data 'nPermute' times and compute test statistic in each case
    PermutationTest pt(S, r1, r2, Z);
    parallelReduce(0, nPermute, pt);
    return pt.pval;
}
