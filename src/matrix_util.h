// Utility functions for dealing with Rcpp matrices

#ifndef MATRIXUTIL_H
#define MATRIXUTIL_H

#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// Row bind two matrices
//
// @param M1 first matrix with dimensions r1 x c
// @param M2 second matrix with dimensions r2 x c
// @return the input matrices row-binded
template<typename MatrixT>
MatrixT rbind(const MatrixT M1,
              const MatrixT M2) {
    std::size_t r1 = M1.nrow();
    std::size_t r2 = M2.nrow();

    MatrixT S(r1 + r2, M1.ncol());
    for (std::size_t i = 0; i < r1; ++i) {
        S(i, _) = M1(i, _);
    }
    for (std::size_t i = 0; i < r2; ++i) {
        S(i + r1, _) = M2(i, _);
    }
    return S;
}

// Extract a row of a numeric matrix as a std::vector
//
// @param M input matrix
// @param row index
// @return the specified row represented as a std::vector
template<typename MatrixT>
std::vector<double> getRow(const MatrixT M,
                           std::size_t row) {
    std::size_t ncol = M.ncol();
    std::vector<double> v(ncol);
    for (std::size_t i = 0; i < ncol; ++i) {
        v[i] = M(row, i);
    }
    return v;
}

#endif //MATRIXUTIL_H
