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
                bool s = j & (1 << (ndim-1-k));
                bool check = s ? (pt[k] < origin[k]) : (pt[k] > origin[k]);
                in_oct = in_oct && check;
            }
            if (in_oct) {
                ++counts[j];
                break;
            }
        }
    }
    return counts;
}
