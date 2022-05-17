// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ffTestStatistic
NumericVector ffTestStatistic(const NumericMatrix S1, const NumericMatrix S2);
RcppExport SEXP _fasano_franceschini_test_ffTestStatistic(SEXP S1SEXP, SEXP S2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type S1(S1SEXP);
    Rcpp::traits::input_parameter< const NumericMatrix >::type S2(S2SEXP);
    rcpp_result_gen = Rcpp::wrap(ffTestStatistic(S1, S2));
    return rcpp_result_gen;
END_RCPP
}
// permutationTestSeeded
unsigned int permutationTestSeeded(const NumericMatrix S1, const NumericMatrix S2, int nPermute, bool verbose, int seed);
RcppExport SEXP _fasano_franceschini_test_permutationTestSeeded(SEXP S1SEXP, SEXP S2SEXP, SEXP nPermuteSEXP, SEXP verboseSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type S1(S1SEXP);
    Rcpp::traits::input_parameter< const NumericMatrix >::type S2(S2SEXP);
    Rcpp::traits::input_parameter< int >::type nPermute(nPermuteSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(permutationTestSeeded(S1, S2, nPermute, verbose, seed));
    return rcpp_result_gen;
END_RCPP
}
// permutationTest
unsigned int permutationTest(const NumericMatrix S1, const NumericMatrix S2, int nPermute, bool verbose);
RcppExport SEXP _fasano_franceschini_test_permutationTest(SEXP S1SEXP, SEXP S2SEXP, SEXP nPermuteSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type S1(S1SEXP);
    Rcpp::traits::input_parameter< const NumericMatrix >::type S2(S2SEXP);
    Rcpp::traits::input_parameter< int >::type nPermute(nPermuteSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(permutationTest(S1, S2, nPermute, verbose));
    return rcpp_result_gen;
END_RCPP
}
// permutationTestParallel
unsigned int permutationTestParallel(const NumericMatrix S1, const NumericMatrix S2, int nPermute);
RcppExport SEXP _fasano_franceschini_test_permutationTestParallel(SEXP S1SEXP, SEXP S2SEXP, SEXP nPermuteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type S1(S1SEXP);
    Rcpp::traits::input_parameter< const NumericMatrix >::type S2(S2SEXP);
    Rcpp::traits::input_parameter< int >::type nPermute(nPermuteSEXP);
    rcpp_result_gen = Rcpp::wrap(permutationTestParallel(S1, S2, nPermute));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fasano_franceschini_test_ffTestStatistic", (DL_FUNC) &_fasano_franceschini_test_ffTestStatistic, 2},
    {"_fasano_franceschini_test_permutationTestSeeded", (DL_FUNC) &_fasano_franceschini_test_permutationTestSeeded, 5},
    {"_fasano_franceschini_test_permutationTest", (DL_FUNC) &_fasano_franceschini_test_permutationTest, 4},
    {"_fasano_franceschini_test_permutationTestParallel", (DL_FUNC) &_fasano_franceschini_test_permutationTestParallel, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_fasano_franceschini_test(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}