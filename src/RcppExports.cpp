// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/RLumCarlo.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// MC_C_CW_IRSL_LOC
List MC_C_CW_IRSL_LOC(arma::vec times, int n_filled, double r, double A);
RcppExport SEXP _RLumCarlo_MC_C_CW_IRSL_LOC(SEXP timesSEXP, SEXP n_filledSEXP, SEXP rSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_CW_IRSL_LOC(times, n_filled, r, A));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_CW_IRSL_TUN
List MC_C_CW_IRSL_TUN(arma::vec times, int N_e, arma::vec r, double rho, double A);
RcppExport SEXP _RLumCarlo_MC_C_CW_IRSL_TUN(SEXP timesSEXP, SEXP N_eSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_CW_IRSL_TUN(times, N_e, r, rho, A));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_CW_OSL_DELOC
List MC_C_CW_OSL_DELOC(arma::vec times, int N_e, int n_filled, double R, double A);
RcppExport SEXP _RLumCarlo_MC_C_CW_OSL_DELOC(SEXP timesSEXP, SEXP N_eSEXP, SEXP n_filledSEXP, SEXP RSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_CW_OSL_DELOC(times, N_e, n_filled, R, A));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_ISO_DELOC
List MC_C_ISO_DELOC(arma::vec times, int N_e, int n_filled, double R, double E, double s, double T);
RcppExport SEXP _RLumCarlo_MC_C_ISO_DELOC(SEXP timesSEXP, SEXP N_eSEXP, SEXP n_filledSEXP, SEXP RSEXP, SEXP ESEXP, SEXP sSEXP, SEXP TSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_ISO_DELOC(times, N_e, n_filled, R, E, s, T));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_ISO_LOC
List MC_C_ISO_LOC(arma::vec times, int n_filled, double r, double E, double s, double T);
RcppExport SEXP _RLumCarlo_MC_C_ISO_LOC(SEXP timesSEXP, SEXP n_filledSEXP, SEXP rSEXP, SEXP ESEXP, SEXP sSEXP, SEXP TSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_ISO_LOC(times, n_filled, r, E, s, T));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_ISO_TUN
List MC_C_ISO_TUN(arma::vec times, int N_e, arma::vec r, double rho, double E, double s, double T);
RcppExport SEXP _RLumCarlo_MC_C_ISO_TUN(SEXP timesSEXP, SEXP N_eSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP ESEXP, SEXP sSEXP, SEXP TSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_ISO_TUN(times, N_e, r, rho, E, s, T));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_LM_OSL_DELOC
List MC_C_LM_OSL_DELOC(arma::vec times, int N_e, int n_filled, double R, double A);
RcppExport SEXP _RLumCarlo_MC_C_LM_OSL_DELOC(SEXP timesSEXP, SEXP N_eSEXP, SEXP n_filledSEXP, SEXP RSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_LM_OSL_DELOC(times, N_e, n_filled, R, A));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_LM_OSL_LOC
List MC_C_LM_OSL_LOC(arma::vec times, int n_filled, double r, double A);
RcppExport SEXP _RLumCarlo_MC_C_LM_OSL_LOC(SEXP timesSEXP, SEXP n_filledSEXP, SEXP rSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_LM_OSL_LOC(times, n_filled, r, A));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_LM_OSL_TUN
List MC_C_LM_OSL_TUN(arma::vec times, int N_e, arma::vec r, double rho, double A);
RcppExport SEXP _RLumCarlo_MC_C_LM_OSL_TUN(SEXP timesSEXP, SEXP N_eSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_LM_OSL_TUN(times, N_e, r, rho, A));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_TL_DELOC
List MC_C_TL_DELOC(arma::vec times, int N_e, int n_filled, double R, double E, double s, double b);
RcppExport SEXP _RLumCarlo_MC_C_TL_DELOC(SEXP timesSEXP, SEXP N_eSEXP, SEXP n_filledSEXP, SEXP RSEXP, SEXP ESEXP, SEXP sSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_TL_DELOC(times, N_e, n_filled, R, E, s, b));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_TL_LOC
List MC_C_TL_LOC(arma::vec times, int n_filled, double r, double E, double s, double b);
RcppExport SEXP _RLumCarlo_MC_C_TL_LOC(SEXP timesSEXP, SEXP n_filledSEXP, SEXP rSEXP, SEXP ESEXP, SEXP sSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_TL_LOC(times, n_filled, r, E, s, b));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_TL_TUN
List MC_C_TL_TUN(arma::vec times, int N_e, arma::vec r, double rho, double E, double s, double b);
RcppExport SEXP _RLumCarlo_MC_C_TL_TUN(SEXP timesSEXP, SEXP N_eSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP ESEXP, SEXP sSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_TL_TUN(times, N_e, r, rho, E, s, b));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RLumCarlo_MC_C_CW_IRSL_LOC", (DL_FUNC) &_RLumCarlo_MC_C_CW_IRSL_LOC, 4},
    {"_RLumCarlo_MC_C_CW_IRSL_TUN", (DL_FUNC) &_RLumCarlo_MC_C_CW_IRSL_TUN, 5},
    {"_RLumCarlo_MC_C_CW_OSL_DELOC", (DL_FUNC) &_RLumCarlo_MC_C_CW_OSL_DELOC, 5},
    {"_RLumCarlo_MC_C_ISO_DELOC", (DL_FUNC) &_RLumCarlo_MC_C_ISO_DELOC, 7},
    {"_RLumCarlo_MC_C_ISO_LOC", (DL_FUNC) &_RLumCarlo_MC_C_ISO_LOC, 6},
    {"_RLumCarlo_MC_C_ISO_TUN", (DL_FUNC) &_RLumCarlo_MC_C_ISO_TUN, 7},
    {"_RLumCarlo_MC_C_LM_OSL_DELOC", (DL_FUNC) &_RLumCarlo_MC_C_LM_OSL_DELOC, 5},
    {"_RLumCarlo_MC_C_LM_OSL_LOC", (DL_FUNC) &_RLumCarlo_MC_C_LM_OSL_LOC, 4},
    {"_RLumCarlo_MC_C_LM_OSL_TUN", (DL_FUNC) &_RLumCarlo_MC_C_LM_OSL_TUN, 5},
    {"_RLumCarlo_MC_C_TL_DELOC", (DL_FUNC) &_RLumCarlo_MC_C_TL_DELOC, 7},
    {"_RLumCarlo_MC_C_TL_LOC", (DL_FUNC) &_RLumCarlo_MC_C_TL_LOC, 6},
    {"_RLumCarlo_MC_C_TL_TUN", (DL_FUNC) &_RLumCarlo_MC_C_TL_TUN, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_RLumCarlo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
