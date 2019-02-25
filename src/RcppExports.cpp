// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// MC_C_CW_IRSL
List MC_C_CW_IRSL(arma::vec times, int N_e, arma::vec r, double rho, double A);
RcppExport SEXP _RLumCarlo_MC_C_CW_IRSL(SEXP timesSEXP, SEXP N_eSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_CW_IRSL(times, N_e, r, rho, A));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_CW_IRSL_DELOC
List MC_C_CW_IRSL_DELOC(arma::vec times, int N_e, int n_filled, double R, double A);
RcppExport SEXP _RLumCarlo_MC_C_CW_IRSL_DELOC(SEXP timesSEXP, SEXP N_eSEXP, SEXP n_filledSEXP, SEXP RSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_CW_IRSL_DELOC(times, N_e, n_filled, R, A));
    return rcpp_result_gen;
END_RCPP
}
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
// MC_C_ISO
List MC_C_ISO(arma::vec times, int N_e, arma::vec r, double rho, double E, double s, double T);
RcppExport SEXP _RLumCarlo_MC_C_ISO(SEXP timesSEXP, SEXP N_eSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP ESEXP, SEXP sSEXP, SEXP TSEXP) {
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
    rcpp_result_gen = Rcpp::wrap(MC_C_ISO(times, N_e, r, rho, E, s, T));
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
// MC_C_LM_OSL
List MC_C_LM_OSL(arma::vec times, int N_e, arma::vec r, double rho, double A);
RcppExport SEXP _RLumCarlo_MC_C_LM_OSL(SEXP timesSEXP, SEXP N_eSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_LM_OSL(times, N_e, r, rho, A));
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
// MC_C_TL
List MC_C_TL(arma::vec times, int N_e, arma::vec r, double rho, double E, double s);
RcppExport SEXP _RLumCarlo_MC_C_TL(SEXP timesSEXP, SEXP N_eSEXP, SEXP rSEXP, SEXP rhoSEXP, SEXP ESEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_TL(times, N_e, r, rho, E, s));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_TL_DELOC
List MC_C_TL_DELOC(arma::vec times, int N_e, int n_filled, double R, double E, double s);
RcppExport SEXP _RLumCarlo_MC_C_TL_DELOC(SEXP timesSEXP, SEXP N_eSEXP, SEXP n_filledSEXP, SEXP RSEXP, SEXP ESEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type N_e(N_eSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_TL_DELOC(times, N_e, n_filled, R, E, s));
    return rcpp_result_gen;
END_RCPP
}
// MC_C_TL_LOC
List MC_C_TL_LOC(arma::vec times, int n_filled, double r, double E, double s);
RcppExport SEXP _RLumCarlo_MC_C_TL_LOC(SEXP timesSEXP, SEXP n_filledSEXP, SEXP rSEXP, SEXP ESEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type times(timesSEXP);
    Rcpp::traits::input_parameter< int >::type n_filled(n_filledSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(MC_C_TL_LOC(times, n_filled, r, E, s));
    return rcpp_result_gen;
END_RCPP
}
