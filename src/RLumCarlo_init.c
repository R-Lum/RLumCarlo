/* DO NOT CHANGE MANUALLY! */
/* This file was produced by the function RLumCarlo.BuildScripts/RLumCarlo.PBS_EntryPointRegistration.R */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _RLumCarlo_MC_C_CW_IRSL(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RLumCarlo_MC_C_ISO(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RLumCarlo_MC_C_LM_OSL(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RLumCarlo_MC_C_TL(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_RLumCarlo_MC_C_CW_IRSL", (DL_FUNC) &_RLumCarlo_MC_C_CW_IRSL, 5},
    {"_RLumCarlo_MC_C_ISO",     (DL_FUNC) &_RLumCarlo_MC_C_ISO,     5},
    {"_RLumCarlo_MC_C_LM_OSL",  (DL_FUNC) &_RLumCarlo_MC_C_LM_OSL,  5},
    {"_RLumCarlo_MC_C_TL",      (DL_FUNC) &_RLumCarlo_MC_C_TL,      6},
    {NULL, NULL, 0}
};

void R_init_RLumCarlo(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}