// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   MC_C_CW_IRSL_DELOC.cpp
// Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), based on
// code by Johannes Friedrich and equations provided by Vasilis Pagonis
// Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// Date:    Sun Feb 24 14:59:39 2019
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export("MC_C_CW_IRSL_DELOC")]]
List MC_C_CW_IRSL_DELOC(arma::vec times, int N_e, int n_filled, double R, double A) {
  //N >> N_e total: concentration of traps [cm^-3]
  //n >> n_filled: concentration of filled traps [cm^-3]
  //t >> times: refers basically to the temperature
  //A detrapping parameter [s^-1]
  //R: capture coefficient

  // set output matrices
  NumericMatrix signal (times.size(), 1);
  NumericMatrix remaining_e (times.size(), 1);
  NumericVector r_num;

  //this is out p(t) ... use A here to keep things consistent
  double P =  A;

    //t-loop, means run over time/temperature
    for(std::size_t t = 0; t < times.size(); ++t){

          //n_filled; decide whether and electron will be excitated
          for(int j = 0; j < n_filled; ++j){

            //draw random number
            r_num = runif(1);

            if (r_num[0] < n_filled / (N_e * R + n_filled * (1 - R)))
              n_filled = n_filled - 1;

            if (n_filled == 0)
              break;

          } // end n_filled

          //calculate signal and remaining filled
          signal(t,0) = P * (pow(n_filled,2) / (N_e * R + n_filled * (1 - R)));
          remaining_e(t,0) = n_filled;

          if (n_filled == 0)
            break;

        } // end t-loop

    return(Rcpp::List::create(Named("signal") = signal,
                              Named("remaining_e") = remaining_e));
}
