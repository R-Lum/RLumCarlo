// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   MC_C_TL_DELOC.cpp
// Author:  Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom), based on
// code by Johannes Friedrich and equations provided by Vasilis Pagonis
// Contact: sebastian.kreutzer@aber.ac.uk
// Date:    Sun Feb 24 14:59:39 2019
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export("MC_C_TL_DELOC")]]
List MC_C_TL_DELOC(arma::vec times, int N_e, int n_filled, double R, double E, double s) {
  //N >> N_e total: concentration of traps [cm^-3]
  //n >> n_filled: concentration of filled traps [cm^-3]
  //t >> times: refers basically to the temperature
  //E: energy of the trap [eV]
  //s: frequency factor [1/s]
  //R: capture coefficient

   // set Boltzmann's constant
  double k_B = 8.617*pow(10.0,-5.0);

  // set output matrices
  NumericMatrix signal (times.size(), 1);
  NumericMatrix remaining_e (times.size(), 1);
  NumericVector r_num;

    //t-loop, means run over time/temperature
    for(std::size_t t = 0; t < times.size(); ++t){

          //this is out p(t)
          double P =  s * exp(-E/(k_B * (273 + times[t])));

          //n_filled; decide whether and electron will be excitated
          for(int j = 0; j < n_filled; ++j){

            //draw random number
            r_num = runif(1);

            if (r_num[0] < P * (n_filled / (N_e * R + n_filled * (1 - R))))
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
