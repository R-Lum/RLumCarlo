// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   MC_C_ISO_LOC.cpp (ITL localised transiation)
// Author:  Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom), based on
// code by Johannes Friedrich and equations provided by Vasilis Pagonis
// Contact: sebastian.kreutzer@aber.ac.uk
// Date:    Sun Feb 24 14:59:39 2019
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "util.h"
using namespace Rcpp;

// [[Rcpp::export("MC_C_ISO_LOC")]]
List MC_C_ISO_LOC(arma::vec times, int n_filled, double r, double E, double s, double T) {
  //n >> n_filled: concentration of filled traps [cm^-3]
  //t >> times: refers basically to the temperature
  //E: energy of the trap [eV]
  //s: frequency factor [1/s]
  //r: detrapping ratio [cm^-3]
  //T: temperature [deg. C]

   // set Boltzmann's constant
  double k_B = 8.617*pow(10.0,-5.0);

  //determine delta_t which allows to have delta t != 1
  double delta_t = calc_deltat(times);

  // set output matrices
  NumericMatrix signal (times.size(), 1);
  NumericMatrix remaining_e (times.size(), 1);
  NumericVector r_num;

    //this is p(t)
    double P =  s * exp(-E/(k_B * (273 + T)));

    //t-loop, means run over time
    for(std::size_t t = 0; t < times.size(); ++t){

          //n_filled; decide whether and electron will be excitated
          for(int j = 0; j < n_filled; ++j){

            //draw random number
            r_num = runif(1);

            if (r_num[0] < P * delta_t * (n_filled / (r + n_filled)))
              n_filled = n_filled - 1;

            if (n_filled == 0)
              break;

          } // end n_filled

          //calculate signal and remaining filled
          signal(t,0) = P * (pow(n_filled, 2) / (r + n_filled));
          remaining_e(t,0) = n_filled;

          if (n_filled == 0)
            break;

        } // end t-loop

    return(Rcpp::List::create(Named("signal") = signal,
                              Named("remaining_e") = remaining_e));
}
