// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export("MC_C_ISO")]]
List MC_C_ISO(arma::vec times, int N_e, arma::vec r, double rho, double E, double s, double T) {

  NumericMatrix signal (times.size(), r.size());
  NumericMatrix remaining_e (times.size(), r.size());

  // set Boltzmann's constant
  double k_B = 8.617*pow(10.0,-5.0);

    for(std::size_t k = 0; k < r.size(); ++k){

      std::size_t n_filled = N_e;

      double P =  (s * exp(-E/(k_B * (273 + T)))) * exp(-(pow(rho,-1.0/3)) * r[k]);

      for(std::size_t t = 0; t < times.size(); ++t){

        for(std::size_t j = 0; j < n_filled; ++j){

          NumericVector r_num = runif(1);

          if (r_num[0] < P)
            n_filled = n_filled - 1;

          if (n_filled == 0)
            break;

        } // end n_filled
        signal(t,k) = n_filled * P * 3 * pow((double)r[k],2) * exp(-(pow(r[k],3)));
        remaining_e(t,k) = n_filled;

        if (n_filled == 0)
          break;

      } // end t-loop

    } // end r-loop

  return(Rcpp::List::create(Named("signal") = signal,
                            Named("remaining_e") = remaining_e));
}

