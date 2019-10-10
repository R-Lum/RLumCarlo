// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export("MC_C_LM_OSL_TUN")]]
List MC_C_LM_OSL_TUN(arma::vec times, int N_e, arma::vec r, double rho, double A) {

  NumericMatrix signal (times.size(), r.size());
  NumericMatrix remaining_e (times.size(), r.size());

    for(std::size_t k = 0; k < r.size(); ++k){

      std::size_t n_filled = N_e;

      for(std::size_t t = 0; t < times.size(); ++t){

        double P =  A * (times[t]/max(times)) * exp(-(pow(rho,-1.0/3)) * r[k]);

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

