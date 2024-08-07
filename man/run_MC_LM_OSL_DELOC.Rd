% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_MC_LM_OSL_DELOC.R
\encoding{UTF-8}
\name{run_MC_LM_OSL_DELOC}
\alias{run_MC_LM_OSL_DELOC}
\title{Run Monte-Carlo Simulation for LM-OSL (delocalized transitions)}
\usage{
run_MC_LM_OSL_DELOC(
  A,
  times,
  clusters = 10,
  N_e = 200,
  n_filled = N_e,
  R,
  method = "par",
  output = "signal",
  ...
)
}
\arguments{
\item{A}{\link{numeric} (\strong{required}): The optical excitation rate from trap to conduction band (s^-1)}

\item{times}{\link{numeric} (\strong{required}): The sequence of time steps within the simulation (s)}

\item{clusters}{\link{numeric} (\emph{with default}): The number of created clusters for the MC runs. The input can be the output of \link{create_ClusterSystem}. In that case \code{n_filled} indicate absolute numbers of a system.}

\item{N_e}{\link{integer} (\emph{with default}): The total number of electron traps available (dimensionless). Can be a vector of \code{length(clusters)}, shorter values are recycled.}

\item{n_filled}{\link{integer} (\emph{with default}): The number of filled electron traps at the beginning
of the simulation (dimensionless). Can be a vector of \code{length(clusters)}, shorter values are recycled.}

\item{R}{\link{numeric} (\strong{required}): The retrapping ratio for delocalized transitions}

\item{method}{\link{character} (\emph{with default}): Sequential \code{'seq'} or parallel \code{'par'}processing. In
the parallel mode the function tries to run the simulation on multiple CPU cores (if available) with
a positive effect on the computation time.}

\item{output}{\link{character} (\emph{with default}): output is either the \code{'signal'} (the default)
or \code{'remaining_e'} (the remaining charges/electrons in the trap)}

\item{\dots}{further arguments, such as \code{cores} to control the number of used CPU cores or \code{verbose} to silence the terminal}
}
\value{
This function returns an object of class \code{RLumCarlo_Model_Output} which
is a \link{list} consisting of an \link{array} with dimension length(times) x clusters
and a \link{numeric} time vector.
}
\description{
Runs a Monte-Carlo (MC) simulation of linearly modulated optically stimulated
luminescence (LM-OSL) using the one trap one recombination centre (OTOR) model.
Delocalised refers to involvement of the conduction band.
}
\details{
\strong{The model}

\deqn{
I_{DELOC}(t) = -dn/dt = A * t/P * (n^2 / (N*R + n(1-R)))
}

Where in the function: \cr
t := time (s) \cr
A := the optical excitation rate from trap to conduction band (1/s)\cr
n := \code{n_filled}, the instantaneous number of electrons \cr
R :=  the retrapping ratio for delocalized transitions \cr
N := \code{N_e}, the total number of electron traps available (dimensionless) \cr
P := total stimulation time (s)
}
\section{Function version}{
 0.1.0
}

\examples{
run_MC_LM_OSL_DELOC(
 A = 0.12,
 R = 0.1,
 times = 0:50,
 method = "seq",
 clusters = 10) \%>\%
plot_RLumCarlo(legend = TRUE)

} 

\section{How to cite}{
Kreutzer, S., 2024. run_MC_LM_OSL_DELOC(): Run Monte-Carlo Simulation for LM-OSL (delocalized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2024. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.10.9000-5. https://CRAN.R-project.org/package=RLumCarlo
}

\references{
Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S.,
Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution of
defects: A new Monte Carlo simulation approach for feldspar.
Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}

\strong{Further reading}

Chen, R., McKeever, S.W.S., 1997. Theory of Thermoluminescence and Related Phenomena.
WORLD SCIENTIFIC. \doi{10.1142/2781}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
}
\keyword{data}
\keyword{models}
