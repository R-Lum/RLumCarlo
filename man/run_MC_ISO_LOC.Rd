% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_MC_ISO_LOC.R
\encoding{UTF-8}
\name{run_MC_ISO_LOC}
\alias{run_MC_ISO_LOC}
\title{Run Monte-Carlo simulation for ISO-TL (localized transitions)}
\usage{
run_MC_ISO_LOC(
  s,
  E,
  T = 20,
  times,
  clusters = 10,
  n_filled = 100,
  r,
  method = "par",
  output = "signal",
  ...
)
}
\arguments{
\item{s}{\link{numeric} (\strong{required}): The frequency factor of the trap (s^-1)}

\item{E}{\link{numeric} (\strong{required}): Thermal activation energy of the trap (eV)}

\item{T}{\link{numeric} (\emph{with default}): Constant stimulation temperature (°C)}

\item{times}{\link{numeric} (\emph{with default}): The sequence of time steps within the
simulation (s)}

\item{clusters}{\link{numeric} (\emph{with default}): The number of created clusters for the MC runs. The input can be the output of \link{create_ClusterSystem}. In that case \code{n_filled} indicate absolute numbers of a system.}

\item{n_filled}{\link{integer} (\emph{with default}): The number of filled electron traps at the
beginning of the simulation (dimensionless). Can be a vector of \code{length(clusters)}, shorter values are recycled.}

\item{r}{\link{numeric} (\strong{required}): The retrapping ratio for localized transitions.}

\item{method}{\link{character} (\emph{with default}): Sequential \code{'seq'} or parallel \code{'par'}processing. In
the parallel mode the function tries to run the simulation on multiple CPU cores (if available) with
a positive effect on the computation time.}

\item{output}{\link{character} (\emph{with default}): output is either the \code{'signal'} (the default) or
\code{'remaining_e'} (the remaining charges/electrons in the trap)}

\item{\dots}{further arguments, such as \code{cores} to control the number of used CPU cores or \code{verbose} to silence the terminal}
}
\value{
This function returns an object of class \code{RLumCarlo_Model_Output} which
is a \link{list} consisting of an \link{array} with dimension length(times) x clusters
and a \link{numeric} time vector.
}
\description{
Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminescence
(ISO-TL or ITL) using the generalized one trap (GOT) model. Localized transitions refer to transitions
which do no involve the conduction or valence band. These transitions take place between the
ground state and an excited state of the trapped charge, and also involve an energy state of the
recombination centre.
}
\details{
\strong{The model}

\deqn{
I_{LOC}(t) = -dn/dt = (s * exp(-E/(k_{B} * T_{ISO}))) * (n^2 / (r + n)))
}

Where in the function: \cr
t := time (s) \cr
\eqn{k_{B}} := Boltzmann constant (8.617 x 10^-5 eV K^-1)\cr
\eqn{T_{ISO}} := isothermal temperature (°C) \cr
n := \code{n_filled} \cr
s := frequency factor of the trap (1/s) \cr
E := activation energy of the trap (eV) \cr
r := retrapping ratio for localized transitions \cr
}
\section{Function version}{
 0.1.0
}

\examples{
run_MC_ISO_LOC(
 E = 1.45,
 s = 3.5e12,
 T = 200,
 times = 0:100,
 method = 'seq',
 r = 1) \%>\%
plot_RLumCarlo(legend = TRUE)

} 

\section{How to cite}{
Kreutzer, S., 2024. run_MC_ISO_LOC(): Run Monte-Carlo simulation for ISO-TL (localized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2024. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.10.9000-5. https://CRAN.R-project.org/package=RLumCarlo
}

\references{
Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V.,
Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a
random distribution of defects: A new Monte Carlo simulation approach for feldspar.
Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
}
\keyword{data}
\keyword{models}
