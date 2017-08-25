\encoding{utf8}
\name{climbing_birds}
\alias{climbing_birds}
\docType{data}
\title{
Climbing birds
}
\description{
Data extracted from \cite{Hedenström & Alerstam 1992}.
}
\usage{data("climbing_birds")}
\format{
  A data frame with 15 observations on the following 11 variables.
  \describe{
    \item{\code{number}}{a numeric vector}
    \item{\code{name}}{a character vector}
    \item{\code{name.scientific}}{a character vector}
    \item{\code{massEmpty}}{a numeric vector}
    \item{\code{massFat}}{a numeric vector}
    \item{\code{wingSpan}}{a numeric vector}
    \item{\code{wingAspect}}{a numeric vector}
    \item{\code{wingbeatFrequency}}{a numeric vector}
    \item{\code{climbRate}}{a numeric vector}
    \item{\code{climbSpeed}}{a numeric vector}
    \item{\code{climbAlitude}}{a numeric vector}
  }
}
\source{
Hedenström A., Alerstam, T. (1992) Climbing performance of migrating birds as a basis for estimating limits for fuel-carrying capacity and muscle work. \emph{J. Exp. Biol} \bold{164} 19-38 \url{http://jeb.biologists.org/content/164/1/19}
}
\examples{
data(climbing_birds)
climbingBirds <- Bird(climbing_birds)
}
\keyword{datasets}
