\encoding{utf8}
\name{computeBodyFrontalArea}
\alias{computeBodyFrontalArea}
\title{Body frontal area from scaling relation}
\description{Body frontal area is a parameter that relates to body drag. This function estimates body frontal area based on empirical scaling relations with mass. }
\usage{
computeBodyFrontalArea(massEmpty, type = "other")
}
\arguments{
  \item{massEmpty}{empty body mass (in kg)}
  \item{type}{type of bird; available options are: \dQuote{passerine} and \dQuote{other})}
}
\details{
Passerine (\cite{Hedenström and Rosén 2003}): \eqn{S_b = 0.0129 m^{0.614}}{Sb = 0.0129 * m^0.614}

Other (\cite{Pennycuick \emph{et al.} 1988}): \eqn{S_b = 0.00813 m^{0.666}}{Sb = 0.00813 * m^0.666}
}
\value{
Numeric value for the body frontal area.
}
\references{
Pennycuick, C. J., Obrecht III, H. H. and Fuller, M. R. (1988) Empirical estimates of body drag of large waterfowl and raptors. \emph{J. Exp. Biol.} \bold{135}, 253--264.

Hedenström, A. and Rosén, M. (2003) Body frontal area in passerine birds. \emph{J. Avian Biol.} \bold{34}, 159--162.

}
\author{
Marco Klein Heerenbrink
}
\note{
Body frontal area is used for the computation of body drag. Only use this value if it matches the used definition of the body drag coefficient.
}
\seealso{
\code{\link{Bird}}
}
\examples{
massEmpty <- 0.215 # kg
Sb <- computeBodyFrontalArea(massEmpty)
print(Sb)
#   [1] 0.002920751 # m2

massEmpty <- 0.215 # kg
birdType <- "passerine" #
Sb <- computeBodyFrontalArea(massEmpty,birdType)
print(Sb)
#   [1] 0.005020037 # m2
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
