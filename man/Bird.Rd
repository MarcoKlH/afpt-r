\encoding{utf8}
\name{Bird}
\alias{Bird}
\alias{Bird.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bird description}
\description{
This function creates a bird description object, which is basically just a list with predefined variable names. It is named a bird object, but could also contain a description of a bat or insect. Minimal input required to construct a bird are body mass, wing span and wing area (or wing aspect ratio). Other required variables will then be given default values, or they will be estimated from allometric relations from literature.
}
\usage{
Bird(massTotal, wingSpan, wingArea, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{massTotal}{ Total mass that needs to be lifted in flight in kg }
  \item{wingSpan}{ The maximum distance between the wingtips in meters }
  \item{wingArea}{ The area of the fully stretched wings including the root area (left wing, right wing and area in between the wing roots) }
  \item{\dots}{ Any other properties of a valid bird object (see details) }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
bird object with variables required by the various power estimating functions (e.g. \code{\link{computeFlappingPower}}).
}

\author{Marco Klein Heerenbrink}
\seealso{
\code{\link{computeAvailablePower}}, \code{\link{computeChemicalPower}}, \code{\link{computeFlappingPower}}, \code{\link{computeBodyFrontalArea}}, etc.
}
\examples{
myBird = Bird(
  massTotal = 0.215,
  wingSpan = 0.67,
  wingArea = 0.0652,
  name = 'jackdaw',
  type =  'passerine'
)
print(myBird)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
