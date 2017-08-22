\encoding{utf8}
\name{computeFlightPerformance}
\alias{computeFlightPerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute characteristics of a power curve}
\description{
This function calculates the basic characteristic flight speeds for \code{bird}.
}
\usage{
computeFlightPerformance(bird, ..., length.out=10)
}
\arguments{
  \item{bird}{description of the bird or bat, constructed using the \code{\link{Bird}} function}
  \item{\dots}{various optional arguments that are passed on to other functions; see details}
  \item{length.out}{length of calculated power curve; set \code{length.out=0} to not compute a power curve }
}
\details{
Optional arguments can be provided through \code{\dots}. These can be arguments of \code{computeFlappingPower}, e.g. \code{strokeplane}, \code{frequency}, etc., or arguments for \code{findMaximumRangeSpeed}, e.g. \code{windSpeed} and \code{windDir}. The latter will only affect the outcome of the maximum range speed, and should perhaps not be analysed through the current function...
}
\value{
  \item{birdWSName }{ variable name in work-space of the bird object }
  \item{bird }{ bird object }
  \item{table }{ table with characteristic speeds }
  \item{maxClimb }{ table with climb performance }
  \item{powercurve }{ power curve from minimum to maximum speed of length \code{lenght.out} }
}
\references{
Klein Heerenbrink, M., Johansson, L. C. and Hedenström, A. (2015) Power of the wingbeat: modelling the effects of flapping wings in vertebrate flight. \emph{Proc. R. Soc. A} \bold{471}, 2177 \doi{10.1098/rspa.2014.0952}
}
\author{
Marco Klein Heerenbrink
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Bird}}, \code{\link{computeFlappingPower}}
}
\examples{
## Define a bird:
myBird = Bird(
  name = "Jackdaw",
  name.scientific = "Corvus monedula",
  massTotal = 0.215, #  (kg) total body mass
  wingSpan = 0.67, #  (m) maximum wing span
  wingArea = 0.0652, #  (m2) maximum wing area
  type = "passerine"
)

## simplest performance calculation
performance.myBird <- computeFlightPerformance(myBird,strokeplane=0)
performance.myBird
# Name: Jackdaw
# Sc. name: Corvus monedula
# Bird definitions: NA
#               speed power.aero power.chem strokeplane amplitude
# minimumSpeed  3.783      5.228      27.26           0      43.5
# minimumPower  8.227      2.138      12.49           0      31.7
# maximumRange 11.102      2.558      14.49           0      35.1
# maximumSpeed 16.587      5.234      27.29           0      49.6
# Maximum climb performance:
#                  speed power.aero power.chem strokeplane amplitude climbRate
# maximumClimbRate  9.15      5.234      27.29           0      49.4     1.153

\dontrun{ # computationally intensive
## optimize strokeplane angle and use speed dependent frequency
funFrequency = function(U){19.8 - 4.7*U + 0.45*U^2 - 0.0138*U^3}
performance.myBird <- computeFlightPerformance(myBird,strokeplane='opt',frequency=funFrequency)
performance.myBird
# Name: Jackdaw
# Sc. name: Corvus monedula
# Bird definitions: NA
#               speed power.aero power.chem strokeplane amplitude
# minimumSpeed  2.293      5.229      27.27        49.9      43.8
# minimumPower  8.192      2.319      13.35        21.6      42.8
# maximumRange 11.463      2.775      15.53        14.9      44.3
# maximumSpeed 16.088      5.233      27.29         8.3      64.5
# Maximum climb performance:
#                  speed power.aero power.chem strokeplane amplitude climbRate
# maximumClimbRate 9.688      5.234      27.29        17.5      61.7     1.015

## plot variation of speed, power and flapping kinematics
plot(performance.myBird$powercurve[c('speed','power.aero','strokeplane','frequency','amplitude')])
} # end dontrun

## plot power factors
plot(performance.myBird$powercurve[c('speed','power.aero')])
plot(performance.myBird$powercurve[c('speed','kP.ind')])
plot(performance.myBird$powercurve[c('speed','kP.pro0')])
plot(performance.myBird$powercurve[c('speed','kP.pro2')])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
