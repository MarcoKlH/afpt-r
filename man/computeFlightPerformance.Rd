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
\dontrun{ # computationally intensive

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
performance.myBird <- computeFlightPerformance(myBird)
performance.myBird
# Name: Jackdaw
# Sc. name: Corvus monedula
# Bird definitions: NA
#               speed power.aero power.chem strokeplane amplitude
# minimumSpeed  2.706      5.234      27.29        49.9      51.3
# minimumPower  8.031      2.093      12.27        28.1      34.5
# maximumRange 11.025      2.523      14.33        18.2      36.7
# maximumSpeed 16.590      5.235      27.29         6.8      50.2
# Maximum climb performance:
#                  speed power.aero power.chem strokeplane amplitude climbRate
# maximumClimbRate  8.89      5.234      27.29        24.5      53.9      1.18
# Minimized migration time:
#                  speed speed.migration power.aero power.chem power.dep strokeplane amplitude
# minimumTimeSpeed 11.75           1.962      2.741      15.37     3.081       16.49     38.04

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
# maximumClimbRate  8.89      5.234      27.29        24.5      53.9      1.18
# Minimized migration time:
#                  speed speed.migration power.aero power.chem power.dep strokeplane amplitude
# minimumTimeSpeed 12.07           1.905      2.964      16.43     3.081       14.13     45.13

## plot variation of speed, power and flapping kinematics
plot(performance.myBird$powercurve[c('speed','power.aero','strokeplane','frequency','amplitude')])

## plot power factors
plot(performance.myBird$powercurve[c('speed','power.aero')])
plot(performance.myBird$powercurve[c('speed','kP.ind')])
plot(performance.myBird$powercurve[c('speed','kP.pro0')])
plot(performance.myBird$powercurve[c('speed','kP.pro2')])
} # end dontrun

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
