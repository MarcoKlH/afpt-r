\newcommand{\theFunction}{\code{computeFlappingPower}}
\encoding{utf8}
\name{computeFlappingPower}
\alias{computeFlappingPower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate aerodynamic power flapping flight}
\description{The function calculates the aerodynamic power required for the specified bird (or bat) at the specified flight speed.}
\usage{
computeFlappingPower(bird,speed,...,frequency,strokeplane)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bird}{ object describing the relevant morphological parameters of the bird (or bat); this object should be created using the \code{\link{Bird}} constructor.}
  \item{speed}{ a numeric vector of the airspeed. }
  \item{\dots}{ optional arguments (see details) }
  \item{frequency}{ wingbeat frequency as single numeric value, a numeric vector matching the speed vector, or a closure object describing the wingbeat frequency as a function of speed. If not provided, the function will look for a default wingbeat frequency in the bird object. }
  \item{strokeplane}{ angle of the strokeplane in degrees, as a single numeric value, a numeric vector matching the speed vector, a closure object describing the strokeplane angle as a function of speed. Alternatively providing character string "opt" will tell the function to optimize the strokeplane angle for minimum aerodynamic power. }
  }
  \details{
  This function estimates aerodynamic power for a animal in forward flight based on morphology and wingbeat kinematics (\cite{Klein Heerenbrink, 2015}). The model takes into account span reduction during the upstroke, which is typical for vertebrate forward flight.
  \dots
  The minimal input required for the function is a description of the animal (as provided by the Bird constructor) and the speed(range) for which to compute the aerodynamic power. Distinct from other models, this model also requires wingbeat frequency and strokeplane angle. Higher wingbeat frequency tends to lower the induced power, but it may increase profile power. If no wingbeat frequency is provided, the function will use the reference wingbeat frequency from the bird object. Otherwise the user can specify values (either as vectors or as closure object). The user can provide additional optional arguments:
  \describe{
  \item{\code{bodyDragCoefficient}}{  single numeric value, a numeric vector matching the speed vector, or a closure object as a function of speed. If not provided, the function will look for a default value in the bird object.}
  \item{\code{addedDrag}}{  single numeric value or a numeric vector matching the speed vector. This represents additional "drag" (in Newtons) that must be overcome (e.g. during climb). }
  \item{\code{flightcondition}}{ object describing the atmospheric conditions (density, viscosity, gravity). }
  }

  \subsection{Aerodynamic model}{
    \theFunction first computes the drag components for non-flapping flight:
    \deqn{D_\mathrm{ind} = \frac{L^2}{q \pi b^2}}{
          D.ind = L^2 / (q * \pi * b^2)}
    \deqn{D_\mathrm{pro,0} = C_{D_\mathrm{pro,0}} q S}{
          D.pro0 = q * C.Dpro0 * S }
    \deqn{D_\mathrm{pro,2} = k_{p} \frac{L^2}{q S}}{
          D.pro2 = kp * L^2 / (q * S)}
    \deqn{D_\mathrm{par} = C_{D_\mathrm{b}} q S_\mathrm{b} + D_\mathrm{added}}{
          D.par = q * C.Dbody * S.b + D.added }
    which combine to the non-flapping thrust requirement \eqn{T_0 = \sum D_{<>}}{T.0 = sum(D.<>)}. Here \eqn{q = \frac{1}{2}\rho U^2}{q = \rho * U^2 / 2} is the dynamic pressure depending on density (\eqn{\rho}) and speed (\eqn{U}).
    To account for how flapping the wings affects the drag on the wings, \theFunction computes factors \eqn{f_{D_\mathrm{ind}}}{fD.ind}, \eqn{f_{D_\mathrm{pro,0}}}{fD.pro0} and \eqn{f_{D_\mathrm{pro,2}}}{fD.pro2}, which are functions of the strokeplane angle and the (reduced) wingbeat frequency. These factors relate to the returned drag factors \code{kD.ind}, \code{kD.pro0} and \code{kD.pro2} through
    \deqn{k_{D,<>}=1+f_{D,<>} \frac{T}{L}}{kD.<> = 1 + fD.<> * T/L}
    The actual drag in flapping flight is found by multiplying each non-flapping drag component with its respective drag factor. This means that the actual thrust requirement (thrust ratio \eqn{T/L}) can be computed as
    \deqn{\frac{T}{L} = \frac{T_0}{L - f_{D\mathrm{ind}}D_\mathrm{ind} - f_{D\mathrm{pro,0}}D_\mathrm{pro,0} - f_{D\mathrm{pro,2}}D_\mathrm{pro,2} }}{
         T/L = T.0 / (L - fD.ind * D.ind - fD.pro0 * D.pro0 - fD.pro2 * D.pro2)}
    Finally, \theFunction computes the power factors in a similar way to the drag factors (i.e. \eqn{k_{P,i}=1+f_{P,i}\frac{T}{L}}{kP.i = 1 + fP.i * T/L}, with \eqn{f_{P,i}}{fP.i} functions of strokeplane angle and wingbeat frequency).
    The total aerodynamic power is then computed as
    \deqn{P = k_{P\mathrm{ind}} D_\mathrm{ind}U + k_{P\mathrm{pro,0}} D_\mathrm{pro,0}U + k_{P\mathrm{pro,2}} D_\mathrm{pro,2}U + D_\mathrm{par}U}{P = kP.ind * D.ind * U + kP.pro0 * D.pro0 * U + kP.pro2 * D.pro2 * U + D.par * U}
  }
  \subsection{Wingbeat optimization}{
    The underlying numerical model that is represented by functions \eqn{f_{D,i}}{fD.i} and \eqn{f_{P,i}}{fP.i}, has optimised the flapping amplitude for minimum induced power. This means \theFunction implicitly optimizes flapping amplitude, which is the value \code{amplitude} returned in the output.

    \theFunction takes strokeplane angle as input. The underlying numerical model has only explored strokeplane angles over a range of 0 (vertical) to 50 degrees, the latter being defined as having the down-stroke moving forward. In many cases it will be possible to find a strokeplane angle for which the total aerodynamic power is minimal. At high speeds this optimum will be for a vertical strokeplane while at lower speeds it will be more horizontal. By passing \code{strokeplane="opt"} as an argument to \theFunction, it will try to numerically find the optimal strokeplane angle, using the function \code{optimize}.
  }% end subsection
}% end arguments
\value{
  A data.frame including elements
  \item{speed}{ specified speed for which power is computed. }
  \item{power}{ total aerodynamic power. }
  \item{power.chem}{ total chemical power. }
  \item{strokeplane}{ used strokeplane angle (either specified or optimized). }
  \item{amplitude}{ wingbeat amplitude (implicitly optimized for minimum induced power). }
  \item{frequency}{ wingbeat frequency (specified). }
  \item{flags.redFreqLo}{ TRUE if reduced frequency too low (<1; outside model range). }
  \item{flags.redFreqHi}{ TRUE if reduced frequency too high (>6; outside model range). }
  \item{flags.thrustHi}{ TRUE if thrust requirement too high (>0.3; outside model range). }
  \item{flags.speedLo}{ TRUE if speed is too low (invalidating the forward flight assumption). }
  \item{kD.ind}{ induced drag factor }
  \item{kD.pro0}{ zero lift profile drag factor }
  \item{kD.pro2}{ lift dependent profile drag factor }
  \item{kP.ind}{ induced power factor }
  \item{kP.pro0}{ zero lift profile power factor }
  \item{kP.pro2}{ lift dependent profile power factor }
  \item{CDpro0}{ used zero lift profile drag coefficient (laminar boundary layer friction) }
  \item{ReynoldsNumber}{ mean chord Reynolds number }
  \item{Dnf.ind}{ non-flapping induced drag (N) }
  \item{Dnf.pro0}{ non-flapping zero lift profile drag (N) }
  \item{Dnf.pro2}{ non-flapping lift dependent profile drag (N) }
  \item{Dnf.par}{ non-flapping parasitic drag (including body drag and apparent drag due to climbing)}
  \item{L}{ lift (N) }

}
\references{
Klein Heerenbrink, M., Johansson, L. C. and Hedenström, A. (2015) Power of the wingbeat: modelling the effects of flapping wings in vertebrate flight. \emph{Proc. R. Soc. A} \bold{471}, 2177 \doi{10.1098/rspa.2014.0952}
}
\author{Marco Klein Heerenbrink}
\note{
This model aims to predict the optimal flight performance for a bird. Particularly, the induced drag and induced power assume an ideal load distribution over the wing equivalent to the elliptical lift distribution for non-flapping wings. This means that induced power will typically be underestimated.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Bird}}, \code{\link{amplitude}}, \code{\link{fD.ind}}, \code{\link{fD.pro0}}, \code{\link{fD.pro2}}, \code{\link{fP.ind}}, \code{\link{fP.pro0}}, \code{\link{fP.pro2}}
}
\examples{
## Define a bird:
myBird = Bird(
  massTotal = 0.215, #  (kg) total body mass
  wingSpan = 0.67, #  (m) maximum wing span
  wingArea = 0.0652, #  (m2) maximum wing area
  type = "passerine"
)

## define a speed range
speedrange <- seq(5,14,length.out=5)

## compute aerodynamic power for that speed range:
Paero <- computeFlappingPower(myBird,speedrange)
print(Paero[c("speed","power","frequency","strokeplane")])
#   speed    power frequency strokeplane
# 1  5.00 2.789751  5.948083    46.56887
# 2  7.25 2.129466  5.948083    31.89129
# 3  9.50 2.203773  5.948083    22.51896
# 4 11.75 2.740763  5.948083    16.49120
# 5 14.00 3.673714  5.948083    12.09174

## prescribe strokeplane angle:
Paero <- computeFlappingPower(myBird,speedrange,strokeplane=20)
print(Paero[c("speed","power","frequency","strokeplane")])
#   speed    power frequency strokeplane
# 1  5.00 2.950259  5.948083          20
# 2  7.25 2.141581  5.948083          20
# 3  9.50 2.204132  5.948083          20
# 4 11.75 2.741335  5.948083          20
# 5 14.00 3.676224  5.948083          20

## prescribe frequency as a function of speed:
funFrequency = function(U){19.8 - 4.7*U + 0.45*U^2 - 0.0138*U^3}
Paero <- computeFlappingPower(myBird,speedrange,frequency=funFrequency,strokeplane='opt')
print(Paero[c("speed","power","frequency","strokeplane")])
#   speed    power frequency strokeplane
# 1  5.00 2.810431  5.825000    46.16223
# 2  7.25 2.356278  4.119247    25.99702
# 3  9.50 2.390251  3.930725    17.94304
# 4 11.75 2.860463  4.316291    14.52910
# 5 14.00 3.794431  4.332800    11.70058

## examine effect of frequency for a single airspeed:
speedrange <- rep(10,5) #  repeated speed
freqrange <- seq(3,10,length.out=5) #  frequency range
Paero <- computeFlappingPower(myBird,speedrange,frequency=freqrange,strokeplane='opt')
print(Paero[c("speed","power","frequency","strokeplane")])
#   speed    power frequency strokeplane
# 1    10 2.681028      3.00    13.87797
# 2    10 2.367982      4.75    18.90949
# 3    10 2.263765      6.50    21.52433
# 4    10 2.219739      8.25    21.71519
# 5    10 2.200852     10.00    20.18503
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}
