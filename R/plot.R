plot.powercurve.set <- function(x,...){
  opts <- list(...)
  symbol.speed <- .setDefault(opts,"symbol.speed","V")

  graphics::par(mar=c(3.1,3.1,0.4,3.1),mgp=c(1.9,.7,0),cex=0.75)

  mrP <- x$table["maximumRange",]
  maxP <- x$table["maximumSpeed",]
  maxV <- 1.1*maxP$speed
  maxP.aero <- 1.25*maxP$power.aero
  maxP.chem <- 1.05*max(maxP$power.chem,mrP$power.chem)
  minP <- x$table["minimumPower",]
  dyText.aero <- 0.07 * (maxP.aero - 0*minP$power.aero)
  dyText.chem <- 0.07 * (maxP.chem - 0*minP$power.chem)

  Vsub <- function(sub)paste0(symbol.speed,sub)

  # work around annotation interference
  if (maxP$power.aero/minP$power.aero<1.4) maxP.aero <- maxP.aero + minP$power.aero


    # plot power curves
  with(x$powercurve , {
    xinterp <- seq(min(speed),max(speed),length.out=25)
    smspl <- smooth.spline(speed,power.aero)
    plot( predict(smspl,x=xinterp),
          type= 'l',
          xlab=NA, ylab=NA,
          xlim=c(0,maxV), ylim=c(0,maxP.aero),
          axes=FALSE
    )
  })


  # mark Vmin and Vmax
  with(x$table["minimumSpeed",],{
    lines(speed*c(1,1),c(-1.1,1.1)*maxP.aero,lty=2,col="grey60")
    text(0.7*speed,0.55*maxP.aero,Vsub("min"),col="grey50",srt=90)
  })
  with(x$table["maximumSpeed",],{
    lines(speed*c(1,1),c(-1.1,1.1)*maxP.aero,lty=2,col="grey60")
    text(1.05*speed,0.55*maxP.aero,Vsub("max"),col="grey50",srt=90)
  })

  # mark Vclimb
  with(x$maxClimb,{
    lines(c(0,maxV),power.aero*c(1,1),lty=3)
    text(0.06*maxV,power.aero+dyText.aero,'Pmax')
    points(speed,power.aero,pch=17)
    text(speed,power.aero+dyText.aero,paste0(Vsub("mc")," (",round(climbRate,2),"m/s)"))
  })


  graphics::par(new=TRUE)

  # plot power.chem
  with(x$powercurve , {
    xinterp <- seq(min(speed),max(speed),length.out=25)
    smspl <- smooth.spline(speed,power.chem)
    plot( predict(smspl,x=xinterp),
          type='l',col='red3',
           xlab=NA, ylab=NA,
           xlim=c(0,maxV), ylim=c(0,maxP.chem),
           axes=FALSE
    )
  })

  # plot maximum range tangent
  with(x$table["maximumRange",],
       lines( c(0,speed,maxV), c(0,power.chem,power.chem*maxV/speed),
              lwd=0.75,col='red3'
       )
  )

  # mark Vmp and Vmr
  with(x$table[c("minimumPower","maximumRange"),],{
    points(speed,power.chem,col='red3',pch=17)
    text(speed,power.chem+dyText.chem,c(Vsub("mp"),Vsub("mr")),
         col='red3',
         xlim=c(0,maxV), ylim=c(0,maxP.chem)
    )
  })
  graphics::axis(side=2)
  graphics::axis(side=1)
  graphics::axis(side=4,col='red3',col.axis='red3')
  graphics::mtext(side = 1, line = 2,'Airspeed (m/s)')
  graphics::mtext(side = 2, line = 2,'Aerodynamic power (W)')
  graphics::mtext(side = 4, line = 2,'Chemical power (W)', col ='red3')
}
