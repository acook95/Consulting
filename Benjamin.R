library(pwr)
library(pwr2)

pwr.t.test(n=NULL, power = 0.8, d = 0.3, sig.level = 0.05, type = "paired", alternative = "greater")

samplesizes <- seq(from=40,to=150,by=10)
power.samplesizes <- power.t.test(n=samplesizes,d = 0.3, sig.level = 0.05, type = "paired")$power
plot(samplesizes,
     power.samplesizes,
     xlim=c(40,150),
     xlab="Sample size",
     ylab="Expected power",
     ylim=c(0,1),
     type="b",
     col="darkorange",
     lwd=5,axes=FALSE)
axis(1,at=c(0,50,100,150,200))
axis(2,at=c(0,0.25,0.5,0.75,1),labels=paste(c(0,25,50,75,100),"%"))


power1 <- pwr.t.test(n=NULL, power = 0.8, d = 0.3, sig.level = 0.05, type = "paired", alternative = "greater")

plot(power1)

power2 <- pwr.t.test(n=NULL, power = 0.8, d = 0.3, sig.level = 0.01, type = "paired", alternative = "greater")

plot(power2)


