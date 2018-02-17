df.data <-
    read.table("~/Dropbox/mach3/filesDXF/361.csv",
               header=TRUE, sep=";", dec=",")

modello <-
    lm(Z~X+I(X^2)+I(X^3), data=df.data)



regressione <-
    predict(modello, data= df.data$X)
plot(df.data, xlim=c(-5,18), ylim=c(-1,5))
points(df.data$X, regressione, type="l", col=2)
df.newdata <-
    data.frame(X=seq(-10,18, by=0.01))
df.newdata$Z <-
    1.2356236 +
    0.5332291*df.newdata$X +
    -0.0323905*df.newdata$X^2 +
    0.0007024*df.newdata$X^3
points(df.newdata, type="l", col=3)

## E' una cubica quindi
## Z =  1.2356236 +
##      0.5332291 * X +
##     -0.0323905 * X ** 2 +
##      0.0007024 * X ** 3
## in termini di Gcode
