#' =============================================================================
#' Project: ECHO LUR
#' Date Created: March 11, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Running through a vignette for the SpatioTemporal package to try to re-create
#' the model used by the MESA-Air team (Keller et al. 2015)
#' 
#' Intro vignette: http://127.0.0.1:15943/library/SpatioTemporal/doc/ST_intro.pdf
#' Comprehensive: http://127.0.0.1:16840/library/SpatioTemporal/doc/ST_tutorial.pdf
#' =============================================================================

# library(devtools)
# install_version("SpatioTemporal", version = "1.1.9.1", 
#                 repos = "http://cran.us.r-project.org")

library(SpatioTemporal)
browseVignettes(package = "SpatioTemporal")

library(Matrix)
library(plotrix)
library(maps)

data(mesa.data.raw, package="SpatioTemporal")

#' -----------------------------------------------------------------------------
#' Examine the data set
#' -----------------------------------------------------------------------------
names(mesa.data.raw)
head(mesa.data.raw$X) # Spatial covariates
head(mesa.data.raw$obs) # Observations at each sampling site (wide: time vs. location)
head(mesa.data.raw$lax.conc.1500) # ST covariates-- in this case, the CALINE output

#' -----------------------------------------------------------------------------
#' create the ST data object
#' -----------------------------------------------------------------------------
mesa.data <- createSTdata(obs = mesa.data.raw$obs, 
                          covars = mesa.data.raw$X,
                          SpatioTemporal = list(lax.conc.1500=mesa.data.raw$lax.conc.1500))
names(mesa.data)
print(mesa.data)

layout(matrix(c(1,2,1,3), 2, 2))
par(mar=c(2.3,3.3,2,1), mgp=c(2,1,0))
plot(mesa.data, "loc", main="Occurrence of Observations", xlab="",
     ylab="Location", col=c("black", "red"), legend.loc=NULL)
par(mar=c(3.3,3.3,2,1))
qqnorm(mesa.data, line=1)
scatterPlot(mesa.data, covar="km.to.coast", xlab="Distance to coast",
            ylab="NOx (log ppb)", pch=19, cex=.25,
            smooth.args=list(span=4/5,degree=2))

#' -----------------------------------------------------------------------------
#' Create the temporal basis functions
#' -----------------------------------------------------------------------------

#' Step 1: create a data maxtrix
D <- createDataMatrix(mesa.data)
D

#' Step 2: Determine the number of basis functions
SVD.cv <- SVDsmoothCV(D, 0:4)
SVD.cv

#' Figure 3
plot(SVD.cv)

#' From the tutorial: As would be expected in any regression scenario, increasing 
#' the number of basis functions increases R2 and decreases the MSE. All four 
#' statistics flatten out noticable after 2 basis functions, indicating that 2 
#' basis functions is likely to provide the most efficient description of the 
#' temporal variability. The lack of auto-correlation when fitting the temporal 
#' basis to data at each location (Figure 5) shows that 2 basis functions are 
#' sufficient to capture the temporal structure.

#' Step 3: Add the smooth temporal basis functions to the ST data object
mesa.data <- updateTrend(mesa.data, n.basis = 2)

#' Step 4: Evaluate the basis functions
par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(mesa.data, "obs", ID="60370113", xlab="", ylab="NOx (log ppb)", 
     main="Temporal trend 60370113")
plot(mesa.data, "res", ID="60370113", xlab="", ylab="NOx (log ppb)")
plot(mesa.data, "acf", ID="60370113")
plot(mesa.data, "pacf", ID="60370113")

#' -----------------------------------------------------------------------------
#' Fit the model
#' -----------------------------------------------------------------------------

#' Step 5: Compute the regression coefficients
names(mesa.data$covars)

beta.lm <- estimateBetaFields(mesa.data)
beta.lm

par(mfrow=c(1,2), mar=c(3.3,2.3,1.5,1), mgp=c(2,1,0))
plotCI(mesa.data$covars$log10.m.to.a1, beta.lm$beta[,1],
       uiw=1.96*beta.lm$beta.sd[,1], ylab="", xlab="Distance to A1-road",
       main="Beta-field for f1(t)")
plotCI(mesa.data$covars$km.to.coast, beta.lm$beta[,2],
       uiw=1.96*beta.lm$beta.sd[,2], ylab="", xlab="Distance to coast",
       main="Beta-field for f2(t)")

#' Step 6: Create the model object
#' Can specify different LUR formluae
LUR <- list(~log10.m.to.a1 + s2000.pop.div.10000 + km.to.coast, 
            ~km.to.coast, 
            ~km.to.coast)
cov.beta <-  list(covf="exp", nugget=FALSE)
cov.nu <- list(covf="exp", nugget = ~type, random.effect=FALSE)
locations <- list(coords = c("x","y"), long.lat = c("long","lat"), others= "type")

mesa.model <- createSTmodel(mesa.data, LUR = LUR, ST = "lax.conc.1500",
                            cov.beta = cov.beta, cov.nu = cov.nu,
                            locations = locations)
mesa.model

#' Step 7: Estimate model parameters

#' From the tutorial: To avoid potential numerical optimisation issues, the 
#' estimation function allows for multiple starting points, returning all optima 
#' found. The functions loglikeSTdim and loglikeSTnames gives the number of 
#' parameters (and other model dimension) and the names, i.e. expected order, of 
#' the parameters. Using this information a two column matrix, where each column 
#' represents a different optimisation starting point, is constructed:

dim <- loglikeSTdim(mesa.model)
dim
x.init <- cbind(c( rep(2, dim$nparam.cov-1), 0),
                c( rep(c(1,-3), dim$m+1), -3, 0))
rownames(x.init) <- loglikeSTnames(mesa.model, all=FALSE)

x.init

est.mesa.model <- estimate(mesa.model, x.init, type="p", hessian.all = FALSE)
print(est.mesa.model)

#' Step 8: Predictions

pred <- predict(mesa.model, est.mesa.model, LTA = TRUE, type = "p")
pred.log <- predict(mesa.model, est.mesa.model, LTA = TRUE, 
                    transform = "unbiased", type = "p")

par(mfrow=c(2,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0), pty="s")
for(i in 1:3){
  plotCI(x=beta.lm$beta[,i], y=pred$beta$EX[,i],
         uiw=1.96*beta.lm$beta.sd[,i], err="x",
         pch=NA, sfrac=0.005,
         main=paste("Beta-field for f", i, "(t)", sep=""),
         xlab="Empirical estimate",
         ylab="Spatio-Temporal Model")
  plotCI(x=beta.lm$beta[,i], y=pred$beta$EX[,i],
         uiw=1.96*sqrt(pred$beta$VX[,i]),
         add=TRUE, pch=NA, sfrac=0.005)
  abline(0, 1, col="grey")
}

par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
with(pred$LTA, plotCI(colMeans(D, na.rm=TRUE), EX, uiw=1.96*sqrt(VX.pred),
                      xlim=c(2.9,4.4), ylim=c(2.9,4.4),
                      ylab="Predictions", xlab="Observations",
                      main="Average NOx (log ppb)"))
abline(0, 1, col="grey")
with(pred.log$LTA, plotCI(colMeans(exp(D), na.rm=TRUE),
                          EX, uiw=1.96*sqrt(VX.pred),
                          xlim=c(25,95), ylim=c(25,95),
                          ylab="Predictions", xlab="Observations",
                          main="Average NOx (ppb)"))
abline(0, 1, col="grey")

#' -----------------------------------------------------------------------------
#' Cross-validation and model evaluation
#' -----------------------------------------------------------------------------

#' Step 9: Define CV groups

Ind.cv <- createCV(mesa.model, groups = 10, min.dist = .1)

#' The number of locations is roughly even, though the number of observations 
#' differs by group-- remember! This is an unbalanced design
ID.cv <- sapply(split(mesa.model$obs$ID, Ind.cv), unique)
print( sapply(ID.cv, length) )
table(Ind.cv)

#' Step 10: ID starting conditions using previous model without CV

x.init <- coef(est.mesa.model, pars="cov")[,c("par","init")]

#' Step 11: Run the model-- computationally intensive!!
est.cv.mesa <- estimateCV(mesa.model, x.init, Ind.cv)

par(mfrow=c(1,1), mar=c(13.5,2.5,.5,.5), las=2)
with(coef(est.mesa.model, pars="all"),
     plotCI((1:length(par))+.3, par, uiw=1.96*sd,
            col=2, xlab="", xaxt="n", ylab=""))
boxplot(est.cv.mesa, "all", boxwex=.4, col="grey", add=TRUE)

#' Step 12: Prediction-- computationally intensive

# pred.cv.mesa <- predictCV(mesa.model, est.cv.mesa, LTA=TRUE)
data(pred.cv.mesa, package="SpatioTemporal")
pred.cv.mesa.log <- predictCV(mesa.model, est.cv.mesa, LTA=TRUE, transform="unbiased")

summary(pred.cv.mesa.log)
summary(pred.cv.mesa.log, by.date=TRUE) 

#' Step 13: Plotting the CV model results

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4,5,6), 4, 2, byrow=TRUE))
plot(pred.cv.mesa, ID="60371601", xlab="", ylab="NOx (log ppb)",
     main="Predictions for 60371601", lty=c(1,NA), lwd=2,
     pch=c(NA,19), cex=.75)
plot(pred.cv.mesa, ID="60371601", pred.type="EX.mu",
     lty=4, lwd=2, col="blue", add=TRUE)
plot(pred.cv.mesa, ID="60371601", pred.type="EX.mu.beta",
     lty=2, lwd=2, col="green", add=TRUE)
plot(pred.cv.mesa.log, ID="60371601", xlab="", ylab="NOx (ppb)",
     main="Predictions for 60371601", pred.type="EX.pred",
     lty=c(1,NA), lwd=2, pch=c(NA,19), cex=.75)
plot(pred.cv.mesa.log, ID="60371601", pred.type="EX.mu",
     lty=4, lwd=2, col="blue", add=TRUE)
plot(pred.cv.mesa.log, ID="60371601", pred.type="EX.mu.beta",
     lty=2, lwd=2, col="green", add=TRUE)
legend("topright", c("Observations", "Predictions",
                     "Contribution from beta",
                     "Contribution from mean",
                      "95% CI"), bty="n",
         lty=c(NA,1,2,4,NA), lwd=c(NA,2,2,2,NA),
         pch=c(19,NA,NA,NA,15), pt.cex=c(.75,NA,NA,NA,2.5),
         col=c("red", "black", "green", "blue", "grey"))
plot(pred.cv.mesa, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2),
       col=c("ID", "black", "grey"), xlab="Observations",
       ylab="Predictions", main="Cross-validation NOx (log ppb)")
with(pred.cv.mesa.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
                                       xlab="Observations", ylab="Predictions",
                                      main="Temporal average NOx (ppb)"))
abline(0, 1, col="grey")
I.season <- as.factor(as.POSIXlt(pred.cv.mesa$pred.obs$date)$mon+1)
levels(I.season) <- c(rep("Winter",2), rep("Spring",3),
                      rep("Summer",3), rep("Fall",3), "Winter")
qqnorm(pred.cv.mesa, norm=TRUE, main="Normalised residuals",
       col=I.season)
legend("bottomright", legend=as.character(levels(I.season)),
       pch=1, col=1:nlevels(I.season), bty="n")
scatterPlot(pred.cv.mesa, STdata=mesa.model, covar="log10.m.to.a1",
            group=I.season, col=c(2:5,1), type="res",
            xlab="Distance to A1-Road", ylab="Residuals",
            main="Residuals (log ppb)")

#' -----------------------------------------------------------------------------
#' Predicting at unknown locations
#' Code from the comprehensive tutorial: http://127.0.0.1:16840/library/SpatioTemporal/doc/ST_tutorial.pdf
#' -----------------------------------------------------------------------------

library(SpatioTemporal)
library(plotrix)

# load data
data(mesa.data.raw)
data(mesa.model)
data(est.mesa.model)

#' Create an object where we drop the data for two sites
mesa.data.raw$obs <- mesa.data.raw$obs[,!(colnames(mesa.data.raw$obs) %in% c("60595001", "LC003"))]

mesa.data <- with(mesa.data.raw, createSTdata(obs, X, n.basis=2))
mesa.data

#' Update the data with additional dates in the trend (every week instead of every 2 weeks)
mesa.data.org <- mesa.data
T. <- with(mesa.data$trend, seq(min(date), max(date), by=7))
mesa.data <- updateTrend(mesa.data, n.basis = 2, extra.dates = T.)
mesa.data #559 time points, 4229 obs, 25 locations, 23 observed
mesa.data.org #280 time points, 4229 obs

#' Compare the temporal trends for both data sets
#' These should be identical because we are using the observations from the 
#' same 23 locations
par(mfrow=c(2,1), mar=c(2,2,2,1))
plot(mesa.data$trend$date, mesa.data$trend$V1,
     xlab="", ylab="", main="Trend 1")
points(mesa.data.org$trend$date, mesa.data.org$trend$V1,
       col="red", pch=3)
plot(mesa.data$trend$date, mesa.data$trend$V2,
     xlab="", ylab="", main="Trend 2")
points(mesa.data.org$trend$date, mesa.data.org$trend$V2,
       col="red", pch=3)

#' Create model objects for the reduced data set and the full data set
#' strip = T means unobserved locations in the data are dropped

mesa.model.1 <- createSTmodel(mesa.data,
                              LUR=mesa.model$LUR.list, cov.beta=mesa.model$cov.beta,
                              cov.nu=mesa.model$cov.nu,
                              locations=mesa.model$locations.list)

mesa.model.2 <- createSTmodel(mesa.data.org,
                              LUR=mesa.model$LUR.list, cov.beta=mesa.model$cov.beta,
                              cov.nu=mesa.model$cov.nu,
                              locations=mesa.model$locations.list, strip=TRUE)

print(mesa.model.1)
print(mesa.model.2)

#' Normally, we would fit each model
#' Here we're just taking the covariance-parameters from the previously estimated model
x <- coef(est.mesa.model, pars = "cov")$par 
x

#' Compute the predictions for the two models
#' E.1 computes predictions for all of the locations (observed and unobserved)
#' E.2 computes predictions for just the observed locations
E.1 <- predict(mesa.model.1, x, pred.var=FALSE)
E.2 <- predict(mesa.model.2, x, pred.var=FALSE)

print(E.1)
print(E.2)

#' Temporal averages
#' First, create a list with the identifiers for the averaging periods
#' Each element of the list is the group of dates in a year

LTA <- with(mesa.model.1$trend, split(date,as.POSIXlt(date)$year+1900))
LTA

#' Next, assign IDs to each list element
#' Each location needs it's own yearly lists
ID <- mesa.model.1$locations$ID
LTA <- rep(list(LTA), length(ID))
names(LTA) <- ID

ID
LTA
names(LTA)

#' Use this LTA list to predict LTAs
E.1.LTA <- predict(mesa.model.1, x, pred.var=FALSE, LTA=LTA)
head(E.1.LTA$LTA)



