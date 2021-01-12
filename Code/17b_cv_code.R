#' =============================================================================
#' Project: ECHO Aim 1 ST Prediction Model for Black Carbon
#' Task: Fitting the ST model
#' Date created: January 4, 2021
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description:
#' =============================================================================

# library(devtools)
# install_version("SpatioTemporal", version = "1.1.9.1", repos = "https://cran.r-project.org")

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

library(sf)
library(gstat)
library(sp)
library(raster)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(viridis)

#' -----------------------------------------------------------------------------
#' Testing possible models
#' -----------------------------------------------------------------------------

set.seed(1000)

#' -----------------------------------------------------------------------------
#' Model A: 1 temporal trend (basis) function
#' For this version of the model, use iid for cov.beta (beta, beta1) 
#' and exp for cov.nu (error).
#' Here we can specify different LUR formluae. The length of the LUR list should 
#' be number of basis functions + 1.
#' -----------------------------------------------------------------------------

denver.data.A <- denver.data2.1
names(denver.data.A$covars)

LUR.A <- list(covar_fun, covar_fun)

cov.beta.A <-  list(covf = c("iid", "iid"), nugget = T)
cov.nu.A <- list(covf = "exp", nugget = T, random.effect = FALSE)
locations.A <- list(coords = c("x","y"), long.lat = c("lon","lat"), others= "type")

denver.model.A <- createSTmodel(denver.data.A, LUR = LUR.A,
                                ST = c("bc_st_no2", "bc_st_smk"),
                                cov.beta = cov.beta.A, cov.nu = cov.nu.A,
                                locations = locations.A)
denver.model.A

# Estimate model parameters
# Note that:
#     nugget values can range from -1 to -6
#     range values can range from 0 to 4
#     try starting values where the sill and nugget are the same (e.g., both 0) and where they are very different (e.g., 0 and -5)
#     make sure the starting values are pretty different

names <- loglikeSTnames(denver.model.A, all=FALSE)
names

x.init.A <- cbind(c(0, 0, 0, 0, 0),
                  c(-1, -1, 8, -5, -1),
                  c(-5, -5, 8, -1, -5),
                  c(-1, -1, 12, -5, -1),
                  c(-5, -5, 12, -1, -5),
                  c(-7, -7, 12, -3, -3))

rownames(x.init.A) <- loglikeSTnames(denver.model.A, all=FALSE)
x.init.A

#' Difference from tutorial: use Josh Keller's version of the function
source(here::here("Code", "functions_model.R"))
est.denver.model.A <- estimate.STmodel(denver.model.A, x.init.A)
print(est.denver.model.A)

#' Cross-validation
#' Define the CV groups
set.seed(1000)
site_idsA <- unique(denver.model.A$obs$ID[which(str_detect(denver.model.A$obs$ID, "d_") == T)])
site_idsA

Ind.cv.A <- createCV(denver.model.A, groups = 10, min.dist = .1,
                     subset = site_idsA)

ID.cv.A <- sapply(split(denver.model.A$obs$ID, Ind.cv.A), unique)
print(sapply(ID.cv.A, length))
table(Ind.cv.A)

I.col.A <- apply(sapply(ID.cv.A, function(x) denver.model.A$locations$ID%in% x), 1,
                       function(x) if(sum(x)==1) which(x) else 0)
names(I.col.A) <- denver.model.A$locations$ID
print(I.col.A)

par(mfrow=c(1,1))
plot(denver.model.A$locations$long,
     denver.model.A$locations$lat,
     pch=23+floor(I.col.A/max(I.col.A)+.5), bg=I.col.A,
     xlab="Longitude", ylab="Latitude")
map("county", "colorado", col="#FFFF0055",fill=TRUE, add=TRUE)

#' ID starting conditions using previous model without CV:
x.init.A.cv <- coef(est.denver.model.A, pars="cov")[,c("par","init")]
x.init.A.cv

#' Run the model with cross validation
est.denver.A.cv <- estimateCV(denver.model.A, x.init.A.cv, Ind.cv.A)
print(est.denver.A.cv)
summary(est.denver.A.cv)

par(mfrow=c(1,1), mar=c(13.5,2.5,.5,.5), las=2)
with(coef(est.denver.model.A, pars="all"),
     plotCI((1:length(par))+.3, par, uiw=1.96*sd,
             col=2, xlab="", xaxt="n", ylab=""))
boxplot(est.denver.A.cv, "all", boxwex=.4, col="grey", add=TRUE)

#' Save the results as an .rdata object
save(denver.data.A, denver.model.A, est.denver.model.A, est.denver.A.cv,
     file = here::here("Results", "Denver_ST_Model_A.rdata"))

#' Prediction using the CV model
#' Making predictions using the CV model. Printing out the CV summary statistics as well
pred.A.cv <- predictCV(denver.model.A, est.denver.A.cv)#, LTA = T)
pred.A.cv.log <- predictCV(denver.model.A, est.denver.A.cv,# LTA = T,
                           transform="unbiased")

head(pred.A.cv$pred.all$EX)
tail(pred.A.cv$pred.all$EX)

head(pred.A.cv.log$pred.all$EX)
tail(pred.A.cv.log$pred.all$EX)

summary(pred.A.cv)
summary(pred.A.cv.log)

par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
plot(pred.A.cv, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2),
     col=c("ID", "black", "grey"),
     ylim=c(-1,2),
     xlab="Observations", ylab="Predictions",
     main="Cross-validation BC (log ug/m3)")
with(pred.A.cv.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
                                    xlab="Observations", ylab="Predictions",
                                    main="Temporal average BC (ug/m3)"))
abline(0, 1, col="grey")

# jpeg(filename = here::here("Figs", "ST_CV_Obs_vs_Pred_BC_ModA.jpeg"),
#      width = 8, height = 4, units = "in", res = 500)
# par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
# plot(pred.A.cv, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2),
#      col=c("ID", "black", "grey"),
#      ylim=c(-1,2),
#      xlab="Observations", ylab="Predictions",
#      main="Cross-validation BC (log ug/m3)")
# with(pred.A.cv.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
#                                       xlab="Observations", ylab="Predictions",
#                                       main="Temporal average BC (ug/m3)"))
# abline(0, 1, col="grey")
# dev.off()

#' Predictions for 2009-2019
#' What do the long-term predictions look like for this model?
#' Predicting at the distributed (residential + community) sites

x.A <- coef(est.denver.model.A, pars = "cov")$par 
x.A

E.A <- predict(denver.model.A, est.denver.model.A, LTA = T, 
               transform="unbiased", pred.var=FALSE)
print(E.A)

week_preds <- as.data.frame(E.A$EX) %>% 
  mutate(week = as.Date(rownames(E.A$EX)),
         year = year(as.Date(rownames(E.A$EX))))

week_sites <- colnames(week_preds)[which(str_detect(colnames(week_preds), "d_"))]

box_preds <- week_preds %>% 
  select(week, all_of(week_sites)) %>% 
  #filter(week %in% week_weeks) %>% 
  mutate(month = month(week),
         year = year(week)) %>% 
  filter(year %in% c(2009:2020))

box_data <- box_preds %>% 
  pivot_longer(-c(week, month, year), names_to = "location", values_to = "pred")
summary(box_data)

#' Box plot of weekly BC predicted at all distributed sites grouped by month
box_summary <- box_data %>% 
  group_by(month) %>% 
  summarize(median = median(pred, na.rm=T)) %>% 
  arrange(desc(median))
box_summary  

pred_box_plot <- ggplot(box_data) +
  geom_boxplot(aes(x = as.factor(month), y = pred), #, color = as.factor(month)),
               show.legend = F) +
  #scale_color_viridis(name = "Month", discrete = T) +
  facet_wrap(. ~ year, scales = "free") +
  xlab("") + ylab("Distributed site BC concentration (\u03BCg/m\u00B3): Predicted") +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_discrete(labels = str_sub(month.name, 1, 1)) 
pred_box_plot

#' -----------------------------------------------------------------------------
#' Model B: 2 temporal trend (basis) functions
#' Create the model object For this version of the model, use iid for cov.beta 
#' (beta, beta1, beta2) and exp for cov.nu (error).
#' Here we can specify different LUR formluae. The length of the LUR list should 
#' be number of basis functions + 1.
#' -----------------------------------------------------------------------------

denver.data.B <- denver.data2.2
names(denver.data.B$covars)

LUR.B <- list(covar_fun, covar_fun, covar_fun)

cov.beta.B <-  list(covf = c("iid", "iid", "iid"), nugget = T)
cov.nu.B <- list(covf = "exp", nugget = T, random.effect = FALSE)
locations.B <- list(coords = c("x","y"), long.lat = c("lon","lat"), others= "type")

denver.model.B <- createSTmodel(denver.data.B, LUR = LUR.B,
                                ST = c("bc_st_no2", "bc_st_smk"),
                                cov.beta = cov.beta.B, cov.nu = cov.nu.B,
                                locations = locations.B)
denver.model.B

#' Estimate model parameters
names <- loglikeSTnames(denver.model.B, all=FALSE)
names

x.init.B <- cbind(c(0, 0, 0, 0, 0, 0),
                  c(-10, -5, -5, 8, -3, -5),
                  c(-10, -5, -5, 8, -5, -5),
                  c(-10, -5, -5, 10, -3, -5),
                  c(-10, -5, -5, 10, -5, -5),
                  c(-12, -5, -5, 8, -5, -5),
                  c(-12, -5, -5, 10, -5, -5),
                  c(-12, -5, -5, 12, -5, -5))

rownames(x.init.B) <- loglikeSTnames(denver.model.B, all=FALSE)
x.init.B

#' Difference from tutorial: use Josh Keller's version of the function
source(here::here("Code", "functions_model.R"))
est.denver.model.B <- estimate.STmodel(denver.model.B, x.init.B)
print(est.denver.model.B)

#' Cross-validation
#' Define the CV groups

set.seed(1000)

site_idsB <- colnames(denver.model.B$D.beta)[which(str_detect(colnames(denver.model.B$D.beta), "d_") == T)]
site_idsB

Ind.cv.B <- createCV(denver.model.B, groups = 10, #min.dist = .1,
                     subset = site_idsB)

ID.cv.B <- sapply(split(denver.model.B$obs$ID, Ind.cv.B), unique)
print(sapply(ID.cv.B, length))
table(Ind.cv.B)

I.col.B <- apply(sapply(ID.cv.B, function(x) denver.model.B$locations$ID%in% x), 1,
                 function(x) if(sum(x)==1) which(x) else 0)
names(I.col.B) <- denver.model.B$locations$ID
print(I.col.B)

par(mfrow=c(1,1))
plot(denver.model.B$locations$long,
     denver.model.B$locations$lat,
     pch=23+floor(I.col.B/max(I.col.B)+.5), bg=I.col.B,
     xlab="Longitude", ylab="Latitude")
map("county", "colorado", col="#FFFF0055",fill=TRUE, add=TRUE)

#' ID starting conditions using previous model without CV:
x.init.B.cv <- coef(est.denver.model.B, pars="cov")[,c("par","init")]
x.init.B.cv

#' Run the model with cross validation
est.denver.B.cv <- estimateCV(denver.model.B, x.init.B.cv, Ind.cv.B)
print(est.denver.B.cv)

par(mfrow=c(1,1), mar=c(13.5,2.5,.5,.5), las=2)
with(coef(est.denver.model.B, pars="all"),
     plotCI((1:length(par))+.3, par, uiw=1.96*sd,
            col=2, xlab="", xaxt="n", ylab=""))
boxplot(est.denver.B.cv, "all", boxwex=.4, col="grey", add=TRUE)

#' Save the results as an .rdata object
save(denver.data.B, denver.model.B, est.denver.model.B, est.denver.B.cv,
     file = here::here("Results", "Denver_ST_Model_B.rdata"))

#' Prediction using the CV model
#' Making predictions using the CV model. Printing out the CV summary statistics as well

pred.B.cv <- predictCV(denver.model.B, est.denver.B.cv)#, LTA = T)
pred.B.cv.log <- predictCV(denver.model.B, est.denver.B.cv, #LTA = T, 
                           transform="unbiased")

head(pred.B.cv$pred.all$EX)
tail(pred.B.cv$pred.all$EX)

head(pred.B.cv.log$pred.all$EX)
tail(pred.B.cv.log$pred.all$EX)

summary(pred.B.cv)
summary(pred.B.cv.log)

par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
plot(pred.B.cv, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2),
     col=c("ID", "black", "grey"),
     ylim=c(-1,2),
     xlab="Observations", ylab="Predictions",
     main="Cross-validation BC (log ug/m3)")
# with(pred.B.cv.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
#                                     xlab="Observations", ylab="Predictions",
#                                     main="Temporal average BC (ug/m3)"))
abline(0, 1, col="grey")

# jpeg(filename = here::here("Figs", "ST_CV_Obs_vs_Pred_BC_ModB.jpeg"),
#      width = 8, height = 4, units = "in", res = 500)
# par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
# plot(pred.B.cv, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2),
#      col=c("ID", "black", "grey"),
#      ylim=c(-1,2),
#      xlab="Observations", ylab="Predictions",
#      main="Cross-validation BC (log ug/m3)")
# with(pred.B.cv.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
#                                       xlab="Observations", ylab="Predictions",
#                                       main="Temporal average BC (ug/m3)"))
# abline(0, 1, col="grey")
# dev.off()

#' Predictions for 2009-2020
#' What do the long-term predictions look like for this model?
#' Predicting at the distributed (residential + community) sites

x.B <- coef(est.denver.model.B, pars = "cov")$par 
x.B

E.B <- predict(denver.model.B, est.denver.model.B, LTA = T, 
               transform="unbiased", pred.var=FALSE)
print(E.B)

week_preds <- as.data.frame(E.B$EX) %>% 
  mutate(week = as.Date(rownames(E.B$EX)),
         year = year(as.Date(rownames(E.B$EX))))

week_sites <- colnames(week_preds)[which(str_detect(colnames(week_preds), "d_"))]

box_preds <- week_preds %>% 
  select(week, all_of(week_sites)) %>% 
  #filter(week %in% week_weeks) %>% 
  mutate(month = month(week),
         year = year(week)) %>%
  filter(year %in% c(2009:2020))

box_data <- box_preds %>% 
  pivot_longer(-c(week, month, year), names_to = "location", values_to = "pred")
summary(box_data)

#' Box plot of weekly BC predicted at all distributed sites grouped by month
box_summary <- box_data %>% 
  group_by(month) %>% 
  summarize(median = median(pred, na.rm=T)) %>% 
  arrange(desc(median))
box_summary  

pred_box_plot <- ggplot(box_data) +
  geom_boxplot(aes(x = as.factor(month), y = pred), #, color = as.factor(month)),
               show.legend = F) +
  #scale_color_viridis(name = "Month", discrete = T) +
  facet_wrap(. ~ year, scales = "free") +
  xlab("") + ylab("Distributed site BC concentration (\u03BCg/m\u00B3): Predicted") +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_discrete(labels = str_sub(month.name, 1, 1)) 
pred_box_plot
