#' =============================================================================
#' Project: ECHO LUR
#' Date Created: March 11, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' After meeting with Josh Keller (Statistics, CSU), we decided to try to 
#' implement the modeling framework used by the MESA Air team (Keller et al., 
#' 2015). This was implemented using the SpatioTemporal package in R.
#' 
#' The original paper used PLS to select the spatial covariates. That will be 
#' the eventual approach for this analysis, but for now, I'm going to use the
#' covariates selected by the LASSO model (see 16a_LUR_BC_average.R)
#' 
#' Update 03.18.20: I was receiving an error when trying to use the 
#' estimate.STmodel() function from the SpatioTemporal package. I emailed Josh
#' Keller for advice, and he responded with some suggestions. I've pasted his 
#' email response at the end of this script for safe keeping
#' 
#' Basically, I'm going to start with the following:
#' - Simple model (i.e., only one time trend)
#' - iid covariance structure
#' =============================================================================

library(SpatioTemporal)
library(numDeriv)
library(Matrix)
library(plotrix)
library(maps)

#' Step 1. Data formatting
#' Josh Keller made key revisions to the estimate.STmodel() function. Loading his
#' version here and examining the objects from the ```SpatioTemporal``` package

source(here::here("Code", "functions_model.R"))

data(mesa.data.raw, package="SpatioTemporal")
names(mesa.data.raw)

head(mesa.data.raw$X) # Spatial covariates
class(mesa.data.raw$X)

head(mesa.data.raw$obs) # Observations at each sampling site (wide: time vs. location)
class(mesa.data.raw$obs)

head(mesa.data.raw$lax.conc.1500) # ST covariates-- in this case, the CALINE output
class(mesa.data.raw$lax.conc.1500) # Separate matrices for each predictor

#' Get my data to match the Mesa data set. When I ran this the first time, Site 
#' 61 looked much different from the others, so I dropped it for now. I'll need 
#' to go back and figure out what was wonky about that site later.

library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(viridis)
library(lubridate)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' Loading the data sets needed for the analysis. Filtering out indoor filters, 
#' filters missing spatial data, and filters with BC concentrations below 0.

data_name <- "Combined_Filter_Data_AEA.csv"
lur_data <- read_csv(here::here("Data", data_name)) %>%
  filter(!is.na(lon)) %>%
  filter(indoor == 0) %>%
  filter(bc_ug_m3_dem > 0)

lur_data <- lur_data %>%
  rename("pm_ug_m3_raw" = "pm_ug_m3") %>%
  rename("pm_ug_m3" = "pm_ug_m3_dem") %>%
  rename("bc_ug_m3_raw" = "bc_ug_m3") %>%
  rename("bc_ug_m3" = "bc_ug_m3_dem")

#' Format the date variable to be Year-WeekNo. (starts on Sunday)
#' Convert that back to the first day of the week
lur_data <- lur_data %>%
  mutate(sample_week_no = format(StartDateLocal, "%Y-%W")) %>%
  mutate(sample_week = as.Date(cut(as.Date(StartDateLocal), "week")))

#' Add a unique site ID
lur_data <- mutate(lur_data, site_id_lonlat = paste(lon, lat, sep = "_"))

ids <- select(lur_data, site_id_lonlat) %>%
  distinct() %>%
  mutate(site_id = paste("d", seq_along(site_id_lonlat), sep = "_"))

lur_data <- left_join(lur_data, ids, by = "site_id_lonlat") %>%
  mutate(site_id = ifelse(filter_id == "080310027", "central", site_id)) %>% 
  arrange(sample_week)

lur_years <- seq(year(lur_data$sample_week[1]),
                 year(lur_data$sample_week[nrow(lur_data)]),
                 by = 1)
lur_years


#' Something is going on with Site 61. When calculating the beta fields, I got 
#' results that were an order of magnitude larger than for the other sites. How 
#' does this site differ from the others?
#' 
#' The plots below suggest that maybe there is a different temporal pattern at 
#' this site? It's not far from other sites, but we only have one campaign's 
#' worth of data here. I'm going to drop it for now, but I'll plan to come back 
#' to this eventually.

d61_data <- filter(lur_data, site_id == "d_61") %>% 
  st_as_sf(wkt = "WKT", crs = albers)
other_data <- filter(lur_data, site_id != "d_61") %>% 
  st_as_sf(wkt = "WKT", crs = albers)

plot(st_geometry(other_data), col = "red", pch = 16)
plot(st_geometry(d61_data), col = "blue", add = T, pch = 17)
legend(x = "topleft", c("Other sites", "Site 61"), 
       pch = c(16, 17),
       col = c("red", "blue"))

ggplot() +
  geom_boxplot(data = d61_data, aes(x = 1, y = bc_ug_m3, col = "d61")) +
  geom_boxplot(data = other_data, aes(x = 2, y = bc_ug_m3, col = "all")) +
  scale_color_viridis(name = "Site", discrete = T,
                      labels = c("d61" = "Site 61",
                                 "all" = "All other sites"))

ggplot() +
  geom_line(data = d61_data, aes(x = sample_week, y = bc_ug_m3, col = "d61")) +
  geom_line(data = filter(other_data, filter_id == "080310027"), 
            aes(x = sample_week, y = bc_ug_m3, col = "cent")) +
  scale_color_viridis(name = "Site", discrete = T,
                      labels = c("d61" = "Site 61",
                                 "cent" = "Central BC"))

#' Filter out site 61
lur_data <- filter(lur_data, site_id != "d_61")

#' Observations of black carbon at the distributed and central sites. Make sure 
#' to use log-transformed values!
  
bc_obs <- lur_data %>%
  select(site_id, sample_week, bc_ug_m3) %>%
  mutate(log_bc = log(bc_ug_m3)) %>%
  select(-bc_ug_m3) %>%
  pivot_wider(id_cols = sample_week,
              names_from = site_id, values_from = log_bc) %>%
  as.data.frame() %>%
  arrange(sample_week)
rownames(bc_obs) <- bc_obs$sample_week
bc_obs$sample_week <- NULL
bc_obs <- as.matrix(bc_obs)

bc_weeks <- rownames(bc_obs)

# rownames(bc_obs)
# colnames(bc_obs)
class(bc_obs)
dim(bc_obs)

#' Spatial covariates at the distributed site. These are based on the LASSO 
#' model (THIS WILL CHANGE IN THE FUTURE!! I just know how to use LASSO vs. PLS). 
#' Make sure the covariates are scaled here.

load(here::here("Results", "BC_LASSO_Model.Rdata"))
# log_bc_lasso_coef3

lasso_coef_df <- data.frame(name = log_bc_lasso_coef3@Dimnames[[1]][log_bc_lasso_coef3@i + 1],
                            coefficient = log_bc_lasso_coef3@x)
# lasso_coef_df
covars <- as.character(lasso_coef_df$name[-1])
covars
bc_sp_cov <- select(lur_data, site_id, lon, lat, covars) %>%
  distinct() %>%
  mutate_at(.vars = vars(covars),
            scale)

bc_sp_cov <- bc_sp_cov %>%
  rename(ID = site_id) %>%
  mutate(lon2 = lon, lat2 = lat) %>%
  st_as_sf(coords = c("lon2", "lat2"), crs = ll_wgs84) %>%
  st_transform(crs = albers)
sp_coords <- do.call(rbind, st_geometry(bc_sp_cov)) %>%
  as_tibble() %>% setNames(c("x","y"))
bc_sp_cov <- bind_cols(bc_sp_cov, sp_coords) %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

bc_sp_cov$type <- ifelse(bc_sp_cov$ID == "central", "central", "dist")
bc_sp_cov$type <- as.factor(bc_sp_cov$type)
bc_sp_cov$type

# head(bc_sp_cov)
class(bc_sp_cov)

cor(bc_sp_cov[,c(4:8)])

#' Spatiotemporal covariates at the distributed sites. We have available IDW 
#' estimates of temp, NO2, PM2.5, and BC. For now, just using NO2, since the 
#' correlations with distribued site log(BC) are decent. I tried to use two ST 
#' predictors, but ran into issues trying to fit the model.

cor(log(lur_data$bc_ug_m3), lur_data$idw_no2, use = "complete")
cor(log(lur_data$bc_ug_m3), lur_data$idw_pm, use = "complete")
cor(log(lur_data$bc_ug_m3), lur_data$idw_temp, use = "complete")

bc_st_pm <- select(lur_data, site_id, sample_week, idw_pm) %>%
  pivot_wider(id_cols = sample_week,
              names_from = site_id, values_from = idw_pm) %>%
  as.data.frame()
rownames(bc_st_pm) <- bc_st_pm$sample_week
bc_st_pm$sample_week <- NULL
bc_st_pm <- as.matrix(bc_st_pm)

bc_st_no2 <- select(lur_data, site_id, sample_week, idw_no2) %>%
  pivot_wider(id_cols = sample_week,
              names_from = site_id, values_from = idw_no2) %>%
  as.data.frame()
rownames(bc_st_no2) <- bc_st_no2$sample_week
bc_st_no2$sample_week <- NULL
bc_st_no2 <- as.matrix(bc_st_no2)

bc_st_temp <- select(lur_data, site_id, sample_week, idw_temp) %>%
  pivot_wider(id_cols = sample_week,
              names_from = site_id, values_from = idw_temp) %>%
  as.data.frame()
rownames(bc_st_temp) <- bc_st_temp$sample_week
bc_st_temp$sample_week <- NULL
bc_st_temp <- as.matrix(bc_st_temp)

#' Step 2. create the ST data object and determine the number of basis functions 
#' for the temporal trend
#' For now, going to use one ST predictor (NO2 estimated for each sampling location).
#' Josh advised 4 degrees of freedom per year. Going to check the SVDsmoothCV 
#' results for 4 DF per year. 

#' Based on the plots below, it looks like we can still reasonably use one basis 
#' function for now, but I might try two since that's where the metrics seem to 
#' level off. There might be some advantage to using more than 4 df per year. 
#' I'll need to ask Josh if these results suggest we need to add df to the basis
#'  functions (over the 4/year he originally suggested as a starting point).

denver.data <- createSTdata(obs = bc_obs,
                            covars = bc_sp_cov,
                            SpatioTemporal = list(bc_st_no2 = bc_st_no2))
D <- createDataMatrix(denver.data)
# names(denver.data)
# print(denver.data)

# Determine the number of basis functions
n_years <- length(unique(as.Date(cut(as.Date(rownames(bc_obs)), "year"))))

SVD.cv.4py <- SVDsmoothCV(D, 0:4, df = 4*n_years)
print(SVD.cv.4py)
plot(SVD.cv.4py)

n_df <- 4
n_basis <- 1

# Add the smooth temporal basis functions to the ST data object
denver.data <- updateTrend(denver.data, n.basis = n_basis, df = n_df*n_years)

head(denver.data$trend)
plot(denver.data$trend$date, denver.data$trend$V1, col = 1, pch = 16, cex = 0.5,
     xlab = "Date", ylab = "BC",
     main = "Basis function")
# points(denver.data$trend$date, denver.data$trend$V2, col = 2, pch = 16, cex = 0.5)

layout(matrix(c(1,2,1,3), 2, 2))
par(mar=c(2.3,3.3,2,1), mgp=c(2,1,0))
plot(denver.data, "loc", main="Occurrence of Observations", xlab="",
     ylab="Location", col=c("black", "red"), legend.loc=NULL)
par(mar=c(3.3,3.3,2,1))
qqnorm(denver.data, line=1)
scatterPlot(denver.data, covar="aadt_100", xlab="AADT in a 100 m buffer",
            ylab="BC (log ug/m3)", pch=19, cex=.25,
            smooth.args=list(span=4/5,degree=2))

#" Step 3: Evaluate the basis functions
#' Plotting the temporal trends at a few of the sites
par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(denver.data, "obs", ID="d_1", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_1")
plot(denver.data, "res", ID="d_1", xlab="", ylab="BC (log ug/m3)")
plot(denver.data, "acf", ID="d_1")
plot(denver.data, "pacf", ID="d_1")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(denver.data, "obs", ID="d_20", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_20")
plot(denver.data, "res", ID="d_20", xlab="", ylab="BC (log ug/m3)")
plot(denver.data, "acf", ID="d_20")
plot(denver.data, "pacf", ID="d_20")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(denver.data, "obs", ID="d_53", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_53")
plot(denver.data, "res", ID="d_53", xlab="", ylab="BC (log ug/m3)")
plot(denver.data, "acf", ID="d_53")
plot(denver.data, "pacf", ID="d_53")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(denver.data, "obs", ID="central", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend central")
plot(denver.data, "res", ID="central", xlab="", ylab="BC (log ug/m3)")
plot(denver.data, "acf", ID="central")
plot(denver.data, "pacf", ID="central")

#' Step 4: Fit the model
#' Step 4.1: Compute the regression coefficients

names(denver.data$covars)

beta.lm <- estimateBetaFields(denver.data)
# beta_lm
head(beta.lm$beta)

par(mfrow=c(1,2), mar=c(3.3,2.3,1.5,1), mgp=c(2,1,0))
plotCI(denver.data$covars$aadt_100, beta.lm$beta[,1],
       uiw=1.96*beta.lm$beta.sd[,1], ylab="", xlab="AADT in 100 m buffer",
       main="Beta-field for f1(t)")
plotCI(denver.data$covars$tree_cover_100, beta.lm$beta[,2],
       uiw=1.96*beta.lm$beta.sd[,2], ylab="", xlab="%Tree cover in 100 m buffer",
       main="Beta-field for f2(t)")

#' Step 4.2: Create the model object (iid covariance structure)
#' Here we can specify different LUR formluae. The length of the LUR list should 
#' be number of basis functions + 1. 
#' I'm going to start with the ```iid``` covariance structure and will be 
#' setting nugget = T 

names(denver.data$covars)
LUR <- list(~tree_cover_100 + low_int_100 + dist_m_military + dist_m_rail + aadt_100,
            ~tree_cover_100 + low_int_100 + dist_m_military + dist_m_rail + aadt_100)
cov.beta <-  list(covf="iid", nugget = T)
cov.nu <- list(covf="iid", nugget = T, random.effect = FALSE)
locations <- list(coords = c("x","y"), long.lat = c("lon","lat"), others= "type")

denver.model.iid <- createSTmodel(denver.data, LUR = LUR,
                                  ST = "bc_st_no2",
                                  cov.beta = cov.beta, cov.nu = cov.nu,
                                  locations = locations)
denver.model.iid

#' Step 4.3: Estimate model parameters (iid model)
#' Josh gave some guidance on how to set up the intial values (NOTE: the iid 
#' model doesn't have range or sill values, but the exp model does):
#' - nugget values can range from -1 to -6
#' - range values can range from 0 to 4
#' - try starting values where the sill and nugget are the same (e.g., both 0) 
#'   and where they are very different (e.g., 0 and -5)
#' - make sure the starting values are pretty different
#' 
#' We want to try a number of initial conditions to make sure we find the "right" solution. 

dim <- loglikeSTdim(denver.model.iid)
# dim
names <- loglikeSTnames(denver.model.iid, all=FALSE)
names

x.init.iid <- cbind(c(rep(-1, 3)), c(rep(-2, 3)), c(rep(-3, 3)),
                    c(rep(-4, 3)), c(rep(-5, 3)), c(rep(-6, 3)))
x.init.iid[nrow(x.init.iid),] <- 0

rownames(x.init.iid) <- loglikeSTnames(denver.model.iid, all=FALSE)
x.init.iid

#' Difference from tutorial: use Josh Keller's version of the function
source(here::here("Code", "functions_model.R"))
est.denver.model.iid <- estimate.STmodel(denver.model.iid, x.init.iid)
print(est.denver.model.iid)

#' Step 4.4: Create the model object (exp covariance structure)
#' Now I'm using the exp covariance structure and setting nugget = T

names(denver.data$covars)
LUR <- list(~tree_cover_100 + low_int_100 + dist_m_military + dist_m_rail + aadt_100,
            ~tree_cover_100 + low_int_100 + dist_m_military + dist_m_rail + aadt_100)
cov.beta <-  list(covf=c("exp", "iid"), nugget = c(TRUE, TRUE))
cov.nu <- list(covf="exp", nugget = T, random.effect = FALSE)
locations <- list(coords = c("x","y"), long.lat = c("lon","lat"), others= "type")

denver.model.exp <- createSTmodel(denver.data, LUR = LUR,
                                  ST = "bc_st_no2",
                                  cov.beta = cov.beta, cov.nu = cov.nu,
                                  locations = locations)
denver.model.exp

#' Step 4.5: Estimate model parameters (exp model)

dim <- loglikeSTdim(denver.model.exp)
# dim
names <- loglikeSTnames(denver.model.exp, all = FALSE)
names

x.init.exp <-
  cbind(
    c(0, 0, 0, 0, 0, 0, 0),
    c(0,-1,-1,-1, 0,-1,-1),
    c(0,-1,-5,-5, 0,-1,-1),
    c(0,-5,-1,-1, 0,-5,-5),
    c(0,-5,-5,-5, 0,-5,-5),
    c(2,-1,-1,-1, 2,-1,-1),
    c(2,-1,-5,-5, 2,-1,-1),
    c(2,-5,-1,-1, 2,-5,-5),
    c(2,-5,-5,-5, 2,-5,-5),
    c(4,-1,-1,-1, 4,-1,-1),
    c(4,-1,-5,-5, 4,-1,-1),
    c(4,-5,-1,-1, 4,-5,-5),
    c(4,-5,-5,-5, 4,-5,-5),
    c(6,-1,-1,-1, 6,-1,-1),
    c(6,-1,-5,-5, 6,-1,-1),
    c(6,-5,-1,-1, 6,-5,-5),
    c(6,-5,-5,-5, 6,-5,-5)
  )
x.init.exp[nrow(x.init.exp), ] <- 0

rownames(x.init.exp) <-
  loglikeSTnames(denver.model.exp, all = FALSE)
x.init.exp

#' Difference from tutorial: use Josh Keller's version of the function
source(here::here("Code", "functions_model.R"))
est.denver.model.exp <-
  estimate.STmodel(denver.model.exp, x.init.exp)
print(est.denver.model.exp)

#' Step 5: Predictions
#' In order to get this to work, I'm going to use with only.obs = T option. 
#' When I tried to use the predict function without setting only.obs = T, 
#' I got NA values in each of the columns for the pred$LAT object.

pred <- predict(denver.model.exp, est.denver.model.exp, denver.data,
                only.obs = T, LTA = T, type = "p")
pred.log <- predict(denver.model.exp, est.denver.model.exp, denver.data,
                    only.obs = T, LTA = T,
                    transform = "unbiased", type = "p")

par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
with(pred$LTA, plotCI(colMeans(D, na.rm=TRUE), EX, uiw=1.96*sqrt(VX.pred),
                      xlim=c(-0.2,0.8), ylim=c(-0.5,0.8),
                      ylab="Predictions", xlab="Observations",
                      main="Average BC (log ug/m3)"))
abline(0, 1, col="grey")
with(pred.log$LTA, plotCI(colMeans(exp(D), na.rm=TRUE),
                          EX, uiw=1.96*sqrt(VX.pred),
                          xlim=c(0.6,2), ylim=c(0.6,2),
                          ylab="Predictions", xlab="Observations",
                          main="Average BC (ug/m3)"))
abline(0, 1, col="grey")

# jpeg(filename = here::here("Figs", "ST_Obs_vs_Pred_BC.jpeg"),
#      width = 6, height = 3, units = "in", res = 500)
# par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
# with(pred$LTA, plotCI(colMeans(D, na.rm=TRUE), EX, uiw=1.96*sqrt(VX.pred),
#                       xlim=c(-0.2,0.8), ylim=c(-0.5,0.8),
#                       ylab="Predictions", xlab="Observations",
#                       main="Average BC (log ug/m3)"))
# abline(0, 1, col="grey")
# with(pred.log$LTA, plotCI(colMeans(exp(D), na.rm=TRUE),
#                           EX, uiw=1.96*sqrt(VX.pred),
#                           xlim=c(0.8,2), ylim=c(0.6,2),
#                           ylab="Predictions", xlab="Observations",
#                           main="Average BC (ug/m3)"))
# abline(0, 1, col="grey")
# dev.off()

#' Step 6: Cross-validation and model evaluation
#'Josh recommended 10-fold CV for the distributed sites and LOOCV for the 
#'monitor. These will need to be separate processes. 

#' However, it might not make sense to do LOOCV for the central site because we 
#' only have the one. I'll skip it for now, but come back to this in the future
                                                            
#' Step 6.1: Define CV groups
unique(colnames(bc_obs))
Ind.cv <- createCV(denver.model.exp, groups = 10, min.dist = .1, 
                   subset = paste0("d_", 1:60))

#' The number of locations is roughly even, though the number of observations 
#' differs by group-- remember! This is an unbalanced design

ID.cv <- sapply(split(denver.model.exp$obs$ID, Ind.cv), unique)
print(sapply(ID.cv, length))
table(Ind.cv)

I.col <- apply(sapply(ID.cv,function(x) denver.model.exp$locations$ID%in% x), 1,
               function(x) if(sum(x)==1) which(x) else 0)
names(I.col) <- denver.model.exp$locations$ID
print(I.col)

par(mfrow=c(1,1))
plot(denver.model.exp$locations$long,
     denver.model.exp$locations$lat,
     pch=23+floor(I.col/max(I.col)+.5), bg=I.col,
     xlab="Longitude", ylab="Latitude")
map("county", "colorado", col="#FFFF0055",fill=TRUE, add=TRUE)

#' Step 6.2: ID starting conditions using previous model without CV
x.init.cv <- coef(est.denver.model.exp, pars="cov")[,c("par","init")]
x.init.cv

#' Step 6.3 : Run the model
est.denver.cv.exp <- estimateCV(denver.model.exp, x.init.cv, Ind.cv)
print(est.denver.cv.exp)

par(mfrow=c(1,1), mar=c(13.5,2.5,.5,.5), las=2)
with(coef(est.denver.model.exp, pars="all"),
     plotCI((1:length(par))+.3, par, uiw=1.96*sd,
            col=2, xlab="", xaxt="n", ylab=""))
boxplot(est.denver.cv.exp, "all", boxwex=.4, col="grey", add=TRUE)

#' Save the results as an .rdata object
save(denver.data, denver.model.iid, denver.model.exp,
     est.denver.model.iid, est.denver.model.exp,
     est.denver.cv.exp,
     file = here::here("Results", "Denver_ST_Model.rdata"))

#' Step 7: Prediction using the CV model
#' Making predctions using the CV model.
#' Printing out the CV summary statistics as well

pred.cv <- predictCV(denver.model.exp, est.denver.cv.exp, LTA = T)
pred.cv.log <- predictCV(denver.model.exp, est.denver.cv.exp, 
                         LTA = T, transform="unbiased")

names(pred.cv)
summary(pred.cv)
summary(pred.cv.log)

par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
plot(pred.cv, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2), 
     col=c("ID", "black", "grey"),      
     ylim=c(-1,2),
     xlab="Observations", ylab="Predictions", 
     main="Cross-validation BC (log ug/m3)")
with(pred.cv.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
                                  xlab="Observations", ylab="Predictions", 
                                  main="Temporal average BC (ug/m3)"))
abline(0, 1, col="grey")

jpeg(filename = here::here("Figs", "ST_CV_Obs_vs_Pred_BC.jpeg"),
     width = 8, height = 4, units = "in", res = 500)
par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
plot(pred.cv, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2), 
     col=c("ID", "black", "grey"), 
     ylim=c(-1,2),
     xlab="Observations", ylab="Predictions", 
     main="A) Cross-validation BC (log ug/m3)")
with(pred.cv.log$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
                                  xlab="Observations", ylab="Predictions", 
                                  main="B) Temporal average BC (ug/m3)"))
abline(0, 1, col="grey")
dev.off()

#' Step 8: Try adding the PM data to get longer time trends
#' Based on feedback from Josh, I'm going to try to use the long term record for 
#' PM2.5 data from the central site monitors to estimate the spatial trend. 
#' Then I'll add this trend to the STdata object.

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

#' PM2.5 concentrations
pm_data <- read_csv(here::here("Data", "Monitor_PM_Data_AEA.csv")) %>%
  filter(!is.na(Arithmetic_Mean)) %>%
  mutate(year = year(Date_Local)) %>%
  # filter(year %in% lur_years) %>%
  select(-year) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  filter(County_Code %in% counties) %>%
  filter(Sample_Duration != "1 HOUR") %>%
  arrange(Date_Local, monitor_id)

pm_dates <- as.Date(cut(as.Date(pm_data$Date_Local), "week"))

#PM observations
pm_obs <- st_set_geometry(pm_data, NULL) %>%
  select(monitor_id, Date_Local, Arithmetic_Mean) %>%
  mutate(sample_week = as.Date(cut(as.Date(Date_Local), "week"))) %>%
  group_by(monitor_id, sample_week) %>%
  summarize(pm_ug_m3 = mean(Arithmetic_Mean, na.rm=T)) %>%
  pivot_wider(id_cols = sample_week,
              names_from = monitor_id, values_from = pm_ug_m3) %>%
  as.data.frame() %>%
  arrange(sample_week) %>%
  filter(sample_week %in% as.Date(bc_weeks, format = "%Y-%m-%d"))

rownames(pm_obs) <- pm_obs$sample_week
pm_obs$sample_week <- NULL
pm_obs <- as.matrix(pm_obs)

rnames <- rownames(pm_obs)
cnames <- colnames(pm_obs)

#' Scale the data by columns (monitors)?
pm_obs <- apply(pm_obs, 2, scale)

rownames(pm_obs) <- rnames
colnames(pm_obs) <- cnames

head(rownames(pm_obs))
head(colnames(pm_obs))
class(pm_obs)
dim(pm_obs)

# PM SP object
pm_data2 <- pm_data %>%
  mutate(sample_week = as.Date(cut(as.Date(Date_Local), "week"))) 
pm_coords <- do.call(rbind, st_geometry(pm_data2)) %>%
  as_tibble() %>% setNames(c("x","y"))
pm_sp_lonlat <- pm_data2 %>%
  st_transform(crs = ll_wgs84)
pm_coords2 <- do.call(rbind, st_geometry(pm_sp_lonlat)) %>%
  as_tibble() %>% setNames(c("lon","lat"))

pm_sp_cov <- st_set_geometry(pm_data2, NULL) %>%
  select(monitor_id) %>%
  rename(ID = monitor_id) 
pm_sp_cov <- bind_cols(pm_sp_cov, pm_coords, pm_coords2) %>%
  as.data.frame() %>%
  distinct() %>% 
  slice(-10)

pm_STdata <- createSTdata(obs = pm_obs,
                          covars = pm_sp_cov)
print(pm_STdata)

#' Step 9: Create the STdata object for the Denver data and generate the 
#' temporal trend from the pm_stData object
#' Sticking with 4 df per year and 1 basis function for now.

D_pm <- createDataMatrix(pm_STdata)
# D_pm

n_years <- length(unique(as.Date(cut(as.Date(pm_data$Date_Local), "year"))))

SVD.cv.4py <- SVDsmoothCV(D_pm, 0:4, df = 4*n_years)
print(SVD.cv.4py)
plot(SVD.cv.4py)

SVD.cv.8py <- SVDsmoothCV(D_pm, 0:4, df = 8*n_years)
print(SVD.cv.8py)
plot(SVD.cv.8py)

n_df <- 4
n_basis <- 1

#' Now, update the pm STdata object with the basis function
pm_STdata <- updateTrend(pm_STdata, n.basis = n_basis, df = n_df*n_years)
pm_STdata

head(pm_STdata$trend)
plot(pm_STdata$trend$date, pm_STdata$trend$V1, col = 1, pch = 16, cex = 0.5,
     xlab = "Date", ylab = "PM (scaled)")

denver.data2 <- denver.data
denver.data2$trend <- pm_STdata$trend
print(denver.data2)

#' Step 10: Evaluate the basis functions using the PM data instead of the BC data 
#' Plotting the temporal trends at a few of the sites. These should look very similar to the ones from before

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(denver.data2, "obs", ID="d_1", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_1")
plot(denver.data2, "res", ID="d_1", xlab="", ylab="BC (log ug/m3)")
plot(denver.data2, "acf", ID="d_1")
plot(denver.data2, "pacf", ID="d_1")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(denver.data2, "obs", ID="d_20", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_20")
plot(denver.data2, "res", ID="d_20", xlab="", ylab="BC (log ug/m3)")
plot(denver.data2, "acf", ID="d_20")
plot(denver.data2, "pacf", ID="d_20")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(denver.data2, "obs", ID="d_53", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend d_53")
plot(denver.data2, "res", ID="d_53", xlab="", ylab="BC (log ug/m3)")
plot(denver.data2, "acf", ID="d_53")
plot(denver.data2, "pacf", ID="d_53")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(denver.data2, "obs", ID="central", xlab="", ylab="BC (log ug/m3)",
     main="Temporal trend central")
plot(denver.data2, "res", ID="central", xlab="", ylab="BC (log ug/m3)")
plot(denver.data2, "acf", ID="central")
plot(denver.data2, "pacf", ID="central")

#' Step 11: Create the model object (exp covariance structure) using the new 
#' trend data

names(denver.data2$covars)
LUR <- list(~tree_cover_100 + low_int_100 + dist_m_military + dist_m_rail + aadt_100,
            ~tree_cover_100 + low_int_100 + dist_m_military + dist_m_rail + aadt_100)
cov.beta <-  list(covf=c("exp", "iid"), nugget = c(TRUE, TRUE))
cov.nu <- list(covf="exp", nugget = T, random.effect = FALSE)
locations <- list(coords = c("x","y"), long.lat = c("lon","lat"), others= "type")

denver.model.exp2 <- createSTmodel(denver.data2, LUR = LUR,
                                   ST = "bc_st_no2",
                                   cov.beta = cov.beta, cov.nu = cov.nu,
                                   locations = locations)
denver.model.exp2

#' Step 12: Fit the new model 
#' Going with the initial values that were close to the "best" model last time

dim <- loglikeSTdim(denver.model.exp2)
# dim
names <- loglikeSTnames(denver.model.exp2, all=FALSE)
names

x.init.exp2 <- cbind(c(0, 0, 0, 0, 0, 0, 0),
                     c(4, -1, -1, -1, 4, -1, -1),
                     c(4, -1, -5, -5, 4, -1, -1),
                     c(4, -5, -1, -1, 4, -5, -5),
                     c(4, -5, -5, -5, 4, -5, -5)
)
x.init.exp2[nrow(x.init.exp2),] <- 0

rownames(x.init.exp2) <- loglikeSTnames(denver.model.exp2, all=FALSE)
x.init.exp2

#' Difference from tutorial: use Josh Keller's version of the function
source(here::here("Code", "functions_model.R"))
est.denver.model.exp2 <- estimate.STmodel(denver.model.exp2, x.init.exp2)
print(est.denver.model.exp2)

#' Step 13: Cross-validation and model evaluation using the updated time trends
#' Step 13.1: Define CV groups

Ind.cv2 <- createCV(denver.model.exp2, groups = 10, min.dist = .1, 
                    subset = paste0("d_", 1:60))

#' The number of locations is roughly even, though the number of observations 
#' differs by group-- remember! This is an unbalanced design

ID.cv2 <- sapply(split(denver.model.exp2$obs$ID, Ind.cv2), unique)
print(sapply(ID.cv2, length))
table(Ind.cv2)

I.col2 <- apply(sapply(ID.cv2,function(x) denver.model.exp2$locations$ID%in% x), 1,
                function(x) if(sum(x)==1) which(x) else 0)
names(I.col2) <- denver.model.exp2$locations$ID
print(I.col2)

par(mfrow=c(1,1))
plot(denver.model.exp$locations$long,
     denver.model.exp$locations$lat,
     pch=23+floor(I.col/max(I.col)+.5), bg=I.col,
     xlab="Longitude", ylab="Latitude")
map("county", "colorado", col="#FFFF0055",fill=TRUE, add=TRUE)

#' Step 13.2: ID starting conditions using previous model without CV
x.init.cv2 <- coef(est.denver.model.exp2, pars="cov")[,c("par","init")]
x.init.cv2

#' Step 13.3 : Run the CV model
est.denver.cv.exp2 <- estimateCV(denver.model.exp2, x.init.cv2, Ind.cv2)
print(est.denver.cv.exp2)

par(mfrow=c(1,1), mar=c(13.5,2.5,.5,.5), las=2)
with(coef(est.denver.model.exp2, pars="all"),
     plotCI((1:length(par))+.3, par, uiw=1.96*sd,
            col=2, xlab="", xaxt="n", ylab=""))
boxplot(est.denver.cv.exp2, "all", boxwex=.4, col="grey", add=TRUE)

#' Save the results as an .rdata object
save(denver.data, denver.model.iid, denver.model.exp,
     est.denver.model.iid, est.denver.model.exp,
     est.denver.cv.exp,
     denver.model.exp2, est.denver.model.exp2,
     est.denver.cv.exp2,
     file = here::here("Results", "Denver_ST_Model.rdata"))

#' Step 14: Prediction using the CV model with the longer time period
#' Making predctions using the CV model.
#' Printing out the CV summary statistics as well

pred.cv2 <- predictCV(denver.model.exp2, est.denver.cv.exp2, LTA = T)
pred.cv.log2 <- predictCV(denver.model.exp2, est.denver.cv.exp2, 
                          LTA = T, transform="unbiased")

names(pred.cv2)
summary(pred.cv2)
summary(pred.cv.log2)

par(mfrow=c(1,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
plot(pred.cv2, "obs", ID="all", pch=c(19,NA), cex=.25, lty=c(NA,2), 
     col=c("ID", "black", "grey"),      
     ylim=c(-1,2),
     xlab="Observations", ylab="Predictions", 
     main="Cross-validation BC (log ug/m3)")
with(pred.cv.log2$pred.LTA, plotCI(obs, EX.pred, uiw=1.96*sqrt(VX.pred),
                                   xlab="Observations", ylab="Predictions", 
                                   main="Temporal average BC (ug/m3)"))
abline(0, 1, col="grey")

#' Step 15 Predictions for the full grid
#' Due to the size of the grid, each week of spatiotemporal variables is a 
#' separate .csv file. Here I'm predicting just for the summer of 2018 to see 
#' how things look.
#' 
#' Will need to discuss with Josh at our next meeting

#' Read in SP covariates and get into the same format as the filter data
grid_loc_df <- read_csv(here::here("Data", "Grid_250_m_AEA.csv")) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  st_centroid()
grid_loc_df2 <- st_transform(grid_loc_df, crs = ll_wgs84)

grid_aea_coords <- do.call(rbind, st_geometry(grid_loc_df)) %>%
  as_tibble() %>% setNames(c("x","y"))
grid_ll_coords <- do.call(rbind, st_geometry(grid_loc_df2)) %>%
  as_tibble() %>% setNames(c("lon","lat"))

grid_sp_df <- read_csv(here::here("Data", "Spatial_Covariates_Grid_250_m_AEA.csv"))

covars
grid_sp_cov_all <- bind_cols(grid_sp_df, grid_aea_coords, grid_ll_coords) %>%
  select(grid_id, x, y, lon, lat, covars) %>%
  mutate_at(.vars = vars(covars),
            scale) %>%
  rename(ID = grid_id) %>% 
  mutate(type = as.factor("dist"))

# grid_sp_cov <- filter(grid_sp_cov_all, ID %in% c(1:1000))
grid_sp_cov <- grid_sp_cov_all
head(grid_sp_cov)

# #' ST covariates for the summer of 2018
# st_files <- list.files(here::here("Data/Grid_ST_Data"))
# 
# summer_dates_start <- as.Date("2018-06-04", format = "%Y-%m-%d")
# summer_dates_end <- as.Date("2018-08-27", format = "%Y-%m-%d")
# summer_dates <- seq.Date(summer_dates_start, summer_dates_end, by = "week")
# summer_dates <- summer_dates[-6]
# 
# grid_no2 <- data.frame()
# pol <- "idw_no2"
# 
# for(i in 1:length(summer_dates)) {
#   df <- read_csv(here::here("Data/Grid_ST_Data",
#                             paste0("grid_st_cov_", summer_dates[i], ".csv"))) %>%
#     arrange(grid_id) %>%
#     mutate(date = summer_dates[i]) %>%
#     select(grid_id, pol, date)
#   df2 <- df %>%
#     pivot_wider(id_cols = date, names_from = grid_id, values_from = idw_no2)
# 
#   grid_no2 <- bind_rows(grid_no2, df2)
#   rm(df, df2)
# }
# 
# write_csv(grid_no2, here::here("Data", "Grid_IDW_NO2_Summer18.csv"))
grid_no2 <- read_csv(here::here("Data", "Grid_IDW_NO2_Summer18.csv"))

grid_st_no2_all <- grid_no2
rownames(grid_st_no2_all) <- grid_st_no2_all$date
grid_st_no2_all$date <- NULL
grid_st_no2_all <- as.matrix(grid_st_no2_all)

grid_st_no2 <- grid_st_no2_all[,c(1:1000)]
dim(grid_st_no2)
head(grid_st_no2[,c(1:10)])

#' Create an empty observation data set witht the same dimensions as the ST matrix
grid_obs <- matrix(ncol = ncol(grid_st_no2), nrow = nrow(grid_st_no2))

dim(grid_obs)
colnames(grid_obs) <- colnames(grid_st_no2)
rownames(grid_obs) <- rownames(grid_st_no2)
head(grid_obs[,c(1:10)])

#' Create the grid data set and add PM trend data
grid.data <- createSTdata(obs = NULL,
                          covars = grid_sp_cov,
                          SpatioTemporal = list(bc_st_no2 = grid_st_no2),
                          extra.dates = summer_dates)
grid.data
grid.data$trend <- pm_STdata$trend
grid.data

#' Predict for the grid using the model without CV
pred.grid <- predict(object = denver.model.exp2, x = est.denver.model.exp2,
                     STdata = grid.data, LTA = T)
pred.grid.log <- predict(object = denver.model.exp2, x = est.denver.model.exp2,
                         STdata = grid.data, transform="unbiased", LTA = T)

names(pred.grid)
head(pred.grid$EX)
head(pred.grid$LTA)
summary(pred.grid$LTA$EX)
summary(pred.grid$LTA$EX.mu)

names(pred.grid.log)
head(pred.grid.log$EX)
head(pred.grid.log$LTA)
summary(pred.grid.log$LTA$EX)
summary(pred.grid.log$LTA$EX.mu)
