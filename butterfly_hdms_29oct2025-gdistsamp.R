library(unmarked); library(AICcmodavg)

# read in data and create unmarked frame
################################################################################
butts1 <- read.csv("~/butts2024-25.csv")
nrow(butts1)
butts1 <- subset(butts1, practice != "not_enrolled_in_NRCS")
nrow(butts1)

# missing flower values with mean
################################################################################
butts1$flowers[is.na(butts1$flowers)] <- mean(butts1$flowers, na.rm = TRUE)

# prep data for unmarked frame
################################################################################
butt.obs <- as.matrix(butts1[,c("dist1", "dist2", "dist3", "dist4", "dist5")]) # detection data

breaks <- c(0,2,4,6,8,10) # this needs to be length = J + 1 (J= no. of distance classes)

# site covariates
sitecovs1 <- data.frame(
  ######################## site covariates
  "practice" = as.factor(butts1$practice),
  "treated" = as.factor(butts1$treated),   
  "flowers" = as.numeric(butts1$flowers),
  
  # veg
  "canopy" = as.numeric(butts1$perc_canopy),
  "tallsap" = as.numeric(butts1$perc_tallsap),
  "shortsap" = as.numeric(butts1$perc_shortsap),
  "tallshrub" = as.numeric(butts1$perc_tallshrub),
  "shortshrub" = as.numeric(butts1$perc_shortshrub),      
  "forb" = as.numeric(butts1$perc_forb),
  "tallgrass" = as.numeric(butts1$perc_tallgrass),
  "shortgrass" = as.numeric(butts1$perc_shortgrass),
  "CWD" = as.numeric(butts1$perc_CWD),
  "litter" = as.numeric(butts1$perc_litter),
  "bare" = as.numeric(butts1$perc_bare),
  "sap" = as.numeric(butts1$perc_anysap),
  "shrub" = as.numeric(butts1$perc_anyshrub),
  "woody" = as.numeric(butts1$perc_anywoody),
  "grass" = as.numeric(butts1$perc_anygrass),
  
  ######################## survey covariates
  "visit" = as.numeric(butts1$visit),
  "temp" = butts1$temp,
  "observer" = as.factor(butts1$observer),
  "wind" = butts1$wind,
  "cloud" = butts1$cloud,
  "ordinal" = butts1$ordinal,
  "mssr" = butts1$mssr)

# creating unmarked frame

umf1 <- unmarkedFrameGDS(y = butt.obs, 
                         survey = "line",
                         tlength = rep(50, nrow(butt.obs)),
                         unitsIn = "m",
                         dist.breaks = breaks, 
                         numPrimary = 1, 
                         siteCovs = sitecovs1, 
                         yearlySiteCovs = sitecovs1)

summary(umf1)

###########
mod0.p.exp <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                        keyfun = "exp", output = "density", unitsOut = "ha",
                        mixture = "P", K = 100, se = TRUE, data = umf1)

mod0.p.haz <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                        keyfun = "haz", output = "density", unitsOut = "ha",
                        mixture = "P", K = 100, se = TRUE, data = umf1)

mod0.p.hn <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                       keyfun = "halfnorm", output = "density", unitsOut = "ha",
                       mixture = "P", K = 100, se = TRUE, data = umf1)

mod0.nb.exp <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                         keyfun = "exp", output = "density", unitsOut = "ha",
                         mixture = "NB", K = 100, se = TRUE, data = umf1)

mod0.nb.haz <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                         keyfun = "haz", output = "density", unitsOut = "ha",
                         mixture = "NB", K = 100, se = TRUE, data = umf1)

mod0.nb.hn <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                        keyfun = "halfnorm", output = "density", unitsOut = "ha",
                        mixture = "NB", K = 100, se = TRUE, data = umf1)

#mod0.uni <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
#                      keyfun = "uniform", output = "density", unitsOut = "ha",
#                      mixture = "P", K = 100, se = TRUE, data = umf1, starts = coef(mod0.hn))


## +++++++++++++++++++++++++++++++
#         model ranking
## +++++++++++++++++++++++++++++++
modlist1 <- list(mod0.p.exp = mod0.p.exp, mod0.p.haz = mod0.p.haz, mod0.p.hn = mod0.p.hn,
                 mod0.nb.exp = mod0.nb.exp,mod0.nb.haz = mod0.nb.haz, mod0.nb.hn = mod0.nb.hn)
aictab(modlist1)

haz.shape <- exp(coef(mod0.nb.haz, type="det"))
haz.scale <- exp(coef(mod0.nb.haz, type="scale"))
# you need to change the detFun piece of this ("gxhn", "gxhaz" or whatever). Also 
# need to change the arguments inside it; ?detFuns for more info
plot(0:5, gxhaz(0:5, shape = haz.shape, scale = haz.scale), frame = F, type = "l", ylim = c(0,1), xlim = c(0,5),
     xlab = "Distance from Observer (m)", ylab = "Detection Probability", lwd = 3)

## +++++++++++++++++++++++++++++++
#         model ranking
## +++++++++++++++++++++++++++++++

p_null <- mod0.nb.haz

p_mssr <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~mssr,
                    keyfun = "haz", output = "density", unitsOut = "ha",
                    mixture = "P", K = 100, se = TRUE, data = umf1)

p_ord <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~ordinal,
                   keyfun = "haz", output = "density", unitsOut = "ha",
                   mixture = "P", K = 100, se = TRUE, data = umf1)

p_visit <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~visit,
                     keyfun = "haz", output = "density", unitsOut = "ha",
                     mixture = "P", K = 100, se = TRUE, data = umf1,
                     starts = coef(p_ord))

p_temp <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~temp,
                    keyfun = "haz", output = "density", unitsOut = "ha",
                    mixture = "P", K = 100, se = TRUE, data = umf1)

p_wind <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~wind,
                    keyfun = "haz", output = "density", unitsOut = "ha",
                    mixture = "P", K = 100, se = TRUE, data = umf1,
                    starts = coef(p_ord))

p_cloud <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~cloud,
                     keyfun = "haz", output = "density", unitsOut = "ha",
                     mixture = "P", K = 100, se = TRUE, data = umf1)

detmodlist <- list(p_null = p_null,
                   #p_mssr = p_mssr,
                   p_ord = p_ord, 
                   p_temp = p_temp, 
                   p_cloud = p_cloud, 
                   p_wind = p_wind,
                   p_visit = p_visit)
aictab(detmodlist)

###############################################################################
# Building models
###############################################################################

lam_null <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~scale(wind),
                      keyfun = "haz", output = "density", unitsOut = "ha",
                      mixture = "P", K = 100, se = TRUE, data = umf1)

lam_pract <- gdistsamp(lambdaformula = ~practice, phiformula = ~1, pformula = ~scale(wind),
                       keyfun = "haz", output = "density", unitsOut = "ha",
                       mixture = "P", K = 100, se = TRUE, data = umf1)

lam_treat <- gdistsamp(lambdaformula = ~treated, phiformula = ~1, pformula = ~scale(wind),
                       keyfun = "haz", output = "density", unitsOut = "ha",
                       mixture = "P", K = 100, se = TRUE, data = umf1)

lam_treat_pract <- gdistsamp(lambdaformula = ~treated + practice, phiformula = ~1, pformula = ~scale(wind),
                             keyfun = "haz", output = "density", unitsOut = "ha",
                             mixture = "P", K = 100, se = TRUE, data = umf1)

lam_flowers_treat <- gdistsamp(lambdaformula = ~scale(flowers) + treated, phiformula = ~1, pformula = ~scale(wind),
                               keyfun = "haz", output = "density", unitsOut = "ha",
                               mixture = "P", K = 100, se = TRUE, data = umf1)

lam_flowers_pract <- gdistsamp(lambdaformula = ~scale(flowers) + practice, phiformula = ~1, pformula = ~scale(wind),
                               keyfun = "haz", output = "density", unitsOut = "ha",
                               mixture = "P", K = 100, se = TRUE, data = umf1)

lam_flowers_pract_treat <- gdistsamp(lambdaformula = ~scale(flowers) + practice + treated, phiformula = ~1, pformula = ~scale(wind),
                                     keyfun = "haz", output = "density", unitsOut = "ha",
                                     mixture = "P", K = 100, se = TRUE, data = umf1)

lam_flowers <- gdistsamp(lambdaformula = ~scale(flowers), phiformula = ~1, pformula = ~scale(wind),
                         keyfun = "haz", output = "density", unitsOut = "ha",
                         mixture = "P", K = 100, se = TRUE, data = umf1)

lammodlist <- list(lam_null = lam_null, lam_pract = lam_pract,
                   lam_treat = lam_treat, lam_treat_pract = lam_treat_pract,
                   lam_flowers = lam_flowers, lam_flowers_treat = lam_flowers_treat, 
                   lam_flowers_pract = lam_flowers_pract, lam_flowers_pract_treat = lam_flowers_pract_treat)

aictab(lammodlist)

###############################################################################
# GOF
###############################################################################

fitstats <- function(fm) {
  observed <- getY(fm@data)
  expected <- fitted(fm)
  resids <- residuals(fm)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}

(pb <- parboot(lam_flowers_pract, fitstats, nsim=25, report=1))
(c.hat <- pb@t0[2]/mean(pb@t.star[,2])) # 1.019964 

##

ModelToPredict <- lam_flowers_pract
newdat1 = data.frame("practice" = unique(butts1$practice),
                     "flowers" = 0)
pred1 <- predict(ModelToPredict, type = "lambda", newdata = newdat1, append = T)

bp <- barplot(pred1$Predicted,
              names.arg = c("fire", "shrub", "planting", "grazing"),
              ylim = c(0, max(pred1$upper) + 20), # Adjusting y-axis to include error bars
              ylab = "Butterfly Density (95% CI)")

arrows(x0 = bp, y0 = pred1$lower, # Lower bounds
       x1 = bp, y1 = pred1$upper, # Upper bounds
       angle = 90, code = 3, length = 0.1)
abline(h=0)


##
min(umf1@siteCovs$flowers) # 0
max(umf1@siteCovs$flowers) # 356669

newdat1 = data.frame("practice" = "prescribed_burning",
                     "flowers" = seq(from = 0, to = 356669, length.out = 100))
pred1 <- predict(ModelToPredict, type = "lambda", newdata = newdat1, append = T)
plot(-1, -1, xlim = c(min(newdat1$flowers), max(newdat1$flowers)), 
     ylim = c(min(pred1$lower), max(pred1$upper)),
     ylab = "Butterfly Density (ind/ha)", xlab = "Flower Density (#/plot)")
lines(pred1$flowers, pred1$Predicted)
lines(pred1$flowers, pred1$lower, lty = 2)
lines(pred1$flowers, pred1$upper, lty = 2)

########### with pre/post

ModelToPredict <- lam_flowers_pract
practlist <- c(unique(butts1$practice)[1], unique(butts1$practice)[1],
               unique(butts1$practice)[2],unique(butts1$practice)[2],
               unique(butts1$practice)[3], unique(butts1$practice)[3],
               unique(butts1$practice)[4], unique(butts1$practice)[4])
newdat1 = data.frame("practice" = practlist,
                     "flowers" = 0)
pred1 <- predict(ModelToPredict, type = "lambda", newdata = newdat1, append = T)

bp <- barplot(pred1$Predicted,
              names.arg = c("fire", "shrub", "planting", "grazing"),
              ylim = c(0, max(pred1$upper) + 20), # Adjusting y-axis to include error bars
              ylab = "Butterfly Density (95% CI)")

arrows(x0 = bp, y0 = pred1$lower, # Lower bounds
       x1 = bp, y1 = pred1$upper, # Upper bounds
       angle = 90, code = 3, length = 0.1)
abline(h=0)

