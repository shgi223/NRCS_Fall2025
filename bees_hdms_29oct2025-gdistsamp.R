library(unmarked); library(AICcmodavg)

# read in data and create unmarked frame
################################################################################
bees2 <- read.csv("~/bees2024-25.csv") 

nrow(bees2)
head(bees2)
unique(bees2$practice)
bees2 <- subset(bees2, practice != "not_enrolled_in_NRCS")
bees2 <- subset(bees2, survey != "mo_priv022_v2_y2025") # Remove MO_022_02
nrow(bees2)

# missing flower values with mean
################################################################################
bees2$flowers[is.na(bees2$flowers)] <- mean(bees2$flowers, na.rm = TRUE)

# prep data for unmarked frame
################################################################################
bee.obs <- as.matrix(bees2[,c("dist1", "dist2", "dist3", "dist4", "dist5")]) # detection data 

breaks <- c(0,1,2,3,4,5) # this needs to be length = J + 1 (J= no. of distance classes)

###### Stuff skylar is adding in to try and figure this out but is not part of code
length(bees2$practice)
length(bees2$treated)
#length(flowerdens$NoFlowers)
#length(flowerdens$survey_id)

unique(bees2$observer)


#######

# site covariates
sitecovs1 <- data.frame(
  ######################## site covariates
  "practice" = as.factor(bees2$practice),
  "treated" = as.factor(bees2$treated),
  "flowers" = as.numeric(bees2$flowers),
  
  # veg
  "canopy" = as.numeric(bees2$perc_canopy),
  "tallsap" = as.numeric(bees2$perc_tallsap),
  "shortsap" = as.numeric(bees2$perc_shortsap),
  "tallshrub" = as.numeric(bees2$perc_tallshrub),
  "shortshrub" = as.numeric(bees2$perc_shortshrub),      
  "forb" = as.numeric(bees2$perc_forb),
  "tallgrass" = as.numeric(bees2$perc_tallgrass),
  "shortgrass" = as.numeric(bees2$perc_shortgrass),
  "CWD" = as.numeric(bees2$perc_CWD),
  "litter" = as.numeric(bees2$perc_litter),
  "bare" = as.numeric(bees2$perc_bare),
  "sap" = as.numeric(bees2$perc_anysap),
  "shrub" = as.numeric(bees2$perc_anyshrub),
  "woody" = as.numeric(bees2$perc_anywoody),
  "grass" = as.numeric(bees2$perc_anygrass),
  
  ######################## survey covariates
  "visit" = as.numeric(bees2$visit),
  "temp" = bees2$temp,
  "observer" = as.factor(bees2$observer),
  "wind" = bees2$wind,
  "cloud" = bees2$cloud,
  "ordinal" = bees2$ordinal,
  "mssr" = bees2$mssr)

# creating unmarked frame

umf1 <- unmarkedFrameGDS(y = bee.obs, 
                         survey = "line",
                         tlength = rep(50, nrow(bee.obs)),
                         unitsIn = "m",
                         dist.breaks = breaks, 
                         numPrimary = 1, 
                         siteCovs = sitecovs1, 
                         yearlySiteCovs = sitecovs1)

summary(umf1)
summary(siteCovs(umf1)) 


######Make sure everything above is correct !!!

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

## +++++++++++++++++++++++++++++++
#         model ranking
## +++++++++++++++++++++++++++++++

modlist1 <- list(mod0.p.exp = mod0.p.exp, mod0.p.haz = mod0.p.haz, mod0.p.hn = mod0.p.hn,
                 mod0.nb.exp = mod0.nb.exp, mod0.nb.haz = mod0.nb.haz, mod0.nb.hn = mod0.nb.hn)
aictab(modlist1)

#mod0.nb.exp@estimates[1] # 4.81; this is the shape/rate for an exp model
#plot(0:5, gxexp(0:5, rate = 4.81), frame = F, type = "l", ylim = c(0, 1), xlim = c(0, 5),# Generate the plot
#     xlab = "Distance from Observer (m)", ylab = "Detection Probability", lwd = 3)

## +++++++++++++++++++++++++++++++
#         model ranking
## +++++++++++++++++++++++++++++++

p_null <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                    keyfun = "haz", output = "density", unitsOut = "ha",
                    mixture = "NB", K = 100, se = TRUE, data = umf1)

#p_mssr <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~mssr,
#                    keyfun = "haz", output = "density", unitsOut = "ha",
#                    mixture = "NB", K = 100, se = TRUE, data = umf1)

p_ord <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~ordinal,
                   keyfun = "haz", output = "density", unitsOut = "ha",
                   mixture = "NB", K = 100, se = TRUE, data = umf1)

p_visit <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~visit,
                     keyfun = "haz", output = "density", unitsOut = "ha",
                     mixture = "NB", K = 100, se = TRUE, data = umf1)

p_temp <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~temp,
                    keyfun = "haz", output = "density", unitsOut = "ha",
                    mixture = "NB", K = 100, se = TRUE, data = umf1)

p_wind <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~wind,
                    keyfun = "haz", output = "density", unitsOut = "ha",
                    mixture = "NB", K = 100, se = TRUE, data = umf1)

p_cloud <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~cloud,
                     keyfun = "haz", output = "density", unitsOut = "ha",
                     mixture = "NB", K = 100, se = TRUE, data = umf1)

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

lam_null <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~cloud,
                      keyfun = "haz", output = "density", unitsOut = "ha",
                      mixture = "NB", K = 100, se = TRUE, data = umf1)

lam_pract <- gdistsamp(lambdaformula = ~practice, phiformula = ~1, pformula = ~cloud,
                       keyfun = "haz", output = "density", unitsOut = "ha",
                       mixture = "NB", K = 100, se = TRUE, data = umf1)

lam_treat <- gdistsamp(lambdaformula = ~treated, phiformula = ~1, pformula = ~cloud,
                       keyfun = "haz", output = "density", unitsOut = "ha",
                       mixture = "NB", K = 100, se = TRUE, data = umf1)

lam_treat_pract <- gdistsamp(lambdaformula = ~treated + practice, phiformula = ~1, pformula = ~cloud,
                             keyfun = "haz", output = "density", unitsOut = "ha",
                             mixture = "NB", K = 100, se = TRUE, data = umf1)

lam_flowers_treat <- gdistsamp(lambdaformula = ~scale(flowers) + treated, phiformula = ~1, pformula = ~cloud,
                               keyfun = "haz", output = "density", unitsOut = "ha",
                               mixture = "NB", K = 100, se = TRUE, data = umf1)

lam_flowers_pract <- gdistsamp(lambdaformula = ~scale(flowers) + practice, phiformula = ~1, pformula = ~cloud,
                               keyfun = "haz", output = "density", unitsOut = "ha",
                               mixture = "NB", K = 100, se = TRUE, data = umf1)

lam_flowers_pract_treat <- gdistsamp(lambdaformula = ~scale(flowers) + practice + treated, phiformula = ~1, pformula = ~cloud,
                                     keyfun = "haz", output = "density", unitsOut = "ha",
                                     mixture = "NB", K = 100, se = TRUE, data = umf1)

lam_flowers <- gdistsamp(lambdaformula = ~scale(flowers), phiformula = ~1, pformula = ~cloud,
                         keyfun = "haz", output = "density", unitsOut = "ha",
                         mixture = "NB", K = 100, se = TRUE, data = umf1)

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
(c.hat <- pb@t0[2]/mean(pb@t.star[,2])) # 1.003315 

##

ModelToPredict <- lam_flowers_pract
newdat1 = data.frame("practice" = unique(bees1$practice),
                     "flowers" = 0)
pred1 <- predict(ModelToPredict, type = "lambda", newdata = newdat1, append = T)

bp <- barplot(pred1$Predicted,
              names.arg = c("fire", "shrub", "planting", "grazing"),
              ylim = c(0, max(pred1$upper) + 20), # Adjusting y-axis to include error bars
              ylab = "Bee Density (95% CI)")

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
     ylab = "Bee Density (ind/ha)", xlab = "Flower Density (#/plot)")
lines(pred1$flowers, pred1$Predicted)
lines(pred1$flowers, pred1$lower, lty = 2)
lines(pred1$flowers, pred1$upper, lty = 2)

########### with pre/post

ModelToPredict <- lam_flowers_pract
practlist <- c(unique(bees1$practice)[1], unique(bees1$practice)[1],
               unique(bees1$practice)[2],unique(bees1$practice)[2],
               unique(bees1$practice)[3], unique(bees1$practice)[3],
               unique(bees1$practice)[4], unique(bees1$practice)[4])
newdat1 = data.frame("practice" = practlist,
                     "flowers" = 0)
pred1 <- predict(ModelToPredict, type = "lambda", newdata = newdat1, append = T)

bp <- barplot(pred1$Predicted,
              names.arg = c("pre", "post", "pre", "post", "pre", "post", "pre", "post"),
              ylim = c(0, max(pred1$upper) + 20), # Adjusting y-axis to include error bars
              ylab = "Bee Density (95% CI)")

arrows(x0 = bp, y0 = pred1$lower, # Lower bounds
       x1 = bp, y1 = pred1$upper, # Upper bounds
       angle = 90, code = 3, length = 0.1)
abline(h=0)
