library(unmarked); library(AICcmodavg)

# read in data and create unmarked frame
################################################################################
bees1 <- read.csv("C:/Users/new user/Desktop/NRCS Pollinators/analyses/bees2024-25.csv")
nrow(bees1)
head(bees1)
unique(bees1$conservation_practice)
bees1 <- subset(bees1, conservation_practice != "not_enrolled_in_NRCS")
nrow(bees1)

# missing flower values with mean
################################################################################
bees1$flowers[is.na(bees1$flowers)] <- mean(bees1$flowers, na.rm = TRUE)

# prep data for unmarked frame
################################################################################
bee.obs <- as.matrix(bees1[,12:16]) # detection data 

breaks <- c(0,1,2,3,4,5) # this needs to be length = J + 1 (J= no. of distance classes)

length(bees1$conservation_practice)
length(bees1$treatment_status)
length(bees1$)
# site covariates
sitecovs1 <- data.frame(
  ######################## site covariates
  "practice" = as.factor(bees1$conservation_practice),
  "treated" = as.factor(bees1$treatment_status),
  "flowers" = as.numeric(bees1$flowers),
  
  # veg
  "canopy" = as.numeric(bees1$perc_canopy),
  "tallsap" = as.numeric(bees1$perc_tallsap),
  "shortsap" = as.numeric(bees1$perc_shortsap),
  "tallshrub" = as.numeric(bees1$perc_tallshrub),
  "shortshrub" = as.numeric(bees1$perc_shortshrub),      
  "forb" = as.numeric(bees1$perc_forb),
  "tallgrass" = as.numeric(bees1$perc_tallgrass),
  "shortgrass" = as.numeric(bees1$perc_shortgrass),
  "CWD" = as.numeric(bees1$perc_CWD),
  "litter" = as.numeric(bees1$perc_litter),
  "bare" = as.numeric(bees1$perc_bare),
  "sap" = as.numeric(bees1$perc_anysap),
  "shrub" = as.numeric(bees1$perc_anyshrub),
  "woody" = as.numeric(bees1$perc_anywoody),
  "grass" = as.numeric(bees1$perc_anygrass),
  
  ######################## survey covariates
  "visit" = as.numeric(bees1$visit),
  "temp" = bees1$temp,
  "observer" = as.factor(bees1$observer),
  "wind" = bees1$wind,
  "cloud" = bees1$cloud,
  "ordinal" = bees1$ordinal,
  "mssr" = bees1$mssr)

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
summary(siteCovs(umf1)) #Something is weird here, only 846 observations that all say observer = SG???

mod0.exp <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                      keyfun = "exp", output = "density", unitsOut = "ha",
                      mixture = "P", K = 100, se = TRUE, data = umf1)

mod0.haz <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                      keyfun = "haz", output = "density", unitsOut = "ha",
                      mixture = "P", K = 100, se = TRUE, data = umf1)


####First error begins below... #####

mod0.hn <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                     keyfun = "halfnorm", output = "density", unitsOut = "ha",
                     mixture = "P", K = 100, se = TRUE, data = umf1)

#mod0.uni <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
#                      keyfun = "uniform", output = "density", unitsOut = "ha",
#                     mixture = "P", K = 100, se = TRUE, data = umf1, starts = coef(mod0.exp))

## +++++++++++++++++++++++++++++++
#         model ranking
## +++++++++++++++++++++++++++++++

modlist1 <- list(mod0.exp = mod0.exp, mod0.haz = mod0.haz, mod0.hn = mod0.hn)
aictab(modlist1)

mod0.exp@estimates[1] # 4.81; this is the shape/rate for an exp model
plot(0:5, gxexp(0:5, rate = 4.81), frame = F, type = "l", ylim = c(0, 1), xlim = c(0, 5),# Generate the plot
     xlab = "Distance from Observer (m)", ylab = "Detection Probability", lwd = 3)

## +++++++++++++++++++++++++++++++
#         model ranking
## +++++++++++++++++++++++++++++++

p_null <- mod0.exp

p_mssr <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~mssr,
                    keyfun = "exp", output = "density", unitsOut = "ha",
                    mixture = "P", K = 100, se = TRUE, data = umf1)

p_ord <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~ordinal,
                   keyfun = "exp", output = "density", unitsOut = "ha",
                   mixture = "P", K = 100, se = TRUE, data = umf1)

p_visit <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~visit,
                     keyfun = "exp", output = "density", unitsOut = "ha",
                     mixture = "P", K = 100, se = TRUE, data = umf1,
                     starts = coef(p_ord))

p_temp <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~temp,
                    keyfun = "exp", output = "density", unitsOut = "ha",
                    mixture = "P", K = 100, se = TRUE, data = umf1)

p_wind <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~wind,
                    keyfun = "exp", output = "density", unitsOut = "ha",
                    mixture = "P", K = 100, se = TRUE, data = umf1,
                    starts = coef(p_ord))

p_cloud <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~cloud,
                     keyfun = "exp", output = "density", unitsOut = "ha",
                     mixture = "P", K = 100, se = TRUE, data = umf1)

detmodlist <- list(p_null = p_null,
                   p_mssr = p_mssr,
                   p_ord = p_ord, 
                   p_temp = p_temp, 
                   p_cloud = p_cloud, 
                   p_wind = p_wind, ###object "p_wind" not found???
                   p_visit = p_visit)
aictab(detmodlist)

###############################################################################
# Building models
###############################################################################

lam_null <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~temp,
                      keyfun = "exp", output = "density", unitsOut = "ha",
                      mixture = "P", K = 100, se = TRUE, data = umf1)

lam_pract <- gdistsamp(lambdaformula = ~practice, phiformula = ~1, pformula = ~temp,
                       keyfun = "exp", output = "density", unitsOut = "ha",
                       mixture = "P", K = 100, se = TRUE, data = umf1)

lam_treat <- gdistsamp(lambdaformula = ~treated, phiformula = ~1, pformula = ~temp,
                       keyfun = "exp", output = "density", unitsOut = "ha",
                       mixture = "P", K = 100, se = TRUE, data = umf1)

lam_treat_pract <- gdistsamp(lambdaformula = ~treated + practice, phiformula = ~1, pformula = ~temp,
                             keyfun = "exp", output = "density", unitsOut = "ha",
                             mixture = "P", K = 100, se = TRUE, data = umf1)

lam_flowers_treat <- gdistsamp(lambdaformula = ~scale(flowers) + treated, phiformula = ~1, pformula = ~temp,
                               keyfun = "exp", output = "density", unitsOut = "ha",
                               mixture = "P", K = 100, se = TRUE, data = umf1)

lam_flowers_pract <- gdistsamp(lambdaformula = ~scale(flowers) + practice, phiformula = ~1, pformula = ~temp,
                               keyfun = "exp", output = "density", unitsOut = "ha",
                               mixture = "P", K = 100, se = TRUE, data = umf1)

lam_flowers_pract_treat <- gdistsamp(lambdaformula = ~scale(flowers) + practice + treated, phiformula = ~1, pformula = ~temp,
                                     keyfun = "exp", output = "density", unitsOut = "ha",
                                     mixture = "P", K = 100, se = TRUE, data = umf1)

lam_flowers <- gdistsamp(lambdaformula = ~scale(flowers), phiformula = ~1, pformula = ~temp,
                         keyfun = "exp", output = "density", unitsOut = "ha",
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
