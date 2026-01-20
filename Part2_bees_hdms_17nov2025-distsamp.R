library(unmarked); library(AICcmodavg)

# read in data and create unmarked frame
################################################################################
bees2 <- read.csv("~/bees2024-25.csv") 

nrow(bees2)
head(bees2)
unique(bees2$practice)
bees2 <- subset(bees2, practice != "not_enrolled_in_NRCS")
bees2 <- subset(bees2, survey != "mo_priv022_v2_y2025") # Remove MO_022_02
bees2 <- subset(bees2, treated == "pre" | treated == "post")
nrow(bees2)

# missing flower values with mean
################################################################################
bees2$flowers[is.na(bees2$flowers)] <- mean(bees2$flowers, na.rm = TRUE)

# missing cloud cover with mean
################################################################################
bees2$cloud[is.na(bees2$cloud)] <- mean(bees2$cloud, na.rm = TRUE)

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

umf1 <- unmarkedFrameDS(y = bee.obs, 
                        survey = "line",
                        tlength = rep(50, nrow(bee.obs)),
                        unitsIn = "m",
                        dist.breaks = breaks, 
                        siteCovs = sitecovs1)
summary(umf1)
summary(siteCovs(umf1)) 

######Make sure everything above is correct !!!

mod0.exp <- distsamp(~1 ~1, data = umf1, keyfun = 'exp')
mod0.haz <- distsamp(~1 ~1, data = umf1, keyfun = 'hazard')
mod0.uni <- distsamp(~1 ~1, data = umf1, keyfun = 'uniform')
mod0.hn <- distsamp(~1 ~1, data = umf1, keyfun = 'halfnorm')

modlist1 <- list(mod0.exp = mod0.exp, mod0.haz = mod0.haz, mod0.hn = mod0.hn,
                 mod0.uni = mod0.uni)
aictab(modlist1)

## +++++++++++++++++++++++++++++++
#         model ranking
## +++++++++++++++++++++++++++++++

# what is scaling?
par(mfrow = c(1,2))
hist(umf1@siteCovs$mssr, breaks = 10)
hist(scale(umf1@siteCovs$mssr), breaks = 10)
par(mfrow = c(1,1))

p_null <- distsamp(~1 ~1, data = umf1, keyfun = 'hazard')
p_mssr <- distsamp(~scale(mssr) ~1, data = umf1, keyfun = 'hazard')
p_ord <- distsamp(~scale(ordinal) ~1, data = umf1, keyfun = 'hazard')
p_vis <- distsamp(~scale(visit) ~1, data = umf1, keyfun = 'hazard')
p_temp <- distsamp(~scale(temp) ~1, data = umf1, keyfun = 'hazard')
p_wind <- distsamp(~scale(wind) ~1, data = umf1, keyfun = 'hazard')
p_cloud <- distsamp(~scale(cloud) ~1, data = umf1, keyfun = 'hazard')


# this is how you provide starting values... may need to come back to this
#p_cloud2 <- distsamp(~cloud ~1, data = umf1, keyfun = 'hazard', starts=(coef(p_cloud)))

detmodlist <- list(p_null = p_null,
                   p_mssr = p_mssr,
                   p_ord = p_ord, 
                   p_temp = p_temp, 
                   p_cloud = p_cloud, 
                   p_wind = p_wind, 
                   p_vis = p_vis)
aictab(detmodlist)

###############################################################################
# Building models
###############################################################################

lam_null <- distsamp(~scale(wind) ~1, data = umf1, keyfun = 'hazard')
lam_pract <- distsamp(~scale(wind) ~practice, data = umf1, keyfun = 'hazard')
lam_treat <- distsamp(~scale(wind) ~treated, data = umf1, keyfun = 'hazard')
lam_pract_treat <- distsamp(~scale(wind) ~practice + treated, data = umf1, keyfun = 'hazard')
lam_pract_x_treat <- distsamp(~scale(wind) ~practice * treated, data = umf1, keyfun = 'hazard')

lammodlist <- list(lam_null = lam_null, 
                   lam_pract = lam_pract, 
                   lam_treat = lam_treat, 
                   lam_pract_treat = lam_pract_treat, 
                   lam_pract_x_treat = lam_pract_x_treat)

aictab(lammodlist, c.hat= 1.798313)

###############################################################################

umf1@siteCovs$flowers2 <- scale(log(umf1@siteCovs$flowers + 1)) # bc flowers is super skewed

lam_pract_x_treat <- distsamp(~scale(wind) ~practice * treated, data = umf1, keyfun = 'hazard')
lam_pract_x_treat_flow <- distsamp(~scale(wind) ~scale(log(flowers + 1)) + practice * treated, data = umf1, keyfun = 'hazard')

# does adding flowers help?
lammodlist2 <- list(lam_pract_x_treat = lam_pract_x_treat, 
                    lam_pract_x_treat_flow = lam_pract_x_treat_flow)

aictab(lammodlist2, c.hat= 1.798313)

###############################################################################

ModelToPredict <- lam_pract_x_treat
practlist <- c(unique(bees2$practice)[1], unique(bees2$practice)[1],
               unique(bees2$practice)[2],unique(bees2$practice)[2],
               unique(bees2$practice)[3], unique(bees2$practice)[3],
               unique(bees2$practice)[4], unique(bees2$practice)[4])
newdat1 = data.frame("practice" = practlist,
                     "treated" = c("pre", "post"))

pred1 <- predict(ModelToPredict, type = "state", newdata = newdat1, append = T)
pred1
#
treatment_colors <- rep(c("tomato", "lightgreen", "lightpink", "lightyellow"), each = 2)

bp <- barplot(pred1$Predicted,
              col = treatment_colors,
              names.arg = c("pre", "post", "pre", "post", "pre", "post", "pre", "post"),
              ylim = c(0, max(pred1$upper) + 20), # Adjusting y-axis to include error bars
              ylab = "Bee Density (95% CI)")

arrows(x0 = bp, y0 = pred1$lower, # Lower bounds
       x1 = bp, y1 = pred1$upper, # Upper bounds
       angle = 90, code = 3, length = 0.1)
abline(h=0)

# Add custom treatment labels beneath the x-axis
treatment_labels <- c("Presc. fire", "Brush mgmt.", 
                      "Wild. Hab. Pl.", "Presc. graz.")

# Place treatment labels centered under each treatment group
midpoints <- tapply(bp, rep(1:4, each = 2), mean)  # Get the midpoint for each pair
text(x = midpoints, y = -65, labels = treatment_labels, xpd = TRUE)  # Adjust y for spacing

###############################################################################

lam_pract_x_treat <- distsamp(~scale(wind) ~practice * treated, data = umf1, keyfun = 'hazard')
lam_pract_x_treat_flow <- distsamp(~scale(wind) ~scale(flowers) + practice * treated, data = umf1, keyfun = 'hazard')

# does adding flowers help?
lammodlist2 <- list(lam_pract_x_treat = lam_pract_x_treat, 
                   lam_pract_x_treat_flow = lam_pract_x_treat_flow)
                   
aictab(lammodlist2, c.hat= 1.798313) # no longer improves things

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

##Getting errors here###
(pb <- parboot(lam_pract_x_treat_flow, fitstats, nsim=25, report=1)) #changed to lam_pract_x_treat_flow from lam_pract_trt_flowers
(c.hat <- pb@t0[2]/mean(pb@t.star[,2])) # 1.798313

###############################################################################
#
#
#
#   SCRAP CODE BELOW THIS POINT
#
#
#
###############################################################################

ModelToPredict <- lam_pract_flowers
newdat1 = data.frame("practice" = unique(bees2$practice),
                     "flowers" = 0)
pred1 <- predict(ModelToPredict, type = "state", newdata = newdat1, append = T)

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

pred1 <- predict(ModelToPredict, type = "state", newdata = newdat1, append = T)

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

