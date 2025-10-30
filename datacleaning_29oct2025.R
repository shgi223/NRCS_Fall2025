install.packages("readxl"); install.packages("lubridate"); install.packages("suncalc")

library(readxl); library(lubridate); library(suncalc)

data1 <- readxl::read_xlsx("C:/Users/new user/Desktop/UPDATED/2024_CEAP_Pollinators_DataEntry_MASTER_9.12.2025.xlsx",
                           sheet = "pollinators")
#View(data1)
data1 <- as.data.frame(data1)

# make visitID column since we will pseudoreplicate point x visit
data1$survey <- paste0(data1$full_point_id, "_v", data1$visit)


# make an ordinal date column
data1$date <- make_date(year = data1$year, month = data1$month, day = data1$day)
data1$ordinal <- yday(data1$date)


# calculating minutes since sunrise
################################

# Create a date from the year and ordinal day
data1$date <- as.Date(data1$ordinal, origin = paste0(data1$year, "-01-01"))

# Use the suncalc package to get sunrise time for the specific date and location
# first, get sunrise times for every day - I used lat/long for Kansas City
data1$sunrise <- getSunlightTimes(date = data1$date, lat = 39.15, lon = -94.59, tz = "America/Chicago")$sunrise
data1$sunrise <- format(data1$sunrise, format = "%H:%M") # convert to HH:MM format

# convert "raw" times to actual time objects
# Pad with leading zeros to ensure 4-digit format
padded_times <- sprintf("%04d", as.numeric(data1$time))  # Ensure 4 digits (e.g., "0944" instead of "944")
time_objects <- strptime(padded_times, format = "%H%M")# Convert to a time object using strptime()
formatted_times <- format(time_objects, format = "%H:%M") # convert to HH:MM format so these match the sunrise times
data1$survtime <- formatted_times # add to data1

# converted to striptimes to calculate the difference in minutes
sunrise_posix <- strptime(paste("2023-01-01", data1$sunrise), format = "%Y-%m-%d %H:%M")
survey_posix <- strptime(paste("2023-01-01", formatted_times), format = "%Y-%m-%d %H:%M")

# Calculate the difference in minutes
data1$mssr <- as.numeric(difftime(survey_posix, sunrise_posix, units = "mins"))


# Flowers
################################

flowers1 <- readxl::read_xlsx("C:/Users/new user/Desktop/UPDATED/2024_CEAP_Pollinators_DataEntry_MASTER_9.12.2025.xlsx",
                              sheet = "flowers")

flowers1 <- as.data.frame(flowers1)
head(flowers1)
flowers1$survey_id <- paste0(flowers1$full_point_id, "_v", flowers1$visit)
flowerdens <- data.frame("survey_id" = 0, "NoFlowers" = 0)

for(i in 1:length(unique(flowers1$survey_id))){
  survey_i <- unique(flowers1$survey_id)[i]
  survey_i_data <- subset(flowers1, survey_id == survey_i)
  NoFlowers_i <- sum(survey_i_data$flowers)
  newrow <- data.frame("survey_id" = survey_i, "NoFlowers" = NoFlowers_i)
  flowerdens <- rbind(flowerdens, newrow)
}
flowerdens <- flowerdens[2:nrow(flowerdens),]
rownames(flowerdens) <- 1:nrow(flowerdens)

# Veg
################################

veg1 <- readxl::read_xlsx("C:/Users/new user/Desktop/UPDATED/2024_CEAP_Pollinators_DataEntry_MASTER_9.12.2025.xlsx",
                          sheet = "veg")

veg1 <- as.data.frame(veg1)
veg1 <- veg1[,c(3,5,6,30:44)]
head(veg1)

###############################
# visualizing distances
# note - butterflies and bees are separated at this point
unique(data1$distance) # all unique distances
data1$distance[data1$distance == "NA"] <- NA # turn text NAs to real NAs
data1$distance <- as.numeric(data1$distance) #convert to numeric
round(data1$distance, digits = 3) # round values to have 2 decimal points

# bees first
bees1 <- subset(data1, morphospecies != "survey_not_conducted"& morphospecies != "BUTT") # remove non-surveys and butterflies
unique(bees1$morphospecies) # what is left?
beesamp90 <- length(bees1$distance) * 0.90 # number of observations to get 90% for dist cutoff
smallest_values <- sort(bees1$distance)[1:round(beesamp90,0)]
hist(smallest_values, main = "Closest 90% of Bee Records", xlab = "Bee detection distances (m from transect)")
# seems like 5 bins, each 1m wide would be appropriate

# next, butterflies
butts1 <- subset(data1, morphospecies == "no_bees" | morphospecies == "BUTT")
unique(butts1$morphospecies)
buttsamp90 <- length(butts1$distance) * 0.90 # number of observations to get 90% for dist cutoff
smallest_values <- sort(butts1$distance)[1:round(buttsamp90,0)]
hist(smallest_values, main = "Closest 90% of Butterfly Records", xlab = "Butterfly detection distances (m from transect)")
# seems like 5 bins, each 2m wide would be appropriate

# formatting distances -- Bees first
################################

# establishing the list of surveys
surveys <- unique(data1$survey)
df1 <- data.frame("survey" = 0,
                  "practice" = 0,
                  "treated" = 0,
                  "flowers" = 0,
                  "visit" = 0,
                  "observer" = 0,
                  "temp" = 0,
                  "wind" = 0,
                  "cloud" = 0,
                  "ordinal" = 0,
                  "mssr" = 0,
                  "dist1" = 0, "dist2" = 0, "dist3" = 0,
                  "dist4" = 0, "dist5" = 0,
                  "perc_canopy"  = 0,        
                  "perc_tallsap" = 0,  
                  "perc_shortsap" = 0,          
                  "perc_tallshrub" = 0, 
                  "perc_shortshrub" = 0, 
                  "perc_forb" = 0, 
                  "perc_tallgrass" = 0, 
                  "perc_shortgrass" = 0, 
                  "perc_CWD" = 0, 
                  "perc_litter" = 0, 
                  "perc_bare" = 0, 
                  "perc_anysap" = 0, 
                  "perc_anyshrub" = 0, 
                  "perc_anywoody" = 0, 
                  "perc_anygrass" = 0)

#note, we need both of these:
head(data1)
head(bees1)

for(i in 1:length(surveys)){
  #i = 101 ## I do not recall why we use i= 101 here or why we put hashtag in front until we run it...
  survi <- surveys[i]
  datai <- subset(data1, survey == survi) # for metadata
  beesi <- subset(bees1, survey == survi) # for bee data
  practi <- datai$conservation_practice[1]
  treatedi <- datai$treatment_status[1]
  visiti <- datai$visit[1]
  obsi <- datai$observer[1]
  tempi <- datai$temp_f[1]
  windi <- datai$wind[1]
  cloudi <- datai$cloud[1]
  ordinali <- datai$ordinal[1]
  mssri <- datai$mssr[1]
  
  # flowers
  flowers_i <- subset(flowerdens, survey_id == survi)[2]
  flowers_i <- as.numeric(flowers_i)
  
  # veg
  sitei <- substr(survi, 1, 10)
  vegi <- subset(veg1, full_point_id == sitei)
  
  # note - using "beesi" instead of "datai"
  dist1i = nrow(subset(beesi, distance > -0.01 & distance < 1))
  dist2i = nrow(subset(beesi, distance > 0.99 & distance < 2))
  dist3i = nrow(subset(beesi, distance > 1.99 & distance < 3))
  dist4i = nrow(subset(beesi, distance > 2.99 & distance < 4))
  dist5i = nrow(subset(beesi, distance > 3.99 & distance < 5))
  newrow <- data.frame("survey" = survi,
                       "practice" = practi,
                       "treated" = treatedi,
                       "flowers" = flowers_i,
                       "visit" = visiti,
                       "observer" = obsi,
                       "temp" = tempi,
                       "wind" = windi,
                       "cloud" = cloudi,
                       "ordinal" = ordinali,
                       "mssr" = mssri,
                       "dist1" = dist1i, "dist2" = dist2i, "dist3" = dist3i,
                       "dist4" = dist4i, "dist5" = dist5i,
                       "perc_canopy"  = vegi$perc_canopy,        
                       "perc_tallsap" = vegi$perc_tallsap,  
                       "perc_shortsap" = vegi$perc_shortsap,          
                       "perc_tallshrub" = vegi$perc_tallshrub, 
                       "perc_shortshrub" = vegi$perc_shortshrub, 
                       "perc_forb" = vegi$perc_forb, 
                       "perc_tallgrass" = vegi$perc_tallgrass, 
                       "perc_shortgrass" = vegi$perc_shortgrass, 
                       "perc_CWD" = vegi$perc_CWD, 
                       "perc_litter" = vegi$perc_litter, 
                       "perc_bare" = vegi$perc_bare, 
                       "perc_anysap" = vegi$perc_anysap, 
                       "perc_anyshrub" = vegi$perc_anyshrub, 
                       "perc_anywoody" = vegi$perc_anywoody, 
                       "perc_anygrass" = vegi$perc_anygrass)
  df1 <- rbind(df1, newrow)
}
df1 <- df1[2:nrow(df1),]
rownames(df1) <- 1:nrow(df1)

# plot
sums <- colSums(df1[,12:16])
barplot(sums, names.arg = names(df1[,12:16]), xlab = "Distance", ylab = "# Observations", main = "Bee data")
abline(h=0)
write.csv(df1, "C:/Users/new user/Desktop/NRCS Pollinators/analyses/bees2024-25.csv", row.names = F)

# formatting distances -- Butterflies next
################################

# establishing the list of surveys
surveys <- unique(data1$survey)
df2 <- data.frame("survey" = 0,
                  "practice" = 0,
                  "treated" = 0,
                  "flowers" = 0,
                  "visit" = 0,
                  "observer" = 0,
                  "temp" = 0,
                  "wind" = 0,
                  "cloud" = 0,
                  "ordinal" = 0,
                  "mssr" = 0,
                  "dist1" = 0, "dist2" = 0, "dist3" = 0,
                  "dist4" = 0, "dist5" = 0,
                  "perc_canopy"  = 0,        
                  "perc_tallsap" = 0,  
                  "perc_shortsap" = 0,          
                  "perc_tallshrub" = 0, 
                  "perc_shortshrub" = 0, 
                  "perc_forb" = 0, 
                  "perc_tallgrass" = 0, 
                  "perc_shortgrass" = 0, 
                  "perc_CWD" = 0, 
                  "perc_litter" = 0, 
                  "perc_bare" = 0, 
                  "perc_anysap" = 0, 
                  "perc_anyshrub" = 0, 
                  "perc_anywoody" = 0, 
                  "perc_anygrass" = 0)

#note, we need both of these:
head(data1)
head(butts1)

for(i in 1:length(surveys)){
  #i = 101
  survi <- surveys[i]
  datai <- subset(data1, survey == survi) # for metadata
  buttsi <- subset(butts1, survey == survi) # for bee data
  practi <- datai$conservation_practice[1]
  treatedi <- datai$treatment_status[1]
  visiti <- datai$visit[1]
  obsi <- datai$observer[1]
  tempi <- datai$temp_f[1]
  windi <- datai$wind[1]
  cloudi <- datai$cloud[1]
  ordinali <- datai$ordinal[1]
  mssri <- datai$mssr[1]
  
  # flowers
  flowers_i <- subset(flowerdens, survey_id == survi)[2]
  flowers_i <- as.numeric(flowers_i)
  
  # veg
  sitei <- substr(survi, 1, 10)
  vegi <- subset(veg1, full_point_id == sitei)
  
  # note - using "buttsi" instead of "datai"
  dist1i = nrow(subset(buttsi, distance > -0.01 & distance < 2)) ## NOTE DISTANCE DIFFS FROM BEES!
  dist2i = nrow(subset(buttsi, distance > 1.99 & distance < 4))
  dist3i = nrow(subset(buttsi, distance > 3.99 & distance < 6))
  dist4i = nrow(subset(buttsi, distance > 5.99 & distance < 8))
  dist5i = nrow(subset(buttsi, distance > 7.99 & distance < 10))
  newrow <- data.frame("survey" = survi,
                       "practice" = practi,
                       "treated" = treatedi,
                       "flowers" = flowers_i,
                       "visit" = visiti,
                       "observer" = obsi,
                       "temp" = tempi,
                       "wind" = windi,
                       "cloud" = cloudi,
                       "ordinal" = ordinali,
                       "mssr" = mssri,
                       "dist1" = dist1i, "dist2" = dist2i, "dist3" = dist3i,
                       "dist4" = dist4i, "dist5" = dist5i,
                       "perc_canopy"  = vegi$perc_canopy,        
                       "perc_tallsap" = vegi$perc_tallsap,  
                       "perc_shortsap" = vegi$perc_shortsap,          
                       "perc_tallshrub" = vegi$perc_tallshrub, 
                       "perc_shortshrub" = vegi$perc_shortshrub, 
                       "perc_forb" = vegi$perc_forb, 
                       "perc_tallgrass" = vegi$perc_tallgrass, 
                       "perc_shortgrass" = vegi$perc_shortgrass, 
                       "perc_CWD" = vegi$perc_CWD, 
                       "perc_litter" = vegi$perc_litter, 
                       "perc_bare" = vegi$perc_bare, 
                       "perc_anysap" = vegi$perc_anysap, 
                       "perc_anyshrub" = vegi$perc_anyshrub, 
                       "perc_anywoody" = vegi$perc_anywoody, 
                       "perc_anygrass" = vegi$perc_anygrass)
  df2 <- rbind(df2, newrow)
}
df2 <- df2[2:nrow(df2),]
rownames(df2) <- 1:nrow(df2)

# plot
sums <- colSums(df2[,12:16])
barplot(sums, names.arg = names(df2[,12:16]), xlab = "Distance", ylab = "# Observations", main = "Bufferfly Data")
abline(h=0)

write.csv(df2, "C:/Users/new user/Desktop/NRCS Pollinators/analyses/butts2024-25.csv", row.names = F)
