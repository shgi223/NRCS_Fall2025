###############
#Getting files ready 

#beeID <- readxl::read_xlsx("./2024-25_CEAP_MASTER_9.12.2025.xlsx", sheet = "beeID2024-25")

#putting survey ID on pollinator surveys
#pollinatorID <- readxl::read_xlsx("./2024-25_CEAP_MASTER_9.12.2025.xlsx", sheet = "pollinators")
#pollinatorID$survey <- paste0(pollinatorID$full_point_id, "_v", pollinatorID$visit, "_y", pollinatorID$year)
#pollinatorID <- as.data.frame(pollinatorID)
#write.csv(pollinatorID, "C:/Users/new user/Desktop/NRCS_Fall2025/pollinatorID.csv", row.names = F)

#putting survey ID on floral surveys
#flowID <- readxl::read_xlsx("./2024-25_CEAP_MASTER_9.12.2025.xlsx", sheet = "flowers")
#flowID$survey <- paste0(flowID$full_point_id, "_v", flowID$visit, "_y", flowID$year)
#flowID <- as.data.frame(flowID)
#write.csv(flowID, "C:/Users/new user/Desktop/NRCS_Fall2025/flowID.csv", row.names = F)



###############
#Cleaning Code ###
#beeID <- read.csv("C:/Users/new user/Desktop/NRCS_Fall2025/beeID_2024-25.csv")
beeID <- readxl::read_xlsx("./2024-25_CEAP_MASTER_12.2.2025.xlsx", sheet = "beeID2024-25") #read in bee ID sheet from master excel file
write.csv(beeID, "C:/Users/new user/Desktop/NRCS_Fall2025/beeID2024-25.csv", row.names = F) #create a csv file with just the beeID sheet.

beeID <- read.csv("C:/Users/new user/Desktop/NRCS_Fall2025/beeID2024-25.csv") #read just the beeID csv sheet in to R
beeID <- as.data.frame(beeID) #make a data frame

# make visitID column since we will pseudoreplicate point x visit
#beeID$survey <- paste0(beeID$full_point_id, "_v", beeID$visit, "_y", beeID$year)

nrow(beeID) #number of rows in the beeID sheet before subsetting
head(beeID) #preview data frame

unique(beeID$Prelim.ID) #read in unique values of Prelim ID
beeID <- subset(beeID, Prelim.ID == "bee" |  Prelim.ID == "none") #subset to only include rows where prelim ID == "bee" or "none"
beeID <- subset(beeID, trap.n.a == "N") #subset to only include traps that were not messed with (no cows, no rain, etc.)
unique(beeID$survey)
nrow(beeID) #checking our current number of rows


library(dplyr);library(ggplot2)

# make visitID column since we will pseudoreplicate point x visit
#beeID$survey <- paste0(data1$full_point_id, "_v", beeID$visit, "_y", data1$year)

unique(beeID$conservation_practice)
beeID <- subset(beeID, conservation_practice != "not_enrolled_in_NRCS" & conservation_practice != "no_longer_point") #remove not enrolled and no longer point
beeID <- subset(beeID, survey != "mo_priv022_v2_y2025") # Remove MO_022_02 (outlier flowers and bee counts AND not actually managed)
beeID <- subset(beeID, treated == "pre" | treated == "post") #only include points where treated == "pre" or "post" 

nrow(beeID) #checking our updated number of rows after subsetting

unique(beeID$survey)
unique(beeID$short_point_id)
unique(beeID$full_point_id)
unique(beeID$state)
unique(beeID$conservation_practice)
unique(beeID$treated)
unique(beeID$trap.type)
beeID <- subset(beeID, trap.type != "unknown")
unique(beeID$month)
unique(beeID$day)
unique(beeID$year)
unique(beeID$visit)
unique(beeID$trap.n.a)
unique(beeID$Insect.Order)
unique(beeID$Family)
unique(beeID$Genus) ##Still has some question marks
unique(beeID$Subgenera)
unique(beeID$Specimen.ID)
unique(beeID$Sex)
unique(beeID$det)
unique(beeID$Preservation)

####
unique(beeID$Prelim.ID) 
beeID$Prelim.ID[beeID$Prelim.ID == "none"] <- 0 #changes "none" to value of zero in the Prelim.ID column
##could I change "bee" to "1" to make it numeric and easier to count???
beeID$Prelim.ID[beeID$Prelim.ID == "bee"] <- 1 # change "bee" to "1" 
beeID$Prelim.ID <- as.numeric(beeID$Prelim.ID) # make numeric


#########
#Total number of bees identified in current subset
sum(beeID$Prelim.ID) #3,728 bees


#Total number of bees captured per conservation practice and treatment
bee_count <- beeID %>%
  group_by(conservation_practice, treated) %>%
  summarise(
    n_bees = sum(Prelim.ID), #sum of 1's and zeros by conservation practice AND treatment
    .groups = "drop"
  )
bee_count #total bees per practice and treatment

#visualizing the bee_count values
###Creating a barplot of total bee Identifications by practice and treatment
ggplot(bee_count, aes(x = conservation_practice, y = n_bees, fill = treated)) + #fill = Treatment → colors bars for Pre/Post
  geom_col(position = position_dodge(width = 0.8), width = 0.7) + #position_dodge → puts Pre and Post bars side by side for each management style
  labs(
    title = "Total Bee Identifications by Conservation Practice and Treatment",
    x = "Conservation Practice",
    y = "Total Bees"
  ) +
  #theme_minimal()
theme(axis.line = element_line(size = 1)) # Adjust 'size' as desired

## Do I need confidence intervals here?


#######
#Trying to make a for loop that gets gets us the number of bees identified from each sampling/trap occasion

# for() loop that adds up all the bees for each sampling occasion and
# calculates a single "# bees" (bee dens) for each survey

beedens <- data.frame("survey" = 0, "NoBees" = 0)

####new for() loop ####
beedens <- data.frame("survey" = 0, "NoBees" = 0)
# for() loop that adds up all the bees for each survey and
# calculates a single "# bees" (flower dens) for each survey
for(i in 1:length(unique(beeID$survey))){
  survey_i <- unique(beeID$survey)[i]
  survey_i_data <- subset(beeID, survey == survey_i)
  NoBees_i <- sum(survey_i_data$Prelim.ID)
  newrow <- data.frame("survey" = survey_i, "NoBees" = NoBees_i)
  beedens <- rbind(beedens, newrow)
  print(paste0("survey ", i, " (", newrow$survey, ")", " is done! It had ",
               (newrow$NoBees), " bees 🐝", "and ", (length(unique_genera)), " Genera"))  ### Not sure how to make this work in the way I want for Genera
  #Sys.sleep(0.05)
}

beedens <- beedens[2:nrow(beedens),]
rownames(beedens) <- 1:nrow(beedens)

head(beedens)

summary(beedens) ##summary of number of bees per survey 
#mean = 9.2 
#min = 0 
#max = 86

#Make a list of unique Genera for each sampling occasion (surveyID)
unique_genera <- tapply(beeID$Genus, beeID$survey, unique)
head(unique_genera)

#Make a list of unique Genera for each conservation practice
unique_genera_per_pract<- tapply(beeID$Genus, beeID$conservation_practice, unique)
head(unique_genera_per_pract)
unique_genera_per_pract
#Goal = to get number of unique Genera per conservation practice
#note that uncertain Genera (?) are still included and probably need to be edited
###(FOR EXAMPLE:Differentiates "Epimelissodes" and "Epimelissodes?")

genera_count <- beeID %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
genera_count

#make a plot of total/overall Genus richness by conservation practice
ggplot(genera_count, aes(x = conservation_practice, y = n_genera)) +
  geom_col() +
  labs(
    y = "Number of Genera",
    x = "Treatment",
    title = "Unique Genera per Conservation Practice"
  )


#### Get a count of unique genera by conservation practice and treatment (pre/post) for each sampling occasion
###Genera count by Replicate

    
  genera_count_by_rep <- beeID %>%
  group_by(survey, conservation_practice, treated) %>%
  summarise(
    n_genera = n_distinct(Genus),
    .groups = "drop"
  )

genera_count_by_rep # This is giving me average for all sampling periods 
# BUT does not appear to be separated by conservation practice/treatment
mean(genera_count_by_rep$n_genera)


##Getting confidence intervals
genera_summary <- genera_count_by_rep %>%
  group_by(conservation_practice, treated) %>%
  summarise(
    mean_genera = mean(n_genera),
    sd_genera = sd(n_genera),
    n = n(),
    se = sd_genera / sqrt(n),
    ci_lower = mean_genera - 1.96 * se,
    ci_upper = mean_genera + 1.96 * se,  
    .groups = "drop"
  )

#creating a plot with confidence intervals
ggplot(genera_summary, aes(x = conservation_practice, y = mean_genera, fill = treated)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    title = "Mean Unique Genera Per Relicate by Conservation Practice and Treatment (Pre vs Post)",
    x = "Conservation Practice",
    y = "Mean Unique Genera (± 95% CI)"
  ) +
  theme_minimal()


######
postfire <- subset(beeID, treated == "post" & conservation_practice == "prescribed_burning")
postshrub <- subset(beeID, treated == "post" & conservation_practice == "brush_management")
postplanting <- subset(beeID, treated == "post" & conservation_practice == "wildlife_hab_planting")
postgrazing <- subset(beeID, treated == "post" & conservation_practice == "prescribed_grazing")
prefire <- subset(beeID, treated == "pre" & conservation_practice == "prescribed_burning")
preshrub <- subset(beeID, treated == "pre" & conservation_practice == "brush_management")
preplanting <- subset(beeID, treated == "pre" & conservation_practice == "wildlife_hab_planting")
pregrazing <- subset(beeID, treated == "pre" & conservation_practice == "prescribed_grazing")

###############################
###Getting genera count by conservation practice and treatment status
genera_count_treat_by_pract <- beeID %>%
  group_by(conservation_practice, treated) %>%
  summarise(
    n_genera = n_distinct(Genus),
    .groups = "drop"
  )
genera_count_treat_by_pract

###plotting pre vs post for each conservation practice
ggplot(genera_count_treat_by_pract, aes(x = treated, y = n_genera, fill = treated)) +
  geom_col() +
  facet_wrap(~ conservation_practice) +
  labs(
    x = "Period",
    y = "Genera Richness",
    title = "Pre vs Post Treatment Genera Richness"
  )



#######################


#post treatment fire
postfire_genera_count <- postfire %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
postfire_genera_count

##post treatment shrub
postshrub_genera_count <- postshrub %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
postshrub_genera_count

##post treatment planting
postplanting_genera_count <- postplanting %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
postplanting_genera_count

##post treatment grazing
postgrazing_genera_count <- postgrazing %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
postgrazing_genera_count

#pre treatment fire
prefire_genera_count <- prefire %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
prefire_genera_count

#pre treatment shrub
preshrub_genera_count <- preshrub %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
preshrub_genera_count

#pre treatment planting
preplanting_genera_count <- preplanting %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
preplanting_genera_count

#pre treatment grazing
pregrazing_genera_count <- pregrazing %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
pregrazing_genera_count

