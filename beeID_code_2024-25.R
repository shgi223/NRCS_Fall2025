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



##Cleaning Code ###
#beeID <- read.csv("C:/Users/new user/Desktop/NRCS_Fall2025/beeID_2024-25.csv")
beeID <- readxl::read_xlsx("./2024-25_CEAP_MASTER_9.12.2025.xlsx", sheet = "beeID2024-25")
write.csv(beeID, "C:/Users/new user/Desktop/NRCS_Fall2025/beeID2024-25.csv", row.names = F)

beeID <- read.csv("C:/Users/new user/Desktop/NRCS_Fall2025/beeID2024-25.csv")
beeID <- as.data.frame(beeID)

# make visitID column since we will pseudoreplicate point x visit
#beeID$survey <- paste0(beeID$full_point_id, "_v", beeID$visit, "_y", beeID$year)

nrow(beeID)
head(beeID)

unique(beeID$Prelim.ID)
beeID <- subset(beeID, Prelim.ID == "bee")
beeID <- subset(beeID, trap.n.a == "N")
unique(beeID$survey)
nrow(beeID)

# make visitID column since we will pseudoreplicate point x visit
#beeID$survey <- paste0(data1$full_point_id, "_v", beeID$visit, "_y", data1$year)

unique(beeID$conservation_practice)
beeID <- subset(beeID, conservation_practice != "not_enrolled_in_NRCS")
beeID <- subset(beeID, conservation_practice != "no_longer_point")
beeID <- subset(beeID, survey != "mo_priv022_v2_y2025") # Remove MO_022_02
beeID <- subset(beeID, treated == "pre" | treated == "post")

nrow(beeID)


unique(beeID$survey)

unique(beeID$short_point_id)
unique(beeID$full_point_id)
unique(beeID$state)
unique(beeID$conservation_practice)
unique(beeID$treated)
unique(beeID$trap.type)
beeID <- subset(beeID, trap.type != "unknown")
unique(beeID$month) #WHY ARE THERE NAS in the month column?
#beeID <- subset(beeID, month != "NA")
unique(beeID$day)
unique(beeID$year)
unique(beeID$visit)

unique(beeID$trap.n.a)
unique(beeID$Prelim.ID)
unique(beeID$Insect.Order)
unique(beeID$Family)
unique(beeID$Genus)
unique(beeID$Specimen.ID)
unique(beeID$Sex)
unique(beeID$det)
unique(beeID$Preservation)

#########
#Trying to make a for loop that gets gets us the number of bees identified from each sampling/trap occasion

# for() loop that adds up all the bees for each sampling occasion and
# calculates a single "# bees" (bee dens) for each survey

beedens <- data.frame("survey" = 0, "NoBees" = 0)


for(i in 1:length(unique(beeID$survey))){
  survey_i <-unique(beeID$survey)[i]
  survey_i_data <- subset(beeID, survey == survey_i)
  NoBees_i <-(nrow(survey_i_data))
  newrow <- data.frame("survey" = survey_i, "NoBees" = NoBees_i)
  beedens <- rbind (beedens, newrow)
  unique_genera <- tapply(beeID$Genus, beeID$survey, unique)[i] ### Not sure how to make this work in the way I want
   print(paste0("survey ", i, " (", newrow$survey, ")", " is done! It had ",
               (newrow$NoBees), " bees 🐝", "and ", (length(unique_genera)), " Genera"))
  #Sys.sleep(0.05)
}

beedens <- beedens[2:nrow(beedens),]
rownames(beedens) <- 1:nrow(beedens)

head(beedens)

summary(beedens)

#Make a list of unique Genera for each sampling occasion (surveyID)
unique_genera <- tapply(beeID$Genus, beeID$survey, unique)
head(unique_genera)

#Make a list of unique Genera for each conservation practice
unique_genera_per_pract<- tapply(beeID$Genus, beeID$conservation_practice, unique)
head(unique_genera_per_pract)

#Get number of unique Genera/Subgenera (does not differentiate Lasioglossum subgenera here)
library(dplyr);library(ggplot2)

genera_count <- beeID %>%
  group_by(conservation_practice) %>%
  summarise(
    n_genera = n_distinct(Genus)
  )
genera_count


#make a plot of Genus richness by conservation practice
ggplot(genera_count, aes(x = conservation_practice, y = n_genera)) +
  geom_col() +
  labs(
    y = "Number of Genera",
    x = "Treatment",
    title = "Unique Genera per Treatment"
  )

###Getting genera count by conservation practice and treatment status
genera_count_treat_by_pract <- beeID %>%
  group_by(conservation_practice, treated) %>%
  summarise(
    n_genera = n_distinct(Genus),
    .groups = "drop"
  )
genera_count_treat_by_pract

###plotting pre vs post for each treatment
ggplot(genera_count_treat_by_pract, aes(x = treated, y = n_genera, fill = treated)) +
  geom_col() +
  facet_wrap(~ conservation_practice) +
  labs(
    x = "Period",
    y = "Genera Richness",
    title = "Pre vs Post Treatment Genera Richness"
  )



#### Get a count of unique genera by conservation practice and treatment (pre/post) for each sampling occasion
genera_count_treat_by_pract_survey <- beeID %>%
  group_by(conservation_practice, treated, survey) %>%
  summarise(
    n_genera = n_distinct(Genus),
    .groups = "drop"
  )
genera_count_treat_by_pract_survey



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

