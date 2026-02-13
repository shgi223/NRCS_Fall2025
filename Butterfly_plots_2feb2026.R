

###Zach's help with figure 1 #####

library(readxl)
data1 <- readxl::read_xlsx("./2024-25_CEAP_MASTER_1.20.2026.xlsx", sheet = "Bee_ButterflyID")
data1 <- as.data.frame(data1)
# data1 <- subset(data1, year != 2026)

data1$Count <- 1

#aggregating the important stuff
ag1 <- aggregate(Count ~ Prelim_ID + conservation_practice + treatment_status + survey, data = data1, sum)
ag1$Count[ag1$Prelim_ID == "no_butterflies"] <- 0 #changing count of no_butterflies to 0
ag1$Prelim_ID[ag1$Prelim_ID == "no_butterflies"] <- "butterfly" 
ag1 <- ag1[ag1$Prelim_ID == "butterfly",]

#assembling data for first plot from aggregate (species count)
dataforbox1 <- ag1[ag1$conservation_practice %in% c("wildlife_hab_planting"),]
dataforbox1 <- dataforbox1[dataforbox1$treatment_status != "pre/post? Maybe void",]

dataforbox1$treatment_status <- factor(dataforbox1$treatment_status, 
                                       levels = c("pre", "post"), 
                                       labels = c("Pre-Treatment", "Post-Treatment"))

##create boxplot1 (#WHP butterflies caught per visit occasion separated by pre and post treatment)
box1 <- boxplot(Count ~ treatment_status, data = dataforbox1,
        xlab = "WHP Implementation Status", main = "WHP Butterflies", ylab = "# Butterflies caught (per Visit)",
        col= c("red","blue"))

ag2 <- aggregate(Count ~ conservation_practice + treatment_status, data = ag1, mean)
ag2

###
##skylar's turn to try whomp whomp
data1$Count <- 1

#aggregating the important stuff
ag1 <- aggregate(Count ~ Prelim_ID + conservation_practice + treatment_status + survey, data = data1, sum)
ag1$Count[ag1$Prelim_ID == "no_butterflies"] <- 0 #changing count of no_butterflies to 0
ag1$Prelim_ID[ag1$Prelim_ID == "no_butterflies"] <- "butterfly" 
ag1 <- ag1[ag1$Prelim_ID == "butterfly",]

#assembling data for first plot from aggregate (species count)
dataforbox1 <- ag1[ag1$conservation_practice %in% c("wildlife_hab_planting"),]
dataforbox1 <- dataforbox1[dataforbox1$treatment_status != "pre/post? Maybe void",]

dataforbox1$treatment_status <- factor(dataforbox1$treatment_status, 
                                       levels = c("pre", "post"), 
                                       labels = c("Pre-Treatment", "Post-Treatment"))

##create boxplot1 (#WHP butterflies caught per visit occasion separated by pre and post treatment)
boxplot(Count ~ treatment_status, data = dataforbox1,
        xlab = "WHP Implementation", ylab = "# Butterflies caught per Visit")

ag2 <- aggregate(Count ~ conservation_practice + treatment_status, data = ag1, mean)
ag2
#### Figure 2 ####
data1$Count <- 1

#aggregating the important stuff
ag1 <- aggregate(Count ~ Prelim_ID + conservation_practice + treatment_status + survey + Genus, data = data1, sum)
ag1$Count[ag1$Prelim_ID == "no_butterflies"] <- 0 #changing count of no_butterflies to 0
ag1$Prelim_ID[ag1$Prelim_ID == "no_butterflies"] <- "butterfly" 
ag1 <- ag1[ag1$Prelim_ID == "butterfly",]

#assembling data for first plot from aggregate (species count)
dataforbox2 <- ag1[ag1$conservation_practice %in% c("wildlife_hab_planting"),]
dataforbox2 <- dataforbox2[dataforbox1$treatment_status != "pre/post? Maybe void",]

dataforbox2$treatment_status <- factor(dataforbox2$treatment_status, 
                                       levels = c("pre", "post"), 
                                       labels = c("Pre-Treatment", "Post-Treatment"))

##create boxplot1 (#WHP butterflies caught per visit occasion separated by pre and post treatment)
boxplot(Count ~ treatment_status, data = dataforbox2,
        xlab = "Time", ylab = "Genus Richness per Visit")

ag2 <- aggregate(Count ~ conservation_practice + treatment_status, data = ag1, mean)
ag2

unique(dataforbox2$Genus)


##### Figure 3 ####

#% of total samples by top 10 butterfly species collected in WHP post
library(readxl)
dfFig3 <- readxl::read_xlsx("./2024-25_CEAP_MASTER_1.20.2026.xlsx", sheet = "Fig3")
dfFig3 <- as.data.frame(dfFig3)

dfFig3 <- subset(dfFig3, Rank != "11")
dfFig3 <- subset(dfFig3, Rank != "12")
dfFig3 <- subset(dfFig3, Rank != "13")

#View(dfFig3)

####
Fig3 <- ggplot(data = dfFig3, aes(x = Species, y = `% of total sample`)) +
  geom_col(fill = "blue") + # Use geom_col and set a color for the bars
  labs(title = "% of collected butterflies per Species", # Add descriptive labels and titles
       x = "Species",
       y = "% of butterfly sample") +
  theme_minimal() # Apply a clean theme
Fig3

#### Figure 4 ####
#% of total samples by top 10 butterfly species collected in WHP pre
#library(readxl)
dfFig4 <- readxl::read_xlsx("./2024-25_CEAP_MASTER_1.20.2026.xlsx", sheet = "Fig4")
dfFig4 <- as.data.frame(dfFig4)

dfFig4 <- subset(dfFig4, Rank != "11")
dfFig4 <- subset(dfFig4, Rank != "12")
dfFig4 <- subset(dfFig4, Rank != "13")

#View(dfFig4)

####
Fig4 <- ggplot(data = dfFig4, aes(x = Species, y = `% of total sample`)) +
  geom_col(fill = "blue") + # Use geom_col and set a color for the bars
  labs(title = "% of WHP Butterfly Sample by Species", # Add descriptive labels and titles
       x = "Species",
       y = "% of butterfly sample") +
  theme_minimal() # Apply a clean theme
Fig4

###

