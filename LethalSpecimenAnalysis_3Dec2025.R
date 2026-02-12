library(readxl)
data1 <- readxl::read_xlsx("./2024-25_CEAP_MASTER_1.20.2026.xlsx", sheet = "Bee_ButterflyID")
data1 <- as.data.frame(data1)
data1 <- subset(data1, year != 2026)

unique(data1$treatment_status)
unique(data1$conservation_practice)
# weed out some "garbage" points
data1 <- subset(data1, conservation_practice != "not_enrolled_in_NRCS")
data1 <- subset(data1, conservation_practice != "no_longer_point")
data1 <- subset(data1, treatment_status != "pre/post? Maybe void")
data1 <- subset(data1, treatment_status != "na- hayfield")
data1 <- subset(data1, treatment_status != "na - neither")
data1 <- subset(data1, treatment_status != "not_point_yet")
data1 <- subset(data1, treatment_status != "no_longer_point")

# eliminate survey days where traps were not deployed
data1 <- subset(data1, Prelim_ID != "no_trap")

# add "trt" column
data1$trt <- paste0(data1$conservation_practice, "_", data1$treatment_status)
unique(data1$trt)

##### 

NumSurveys <- length(unique(data1$survey))

# make blank data.frame to hold results of for() loop
BigBeeDF <- data.frame("occasion" = character(), "trt" = character(), 
                    "numbBees" = numeric(), "numbGenera" = numeric())

for(i in 1:NumSurveys){
  
  # i = 50
  occasion_i <- unique(data1$survey)[i] # occasion
  
  # subset the data
  data_i <- subset(data1, survey == occasion_i)
  
  # obtain treatment
  treatment_i <- data_i$trt[1]
  
  # count the number of bees
  numb_bees_i <- nrow(subset(data_i, Prelim_ID == "bee")) # number of bees
  
  # count the number of bee genera in the sample
  data_i_bees <- subset(data_i, Prelim_ID == "bee")
  numb_genera_i <- length(unique(data_i_bees$Genus)) # number of unique genera
  
  NewRow <- data.frame("occasion" = occasion_i, "trt" = treatment_i, 
                       "numbBees" = numb_bees_i, "numbGenera" = numb_genera_i)
  BigBeeDF <- rbind(BigDF, NewRow)
  print(paste0("Run number ", i, " of ", NumSurveys, " is complete"))
  Sys.sleep(0.01)
  
}

# Toy boxplot example
df <- data.frame(Trt = sample(c("Pre", "Post"), 20, replace = T),
                 Response = rnorm(20))
df$Trt <- factor(df$Trt, levels = c("Pre", "Post"))


boxplot(Response ~ Trt, data = df)



BiggerBeeDF <- data.frame("trt" = character(), 
                       "meanRichness" = numeric(), "LCIRichness" = numeric(), "UCIRichness" = numeric(),
                       "meanNumbBees" = numeric(), "LCINumbBees" = numeric(), "UCINumbBees" = numeric())

for(a in 1:8){ # 1-8 because there are eight treatments
  
  # a = 1
  trt_a <- unique(BigBeeDF$trt)[a] # occasion
  
  # subset BigDF just to keep trt "a"
  BigBeeDF_a <- subset(BigBeeDF, trt == trt_a)
  
  # calculate mean richness
  mean_rich_a <- mean(BigBeeDF_a$numbGenera)
  
  # calculate richness confidence interval
  CI_rich_a <- 1.96*sd(BigBeeDF_a$numbGenera)/sqrt(nrow(BigBeeDF_a))
  LCI_rich_a <- mean_rich_a - CI_rich_a
  UCI_rich_a <- mean_rich_a + CI_rich_a
  
  # calculate mean number of bees
  mean_numbBees_a <- mean(BigBeeDF_a$numbBees)
  
  # calculate richness confidence interval
  CI_numbBees_a <- 1.96*sd(BigDF_a$numbBees)/sqrt(nrow(BigBeeDF_a))
  LCI_numbBees_a <- mean_numbBees_a - CI_numbBees_a
  UCI_numbBees_a <- mean_numbBees_a + CI_numbBees_a
  
  # assemble parts
  NewRow <- data.frame("trt" = trt_a, 
                       "meanRichness" = mean_rich_a, "LCIRichness" = LCI_rich_a, "UCIRichness" = UCI_rich_a,
                       "meanNumbBees" = mean_numbBees_a, "LCINumbBees" = LCI_numbBees_a, "UCINumbBees" = UCI_numbBees_a)
  BiggerBeeDF <- rbind(BiggerBeeDF, NewRow)
}

BiggerBeeDF <- BiggerBeeDF[order(BiggerBeeDF$trt), ]
###
# cumulative number of genera per treatment
BeeGenusDF <- data.frame("trt" = character(), "cumRich" = numeric())
for(z in 1:8){
  # z = 1
  data3 <- subset(data1, Genus != "none")
  data3 <- subset(data3, Prelim_ID == "bee")
  trt_z <- unique(data3$trt)[z] # treatment
  data3_z <- subset(data3, trt == trt_z)
  NumbBeeGenera_z <- length(unique(data3_z$Genus))
  NewrowBeeGenus <- data.frame("trt" = trt_z, "cumRich" = NumbBeeGenera_z)
  BeeGenusDF <- rbind(BeeGenusDF, NewrowBeeGenus)
  
}
BeeGenusDF <- BeeGenusDF[order(BeeGenusDF$trt), ]
BeeGenusDF
ButtGenusDF

BiggerButtDF
BiggerBeeDF


##################
#Butterflies???
# make blank data.frame to hold results of for() loop
BigButtDF <- data.frame("occasion" = character(), "trt" = character(), 
                        "numbButts" = numeric(), "numbGenera" = numeric())

for(i in 1:NumSurveys){
  
   #i = 50
  occasion_i <- unique(data1$survey)[i] # occasion
  
  # subset the data
  data_i <- subset(data1, survey == occasion_i)
  
  # obtain treatment
  treatment_i <- data_i$trt[1]
  
  # count the number of bees
  numb_butts_i <- nrow(subset(data_i, Prelim_ID == "butterfly")) # number of butterflies
  
  # count the number of bee genera in the sample
  data_i_butts <- subset(data_i, Prelim_ID == "butterfly")
  numb_genera_i <- length(unique(data_i_butts$Genus)) # number of unique genera
  
  NewRow <- data.frame("occasion" = occasion_i, "trt" = treatment_i, 
                       "numbButts" = numb_butts_i, "numbGenera" = numb_genera_i)
  BigButtDF <- rbind(BigButtDF, NewRow)
  print(paste0("Run number ", i, " of ", NumSurveys, " is complete"))
  Sys.sleep(0.01)
  
}
BiggerButtDF <- data.frame("trt" = character(), 
                          "meanRichness" = numeric(), "LCIRichness" = numeric(), "UCIRichness" = numeric(),
                          "meanNumbButts" = numeric(), "LCINumbButts" = numeric(), "UCINumbButts" = numeric())

for(a in 1:8){ # 1-8 because there are eight treatments
  
  # a = 1
  trt_a <- unique(BigButtDF$trt)[a] # occasion
  
  # subset BigDF just to keep trt "a"
  BigButtDF_a <- subset(BigButtDF, trt == trt_a)
  
  # calculate mean richness
  mean_rich_a <- mean(BigButtDF_a$numbGenera)
  
  # calculate richness confidence interval
  CI_rich_a <- 1.96*sd(BigButtDF_a$numbGenera)/sqrt(nrow(BigButtDF_a))
  LCI_rich_a <- mean_rich_a - CI_rich_a
  UCI_rich_a <- mean_rich_a + CI_rich_a
  
  # calculate mean number of bees
  mean_numbButts_a <- mean(BigButtDF_a$numbButts)
  
  # calculate richness confidence interval
  CI_numbButts_a <- 1.96*sd(BigButtDF_a$numbButts)/sqrt(nrow(BigButtDF_a))
  LCI_numbButts_a <- mean_numbButts_a - CI_numbButts_a
  UCI_numbButts_a <- mean_numbButts_a + CI_numbButts_a
  
  # assemble parts
  NewRow <- data.frame("trt" = trt_a, 
                       "meanRichness" = mean_rich_a, "LCIRichness" = LCI_rich_a, "UCIRichness" = UCI_rich_a,
                       "meanNumbButts" = mean_numbButts_a, "LCINumbButts" = LCI_numbButts_a, "UCINumbButts" = UCI_numbButts_a)
  BiggerButtDF <- rbind(BiggerButtDF, NewRow)
}

BiggerButtDF <- BiggerButtDF[order(BiggerButtDF$trt), ]

# cumulative number of genera per treatment
ButtGenusDF <- data.frame("trt" = character(), "cumRich" = numeric())
for(z in 1:8){
  # z = 1
  data2 <- subset(data1, Genus != "none")
  data2 <- subset(data1, Prelim_ID == "butterfly")
  trt_z <- unique(data2$trt)[z] # treatment
  data2_z <- subset(data2, trt == trt_z)
  NumbButtGenera_z <- length(unique(data2_z$Genus))
  NewrowButtGenus <- data.frame("trt" = trt_z, "cumRich" = NumbButtGenera_z)
  ButtGenusDF <- rbind(ButtGenusDF, NewrowButtGenus)
  
}
ButtGenusDF <- ButtGenusDF[order(ButtGenusDF$trt), ]
ButtGenusDF
###
##Resulting data frames
BiggerButtDF # has confidence intervals and mean for richness and number of butterflies
ButtGenusDF # just genus richness (butterflies)


BiggerBeeDF # has confidence intervals and mean for richness and number of bees
BeeGenusDF # just genus richness (bees)
############



