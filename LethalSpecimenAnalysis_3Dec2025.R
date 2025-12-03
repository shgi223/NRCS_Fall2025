library(readxl)
data1 <- readxl::read_xlsx("./2024-25_CEAP_MASTER_12.3.2025.xlsx", sheet = "beeID2024-25")
data1 <- as.data.frame(data1)
data1 <- subset(data1, year == 2024)

# weed out some "garbage" points
data1 <- subset(data1, conservation_practice != "not_enrolled_in_NRCS")
data1 <- subset(data1, conservation_practice != "no_longer_point")
data1 <- subset(data1, treated != "pre/post? Maybe void")
data1 <- subset(data1, treated != "na- hayfield")
data1 <- subset(data1, treated != "na - neither")
data1 <- subset(data1, treated != "not_point_yet")

# eliminate survey days where traps were not deployed
data1 <- subset(data1, Prelim_ID != "no_trap")

# add "trt" column
data1$trt <- paste0(data1$conservation_practice, "_", data1$treated)
unique(data1$trt)

##### 

NumSurveys <- length(unique(data1$survey))

# make blank data.frame to hold results of for() loop
BigDF <- data.frame("occasion" = character(), "trt" = character(), 
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
  BigDF <- rbind(BigDF, NewRow)
  print(paste0("Run number ", i, " of ", NumSurveys, " is complete"))
  Sys.sleep(0.01)
  
}

BiggerDF <- data.frame("trt" = character(), 
                       "meanRichness" = numeric(), "LCIRichness" = numeric(), "UCIRichness" = numeric(),
                       "meanNumbBees" = numeric(), "LCINumbBees" = numeric(), "UCINumbBees" = numeric())

for(a in 1:8){ # 1-8 because there are eight treatments
  
  # a = 1
  trt_a <- unique(BigDF$trt)[a] # occasion
  
  # subset BigDF just to keep trt "a"
  BigDF_a <- subset(BigDF, trt == trt_a)
  
  # calculate mean richness
  mean_rich_a <- mean(BigDF_a$numbGenera)
  
  # calculate richness confidence interval
  CI_rich_a <- 1.96*sd(BigDF_a$numbGenera)/sqrt(nrow(BigDF_a))
  LCI_rich_a <- mean_rich_a - CI_rich_a
  UCI_rich_a <- mean_rich_a + CI_rich_a
  
  # calculate mean number of bees
  mean_numbBees_a <- mean(BigDF_a$numbBees)
  
  # calculate richness confidence interval
  CI_numbBees_a <- 1.96*sd(BigDF_a$numbBees)/sqrt(nrow(BigDF_a))
  LCI_numbBees_a <- mean_numbBees_a - CI_numbBees_a
  UCI_numbBees_a <- mean_numbBees_a + CI_numbBees_a
  
  # assemble parts
  NewRow <- data.frame("trt" = trt_a, 
                       "meanRichness" = mean_rich_a, "LCIRichness" = LCI_rich_a, "UCIRichness" = UCI_rich_a,
                       "meanNumbBees" = mean_numbBees_a, "LCINumbBees" = LCI_numbBees_a, "UCINumbBees" = UCI_numbBees_a)
  BiggerDF <- rbind(BiggerDF, NewRow)
}

BiggerDF <- BiggerDF[order(BiggerDF$trt), ]

# cumulative number of genera per treatment
GenusDF <- data.frame("trt" = character(), "cumRich" = numeric())
for(z in 1:8){
  # z = 1
  data2 <- subset(data1, Genus != "none")
  trt_z <- unique(data2$trt)[z] # treatment
  data2_z <- subset(data2, trt == trt_z)
  NumbGenera_z <- length(unique(data2_z$Genus))
  NewrowGenus <- data.frame("trt" = trt_z, "cumRich" = NumbGenera_z)
  GenusDF <- rbind(GenusDF, NewrowGenus)
  
}
GenusDF <- GenusDF[order(GenusDF$trt), ]
GenusDF

BiggerDF




