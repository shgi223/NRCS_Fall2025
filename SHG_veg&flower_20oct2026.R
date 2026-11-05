library(readxl)

################################# part 1 - floral data
flowers1

postfire <- subset(flowers1, treated == "post" & practice == "prescribed_burning")
postshrub <- subset(flowers1, treated == "post" & practice == "brush_management")
postplanting <- subset(flowers1, treated == "post" & practice == "wildlife_hab_planting")
postgrazing <- subset(flowers1, treated == "post" & practice == "prescribed_grazing")
prefire <- subset(flowers1, treated == "pre" & practice == "prescribed_burning")
preshrub <- subset(flowers1, treated == "pre" & practice == "brush_management")
preplanting <- subset(flowers1, treated == "pre" & practice == "wildlife_hab_planting")
pregrazing <- subset(flowers1, treated == "pre" & practice == "prescribed_grazing")


mean(preplanting$flowers)
mean(postplanting$flowers)

#non-parametric anova on LOG flower density
flowers1$treatment <- paste0(flowers1$practice, "_", flowers1$treated) # make practice_treatment column 
flowers.KW <- subset(flowers1, treatment == "prescribed_burning_pre" | treatment == "prescribed_burning_post" |treatment == "brush_management_pre"|
                       treatment == "brush_management_post"|treatment =="wildlife_hab_planting_pre"| treatment =="wildlife_hab_planting_post"|
                       treatment == "prescribed_grazing_pre"| treatment == "prescribed_grazing_post") #only using cleaned treatment + practice data

unique(flowers.KW$treatment)#double checking treatment
log_my_data <- as.data.frame(flowers.KW)#creating data frame for Kruskal-Wallis test

kruskal.test(log(flowers.KW$flowers+1) ~ flowers.KW$treatment, data = log_my_data )

#install.packages("rstatix")
library(rstatix)

log_my_data %>% dunn_test(flowers ~ treatment)
library(FSA)
library(rcompanion)

# Perform the Dunn Test with NO p-value adjustment ("none")
dunn_results <- dunnTest(flowers ~ treatment, 
                         data = log_my_data, 
                         method = "none") # "none" specifies the unadjusted method

#Extract the comparison dataframe
logcomparison_df <- dunn_results$res

#Generate the compact letter display (CLD)
logletter_groupings <- cldList(P.unadj ~ Comparison, 
                               data = logcomparison_df, 
                               threshold = 0.05) # Uses your preferred alpha level

#View the final letter groupings for log(flowers)
print(logletter_groupings)

#create boxplot for log flower density with letter groupings
logbp <-boxplot(log(prefire$flowers+1), log(postfire$flowers+1), 
        log(preshrub$flowers+1), log(postshrub$flowers+1),
        log(preplanting$flowers+1), log(postplanting$flowers+1),
        log(pregrazing$flowers+1), log(postgrazing$flowers+1),
        at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7),
        names = c("pre", "post", "pre", "post", 
                  "pre", "post", "pre", "post"),
        las = 2,
        col = c("azure3","azure3", 
                "darkolivegreen1", "darkolivegreen1",
                "plum1", "plum1",
                "cornsilk2","cornsilk2"),
        boxwex = 0.5,
        outline=FALSE, ylab = "Log Floral Density", ylim = c(0,15))

# Stripchart - add jittered points
stripchart(list(log(prefire$flowers+1), log(postfire$flowers+1), 
                log(preshrub$flowers+1), log(postshrub$flowers+1),
                log(preplanting$flowers+1), log(postplanting$flowers+1),
                log(pregrazing$flowers+1), log(postgrazing$flowers+1)),
           method = "jitter",      # Add jitter to spread points horizontally
           pch = 16,               # Solid circle points
           
           col = c(rgb(0, 0, 0, 0.25),  # Transparent black for nest
                   rgb(1, 0, 0, 0.25),  # Transparent red for random
                   rgb(0, 0, 0, 0.25),  # Transparent black for nest
                   rgb(1, 0, 0, 0.25),  # Transparent red for random
                   rgb(0, 0, 0, 0.25),  # Transparent black for nest
                   rgb(1, 0, 0, 0.25),  # Transparent red for random
                   rgb(0, 0, 0, 0.25),  # Transparent black for nest
                   rgb(1, 0, 0, 0.25)),  # Transparent red for random
           
           add = TRUE, vertical = TRUE, # Add to existing boxplot
           at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7), # Same positions as boxplots
           cex = 0.5)

text(x = 1.375, y = 13.5, labels = "Fire")
text(x = 3.125, y = 13.5, labels = "Brush Mgmt")
text(x = 4.875, y = 13.5, labels = "Planting")
text(x = 6.625, y = 13.5, labels = "Grazing")

##Extract the top y-coordinate for each box/whisker 
## bp$stats[5, ] gives the top whisker value for each group
top_y <- logbp$stats[5, ]+1 #

# Add letter grouping labels
text(x = 1:length(logbp$names), 
     y = top_y, 
     labels = logletter_groupings$MonoLetter, 
     xpd = TRUE) # xpd = TRUE allows plotting outside the default box clipping region


############
##Now, working with floral density (WITHOUT log)
#non-parametric anova on RAW flower density
flowers1$treatment <- paste0(flowers1$practice, "_", flowers1$treated) # make practice_treatment column 
flowers.KW <- subset(flowers1, treatment == "prescribed_burning_pre" | treatment == "prescribed_burning_post" |treatment == "brush_management_pre"|
                       treatment == "brush_management_post"|treatment =="wildlife_hab_planting_pre"| treatment =="wildlife_hab_planting_post"|
                       treatment == "prescribed_grazing_pre"| treatment == "prescribed_grazing_post") #only using cleaned treatment + practice data

unique(flowers.KW$treatment)#double checking treatment
my_data <- as.data.frame(flowers.KW)#creating data frame for Kruskal-Wallis test

kruskal.test(flowers.KW$flowers ~ flowers.KW$treatment, data = my_data )

#install.packages("rstatix")
library(rstatix)

my_data %>% dunn_test(flowers ~ treatment)
library(FSA)
library(rcompanion)

# 2. Perform the Dunn Test with NO p-value adjustment ("none")
dunn_results <- dunnTest(flowers ~ treatment, 
                         data = my_data, 
                         method = "none") # "none" specifies the unadjusted method

# 3. Extract the comparison dataframe
comparison_df <- dunn_results$res

# 4. Generate the compact letter display (CLD)
letter_groupings <- cldList(P.unadj ~ Comparison, 
                            data = comparison_df, 
                            threshold = 0.05) # Uses your preferred alpha level

# 5. View the final letter groupings
print(letter_groupings)

##########
#boxplot figure without log flowers
bp <-boxplot(prefire$flowers, postfire$flowers, 
       preshrub$flowers, postshrub$flowers,
        preplanting$flowers, postplanting$flowers,
        pregrazing$flowers, postgrazing$flowers,
        at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7),
        names = c("pre", "post", "pre", "post", 
                  "pre", "post", "pre", "post"),
        las = 2,
        col = c("azure3","azure3", 
                "darkolivegreen1", "darkolivegreen1",
                "plum1", "plum1",
                "cornsilk2","cornsilk2"),
        boxwex = 0.5,
        outline=FALSE, ylab = "Floral Density")

# Stripchart - add jittered points (No log, just raw flower data)
stripchart(list(prefire$flowers, postfire$flowers, 
                preshrub$flowers, postshrub$flowers,
                preplanting$flowers, postplanting$flowers,
                pregrazing$flowers, postgrazing$flowers),
           method = "jitter",      # Add jitter to spread points horizontally
           pch = 16,               # Solid circle points
           
           col = c(rgb(0, 0, 0, 0.25),  # Transparent black for nest
                   rgb(1, 0, 0, 0.25),  # Transparent red for random
                   rgb(0, 0, 0, 0.25),  # Transparent black for nest
                   rgb(1, 0, 0, 0.25),  # Transparent red for random
                   rgb(0, 0, 0, 0.25),  # Transparent black for nest
                   rgb(1, 0, 0, 0.25),  # Transparent red for random
                   rgb(0, 0, 0, 0.25),  # Transparent black for nest
                   rgb(1, 0, 0, 0.25)),  # Transparent red for random
           
           add = TRUE, vertical = TRUE, # Add to existing boxplot
           at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7), # Same positions as boxplots
           cex = 0.5)

text(x = 1.375, y = 450, labels = "Fire")
text(x = 3.125, y = 450, labels = "Brush Mgmt")
text(x = 4.875, y = 450, labels = "Planting")
text(x = 6.625, y = 450, labels = "Grazing")

##Extract the top y-coordinate for each box/whisker 
top_y <- bp$stats[5, ]### bp$stats[5, ] gives the top whisker value for each group

# Add letter grouping labels
text(x = 1:length(bp$names), 
     y = top_y, 
     labels = letter_groupings$MonoLetter, 
     xpd = TRUE) # xpd = TRUE allows plotting outside the default box clipping region




###########################################

##### Floral Species Richness
##### 

BigDF <- data.frame("SurveyID"=1, "treatment"=1,"richness"=1) # make blank data.frame to hold results of for() loop

for(i in 1: length(unique(my_data$survey))){
  survey_i_data<-subset(my_data, survey == unique(my_data$survey)[i])
  richness_i <- length(unique(survey_i_data$species))
  newrow_i<-c(survey_i_data$survey[1], survey_i_data$treatment[1],richness_i)
  BigDF <- rbind(BigDF, newrow_i)
}
BigDF <- BigDF[2:nrow(BigDF),]
BigDF$richness <- as.numeric(BigDF$richness)

kruskal.test(BigDF$richness ~ BigDF$treatment, data = my_data )
mean(BigDF$richness)
#install.packages("rstatix")
library(rstatix)

#BigDF %>% dunn_test(richness ~ treatment)
library(FSA)
library(rcompanion)

# Perform the Dunn Test with NO p-value adjustment ("none")
dunn_results <- dunnTest(richness ~ treatment, 
                         data = BigDF, 
                         method = "none") # "none" specifies the unadjusted method

#Extract the comparison dataframe
richcomparison_df <- dunn_results$res

#Generate the compact letter display (CLD)
richletter_groupings <- cldList(P.unadj ~ Comparison, 
                            data = richcomparison_df, 
                            threshold = 0.05) # Uses preferred alpha level
print(richletter_groupings)


#Floral species Richness figure
postfire <- subset(BigDF, treatment == "prescribed_burning_post")
postshrub <- subset(BigDF, treatment == "brush_management_post")
postplanting <- subset(BigDF, treatment == "wildlife_hab_planting_post")
postgrazing <- subset(BigDF, treatment == "prescribed_grazing_post")
prefire <- subset(BigDF, treatment == "prescribed_burning_pre")
preshrub <- subset(BigDF, treatment ==  "brush_management_pre")
preplanting <- subset(BigDF, treatment == "wildlife_hab_planting_pre")
pregrazing <- subset(BigDF, treatment == "prescribed_grazing_pre")
 #mean(preplanting$richness)
 #mean(postplanting$richness)

richbpreplantingrichbp <-boxplot(prefire$richness, postfire$richness, 
        preshrub$richness, postshrub$richness,
        preplanting$richness, postplanting$richness,
        pregrazing$richness, postgrazing$richness,
        at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7),
        names = c("pre", "post", "pre", "post", 
                  "pre", "post", "pre", "post"),
        las = 2,
        col = c("azure3","azure3", 
                "darkolivegreen1", "darkolivegreen1",
                "plum1", "plum1",
                "cornsilk2","cornsilk2"),
        boxwex = 0.5,
        outline=FALSE, ylab = "Floral Species Richness")

text(x = 1.375, y = 18, labels = "Fire")
text(x = 3.125, y = 18, labels = "Brush Mgmt")
text(x = 4.875, y = 18, labels = "Planting")
text(x = 6.625, y = 18, labels = "Grazing")

#richbp$stats
##Extract the top y-coordinate for each box/whisker 
# bp$stats[5, ] gives the top whisker value for each group
top_y <- richbp$stats[5, ]##+1 adds 1 space for text on plot

# Add letter grouping labels
text(x = 1:length(richbp$names), 
     y = top_y, 
     labels = richletter_groupings$MonoLetter, 
     xpd = TRUE) # xpd = TRUE allows plotting outside the default box clipping region



################################# part 2 - veg data

veg1 <- readxl::read_xlsx("./2024-26_CEAP_MASTER_10.14.2026.xlsx",
                          sheet = "veg")
veg1 <- as.data.frame(veg1)
veg1 <- veg1[,c(4,5,6,7,31:45)]
head(veg1)

postfire_veg <- subset(veg1, treatment_status == "post" & conservation_practice == "prescribed_burning")
postshrub_veg <- subset(veg1, treatment_status == "post" & conservation_practice == "brush_management")
postplanting_veg <- subset(veg1, treatment_status == "post" & conservation_practice == "wildlife_hab_planting")
postgrazing_veg <- subset(veg1, treatment_status == "post" & conservation_practice == "prescribed_grazing")
prefire_veg <- subset(veg1, treatment_status == "pre" & conservation_practice == "prescribed_burning")
preshrub_veg <- subset(veg1, treatment_status == "pre" & conservation_practice == "brush_management")
preplanting_veg <- subset(veg1, treatment_status == "pre" & conservation_practice == "wildlife_hab_planting")
pregrazing_veg <- subset(veg1, treatment_status == "pre" & conservation_practice == "prescribed_grazing")

# prescribed fire
veg_fire <- boxplot(prefire_veg$perc_bare, postfire_veg$perc_bare, # bare ground
        prefire_veg$perc_anygrass, postfire_veg$perc_anygrass, # grass
        prefire_veg$perc_forb, postfire_veg$perc_forb, # forbs
        prefire_veg$perc_anywoody, postfire_veg$perc_anywoody, #woody
        at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7),
        names = c("pre", "post", "pre", "post", 
                  "pre", "post", "pre", "post"),
        las = 2,
        col = c("burlywood4","burlywood4", 
                "chartreuse3", "chartreuse3",
                "slateblue3", "slateblue3",
                "red2","red2"),
        boxwex = 0.5,
        outline=FALSE, ylab = "Percent Cover", 
        ylim = c(0,110), main = "Prescribed Fire")

text(x = 1.375, y = 100, labels = "Bare Ground")
text(x = 3.125, y = 100, labels = "Grass")
text(x = 4.875, y = 100, labels = "Forbs")
text(x = 6.625, y = 100, labels = "Woody Stems")

# brush management
veg_brush <-boxplot(preshrub_veg$perc_bare, postshrub_veg$perc_bare, # bare ground
        preshrub_veg$perc_anygrass, postshrub_veg$perc_anygrass, # grass
        preshrub_veg$perc_forb, postshrub_veg$perc_forb, # forbs
        preshrub_veg$perc_anywoody, postshrub_veg$perc_anywoody, #woody
        at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7),
        names = c("pre", "post", "pre", "post", 
                  "pre", "post", "pre", "post"),
        las = 2,
        col = c("burlywood4","burlywood4", 
                "chartreuse3", "chartreuse3",
                "slateblue3", "slateblue3",
                "red2","red2"),
        boxwex = 0.5,
        outline=FALSE, ylab = "Percent Cover", 
        ylim = c(0,110), main = "Shrub Management")

text(x = 1.375, y = 100, labels = "Bare Ground")
text(x = 3.125, y = 100, labels = "Grass")
text(x = 4.875, y = 100, labels = "Forbs")
text(x = 6.625, y = 100, labels = "Woody Stems")

# prescribed grazing
veg_grazing <-boxplot(pregrazing_veg$perc_bare, postgrazing_veg$perc_bare, # bare ground
        pregrazing_veg$perc_anygrass, postgrazing_veg$perc_anygrass, # grass
        pregrazing_veg$perc_forb, postgrazing_veg$perc_forb, # forbs
        pregrazing_veg$perc_anywoody, postgrazing_veg$perc_anywoody, #woody
        at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7),
        names = c("pre", "post", "pre", "post", 
                  "pre", "post", "pre", "post"),
        las = 2,
        col = c("burlywood4","burlywood4", 
                "chartreuse3", "chartreuse3",
                "slateblue3", "slateblue3",
                "red2","red2"),
        boxwex = 0.5,
        outline=FALSE, ylab = "Percent Cover", 
        ylim = c(0,110), main = "Prescribed Grazing")

text(x = 1.375, y = 100, labels = "Bare Ground")
text(x = 3.125, y = 100, labels = "Grass")
text(x = 4.875, y = 100, labels = "Forbs")
text(x = 6.625, y = 100, labels = "Woody Stems")

# wildlife habitat planting
veg_WHP <-boxplot(preplanting_veg$perc_bare, postplanting_veg$perc_bare, # bare ground
        preplanting_veg$perc_anygrass, postplanting_veg$perc_anygrass, # grass
        preplanting_veg$perc_forb, postplanting_veg$perc_forb, # forbs
        preplanting_veg$perc_anywoody, postplanting_veg$perc_anywoody, #woody
        at = c(1, 1.75, 2.75, 3.5, 4.5, 5.25, 6.25, 7),
        names = c("pre", "post", "pre", "post", 
                  "pre", "post", "pre", "post"),
        las = 2,
        col = c("burlywood4","burlywood4", 
                "chartreuse3", "chartreuse3",
                "slateblue3", "slateblue3",
                "red2","red2"),
        boxwex = 0.5,
        outline=FALSE, ylab = "Percent Cover", 
        ylim = c(0,110), main = "Wildlife Habitat Planting")

text(x = 1.375, y = 100, labels = "Bare Ground")
text(x = 3.125, y = 100, labels = "Grass")
text(x = 4.875, y = 100, labels = "Forbs")
text(x = 6.625, y = 100, labels = "Woody Stems")






####
subset(flowers1$practice, )




######
#May be unnecessary

library(dplyr)
# 1. Calculate descriptive statistics
summary_table <- my_data %>%
  group_by(treatment) %>%
  summarise(
    Mean = mean(flowers, na.rm = TRUE),
    SD = sd(flowers, na.rm = TRUE),
    N = n()
  )

# Merge the letter groupings
# cldList outputs a column named 'Group'. We rename it to match 'treatment'.
letters_clean <- letter_groupings %>% 
  rename(treatment = Group)

final_table <- summary_table %>%
  left_join(letters_clean, by = "treatment")

# View final table
print(final_table)

# Perform the Dunn Test with NO p-value adjustment ("none")
dunn_results <- dunnTest(flowers ~ treatment, 
                         data = my_data, 
                         method = "none") # "none" specifies the unadjusted method

# Extract the comparison dataframe
comparison_df <- dunn_results$res

# Generate the compact letter display (CLD)
letter_groupings <- cldList(P.unadj ~ Comparison, 
                            data = comparison_df, 
                            threshold = 0.05) # Uses your preferred alpha level

#View the final letter groupings
print(letter_groupings)

# Calculate group means and merge with letters
group_means <- tapply(my_data$flowers, my_data$treatment, mean)
# Ensure letter order matches the barplot categories
cld_res <- cld_res[match(names(group_means), cld_res$Group), ]


#######Create barplot with letter groupings
bp <- barplot(group_means, 
              ylim = c(0, 2000), 
              xlab = "Treatment", 
              ylab = "Flowers", 
              col = "blue", 
              main = "Dunn Post-Hoc Letter Groupings")

# Add letters above the bars
text(x = bp, 
     y = group_means + max(group_means) * 0.05, 
     labels = letter_groupings, 
     font = 2)




############
#non-parametric anova on log flower density


