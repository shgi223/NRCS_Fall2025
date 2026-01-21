library(readxl)

################################# part 1 - floral data
flowers1

postfire <- subset(flowers1, treated == "post" & practice == "prescribed_burning")
postshrub <- subset(df2, treated == "post" & practice == "brush_management")
postplanting <- subset(df2, treated == "post" & practice == "wildlife_hab_planting")
postgrazing <- subset(df2, treated == "post" & practice == "prescribed_grazing")
prefire <- subset(df2, treated == "pre" & practice == "prescribed_burning")
preshrub <- subset(df2, treated == "pre" & practice == "brush_management")
preplanting <- subset(df2, treated == "pre" & practice == "wildlife_hab_planting")
pregrazing <- subset(df2, treated == "pre" & practice == "prescribed_grazing")

boxplot(log(prefire$flowers+1), log(postfire$flowers+1), 
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

################################# part 2 - veg data

veg1 <- readxl::read_xlsx("C:/Users/User/Desktop/NRCS Pollinators/analyses/2024_CEAP_Pollinators_DataEntry_27dec2024.xlsx",
                          sheet = "veg")

veg1 <- as.data.frame(veg1)
veg1 <- veg1[,c(3,5,6,30:44)]
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
boxplot(prefire_veg$perc_bare, postfire_veg$perc_bare, # bare ground
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
boxplot(preshrub_veg$perc_bare, postshrub_veg$perc_bare, # bare ground
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
boxplot(pregrazing_veg$perc_bare, postgrazing_veg$perc_bare, # bare ground
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
boxplot(preplanting_veg$perc_bare, postplanting_veg$perc_bare, # bare ground
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