library(ggplot2)
library(dplyr)
library(reshape)

### 02_Descriptive

# setwd()
setwd("E:/Landsat/summerschool2016/")

# read data ()
vegetationDat <- read.table("raw/Aufnahme_PLOTS.txt")

### lets have a first look at the data
str(vegetationDat)
head(vegetationDat)
tail(vegetationDat)

### calculate total vegetation cover
#vegetationDat$VegCover <- vegetationDat$Tree_cover+vegetationDat$Shrub_cover+
#  vegetationDat$Herb_cover+vegetationDat$Moss_cover+vegetationDat$Bryo_Moos+
#  vegetationDat$Lich_Flechten

vegDataMlt <- melt(vegetationDat, id.vars = c("vegtype_ID", "vegtype_1", "vegtype_2", 
                                              "LAT", "LONG", "ELEV",  "EXP", "INCL")) 

names(vegDataMlt)
colnames(vegDataMlt) <- c("vegtype_ID", "vegtype_1", "vegtype_2", "LAT", "LONG", "ELEV", "EXP", 
                          "INCL", "veg_cover", "veg_cov_val")

## have a look at the new variable
#str(vegetationDat$VegCover)


### now lets have a closer statistical look at our new generated variable
# boxplot VegCover
#vgt.bxp <- boxplot(vegetationDat$VegCover)

# xy-plot elevation ~ vegetation cover
plot(vegetationDat$ELEV,vegetationDat$VegCover)

# convert veg_col_val as.numeric
vegDataMlt$veg_cov_val <- as.numeric(vegDataMlt$veg_cov_val)

vegDataMltSb <- subset(vegDataMlt, vegDataMlt$veg_cov_val > 0)
vegDataMltSbVeg <- subset(vegDataMltSb,
                          vegDataMltSb$veg_cover != "Tree_height" &
                          vegDataMltSb$veg_cover != "Shrub_height" &
                          vegDataMltSb$veg_cover != "Herb_height" &
                          vegDataMltSb$veg_cover != "Soil" &
                          vegDataMltSb$veg_cover != "Rock") 


density <- ggplot(vegDataMltSbVeg, aes(ELEV)) +
  geom_histogram() +
  facet_wrap( ~ veg_cover) #+
  



# at what elevations was sampled?
hist(vegetationDat$ELEV)
vgt.dns <- hist(vegetationDat$ELEV)

# what is the vegetation cover of the individual vegetation types
vgt.bxp.ty <- boxplot(vegetationDat$VegCover~vegetationDat$vegtype_1,las=2)

# mean vegetationDat$vegCover
vgt.mn <- mean(vegetationDat$VegCover)

# mean vegetationDat$vegCover na.rm
vgt.mn <- mean(vegetationDat$VegCover, na.rm = TRUE)

# lets remove all NAs of VegCover
vegetationDatClean <- subset(vegetationDat, 
                             vegetationDat$VegCover != "NA")

### Lageparameter of boxplot
# median
vgt.md <- median(vegetationDatClean$VegCover)

# min
vgt.min <- min(vegetationDatClean$VegCover)

# max
vgt.max <- max(vegetationDatClean$VegCover)

#sd
vgt.sd <- sd(vegetationDatClean$VegCover)
