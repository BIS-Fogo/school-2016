library(ggplot2)
library(plyr)
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


########
########
#### First, do some data manipulation to get the data into shape
### melt dataframe
vegDataMlt <- melt(vegetationDat, id.vars = c("vegtype_ID", "vegtype_1", "vegtype_2", 
                                              "LAT", "LONG", "ELEV",  "EXP", "INCL")) 

# rename colnames of melted dataframe
colnames(vegDataMlt) <- c("vegtype_ID", "vegtype_1", "vegtype_2", "LAT", "LONG", "ELEV", "EXP", 
                          "INCL", "veg_cover", "veg_cov_val")

# convert veg_col_val as.numeric
vegDataMlt$veg_cov_val <- as.numeric(vegDataMlt$veg_cov_val)

vegDataMltSb <- subset(vegDataMlt, vegDataMlt$veg_cov_val > 0)
vegDataMltSbVeg <- subset(vegDataMltSb,
                          vegDataMltSb$veg_cover != "Tree_height" &
                          vegDataMltSb$veg_cover != "Shrub_height" &
                          vegDataMltSb$veg_cover != "Herb_height" &
                          vegDataMltSb$veg_cover != "Soil" &
                          vegDataMltSb$veg_cover != "Rock") 


########
########
#### ok, let's start plotting
### density plot
density.pl <- ggplot(vegDataMltSbVeg, aes(ELEV)) +
  geom_histogram() +
  facet_wrap( ~ veg_cover) #+


### boxplot elevation
boxplot.pl <- ggplot(vegDataMltSbVeg, aes(veg_cover, ELEV)) +
  geom_boxplot()


### elevation plot
# set up function for regression equation 
lm_eqn = function(vegDataMltSbVeg){
  m = lm(ELEV ~ veg_cov_val, vegDataMltSbVeg);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

# apply regression equation function on each vegetation cover type
eq <- ddply(vegDataMltSbVeg,.(veg_cover),lm_eqn)

# plot elevation plot
elevation.pl <- ggplot(vegDataMltSbVeg, aes(veg_cov_val, ELEV)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  geom_text(data = eq, aes(x = 70, y = 1750, label = V1), parse = TRUE, inherit.aes = FALSE) +
  facet_wrap( ~ veg_cover)
