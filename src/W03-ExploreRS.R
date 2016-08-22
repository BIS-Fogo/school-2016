

library(raster)
library(rgdal)

setwd("B:/Landsat/raw/")

######
### raster data
# list landsat files
fls <- list.files(path = wd, pattern = "LC82100502015347LGN00*")

# stack the relevant landsat files  
ldst <- stack(fls[c(2,5:10)])

# crop landsat files to Fogo extent
ldst.crp <- crop(ldst, extent(765000, 795000, 1638000, 1667000))

# rgb plot
rgb <- plotRGB(ldst.crp, r = 4, g = 3, b = 2, stretch = "lin")

# false colour plot
rgb.fls <- plotRGB(ldst.crp, r = 5, g = 3, b = 2)

# calculate the ndvi
ndvi <- (ldst.crp[[5]] - ldst.crp[[3]]) / (ldst.crp[[5]] + ldst.crp[[3]])

# plot ndvi
plot(ndvi)

