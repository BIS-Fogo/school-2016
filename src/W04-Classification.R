library(raster)
library(rgdal)
library(caret)
library(randomForest)
library(Rsenal)

setwd("B:/Landsat/raw/")

################################################################################
# Read and pre-process data
################################################################################
### load and pre-process the raster data
# stack landsat files  
ldst <- stack("LC82100502015347LGN00_B1.TIF","LC82100502015347LGN00_B2.TIF",
              "LC82100502015347LGN00_B3.TIF","LC82100502015347LGN00_B4.TIF",
              "LC82100502015347LGN00_B5.TIF","LC82100502015347LGN00_B6.TIF",
              "LC82100502015347LGN00_B7.TIF")
# crop landsat files
ldst.crp <- crop(ldst, extent(765000, 795000, 1638000, 1667000))

# read shapelayer training sites
trainingsites <- readOGR("training/trainingsites_io.shp", layer = "trainingsites_io")

# get projection of Landsat and training sites
projection(ldst.crp)
projection(trainingsites)

# reproject training sites
trainingsites.utm <- spTransform(trainingsites,proj4string(ldst.crp))
projection(trainingsites.utm)

## plot landsat combined with training sites
plot(ldst.crp[[5]], asp=0)
plot(trainingsites.utm, add=T)

# extract information of landsat bands for the location and
# return results as data.frame
train.df <- extract(ldst.crp, trainingsites.utm, df = TRUE)
summary(train.df)

# add a new column to the data.frame that includes the land cover 
# information from the training site shapefiles
trainingsites.utm$Class[1] # class name of the first poygon
train.df$class <- trainingsites.utm$Class[train.df$ID]
head(train.df)


################################################################################
#Model training
################################################################################

## Split data.frame into training and testing data
set.seed(500)

# 30% training, 70% testing
trainIndex <- createDataPartition(train.df$class, 
                                  p = 0.3,
                                  list = FALSE,
                                  times = 1)

trainData <- train.df[trainIndex,]
testData <- train.df[-trainIndex,]

### define predictors and response
predictors <- trainData[,2:8]
response <- trainData$class

###feature plot
featurePlot(predictors,response,plot="pairs",
            auto.key = list(columns = 2))

### train a random forest model
model <- train(predictors, response, method = "rf")

# print result of rf model
print(model)
################################################################################
#Explore Model and predict on whole Landsat scene
################################################################################

# plot tuning results
plot(model)

# plot Variable Importance
plot(varImp(model))

### predict on the whole landsat scene
prediction <- predict(ldst.crp, model)

#plot the land cover map
spplot(prediction)
#adapt the colors
lucc <- spplot(prediction,
               col.regions = rev(c("blue", "grey", "brown", "black", "darkgreen", "green")))

################################################################################
# validation
################################################################################
#Predict on left-out test data
prediction_test <- predict(model, testData)
#calculate contingency table
ctab <- table(testData$class, prediction_test)
#write tabel
write.csv(ctab,"ctab.csv")

# calculate the kappa index
kappa.index <- kstat(testData$class, predict(model, testData))
