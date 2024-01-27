# Load required packages

library(dplyr)
library(sf)
library(terra)
library(raster)
library(mapview)
library(excelR)
library(stringr)
library(tidyr)
library(campfin)
###
setwd("/media/geodata/shared/Turf/Greenville")
library(here)

## load geopackage with all the VIs
Green.plots<-read_sf(here("ToZonal/Greenville_plots_2024zonal11.gpkg"))

# Dataframe just for visualization purposes
Green.plots.vis<-Green.plots%>%
  dplyr::select(RepPlot)

# Load Shaun's field data for Y2023
VisualRat2023<-readxl::read_excel(here("Greenvilleratings_22_23.xlsx"), sheet = "KBG_2023",
                                  skip = 2)

VisualRat2023<- VisualRat2023%>%
  unite(RepPlot, Rep:EntryNum, remove=FALSE)


VisualRat2022<-visual.ratings%>%
  dplyr::select(RepPlot,VR_07_28,VR_08_10,VR_09_23)%>%
  rename_with(~ paste(., "2022", sep = "_"),starts_with("VR")) %>% # adds the year "2022" to the end of the column name
  rename_with(~str_remove(.,"\\_")) # removes the "_" after the first two characters in the column name

Plots.greenville.field.spectral<- Green.plots %>%
  left_join(VisualRat2022, by="RepPlot")%>%
  left_join(VisualRat2023, by = "RepPlot")%>%
  dplyr::rename("VR08_16_2022" = "VR08_10_2022",
                "VR09_20_2022" = "VR09_23_2022",
                "VR09_25_2023" = "VR09_26_2023",
                "VR10_23_2023" = "VR10_24_2023")%>%
  dplyr::select(1,2,36:294)%>%
  rename_with(~str_remove(., '_median'))%>%
  rename_with(~str_remove(., 'Multi_')) %>%
  rename_with(~str_remove(., 'Thermal_'))%>%
  rename_with(~str_remove(., 'RGB_'))%>%
  dplyr::select(-c(248:252))%>%
  st_drop_geometry()
  
## Conducting a pivot longer approach - will add details below
#############
## 
Plots.greenville.field.spectral <- Plots.greenville.field.spectral%>%
  dplyr::select(-c(variety_0728,id,VR07_12_2023))

### Renaming the predictors so that the index abbreviation is before the date
Plots.greenville.field.spectral2<-Plots.greenville.field.spectral%>%
  rename_with(~str_replace(., "GNDVI", "GREEN"))%>%
  rename_with(~str_replace(., "DVI", "DiffVeg"))

names(Plots.greenville.field.spectral2)
Plots.greenville.field.spectral3<-Plots.greenville.field.spectral2%>%
  rename_prefix(suffix = c("TVI","GLI"), punct = FALSE)%>%
  rename_prefix(suffix = c("PSRI","EVI"), punct = FALSE)%>%
  rename_prefix(suffix = c("HI","VARI"), punct = FALSE)%>%
  rename_prefix(suffix = c("CIRE","NDRE"), punct = FALSE)%>%
  rename_prefix(suffix = c("NDVI","BI"), punct = FALSE)%>%
  rename_prefix(suffix = c("RVI","CVI"), punct = FALSE)%>%
  rename_prefix(suffix = c("SCI","NGRDI"), punct = FALSE)%>%
  rename_prefix(suffix = c("HUE","BGI"), punct = FALSE)%>%
  rename_prefix(suffix = c("CIG","SI"), punct = FALSE)%>%
  rename_prefix(suffix = c("GREEN","NDiffVeg"), punct = FALSE)%>%
  rename_prefix(suffix = c("RAW","DGCI"), punct = FALSE)
names(Plots.greenville.field.spectral3)

### Conducting the pivot to longer
Plots.longer<- Plots.greenville.field.spectral3%>%
  pivot_longer(cols= -1,
               names_pattern = "(.*)(07_28_2022|08_16_2022|09_20_2022|04_28_2023|05_15_2023|05_30_2023|06_30_2023
               |07_18_2023|08_31_2023|09_25_2023|10_23_2023)$",
               names_to = c(".value", "names")) 

### Get rid of na's
Plots.longer<-Plots.longer%>%
  filter(!if_all(names, is.na))


#################################

library(ggpubr)
# Data visualization
ggscatter(Plots.longer, x = "DGCI", y="VR",
          color = "names",
          add = "reg.line",
          add.params = list(color ="red", fill="lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE,
          cor.coeff.args = list(method ="pearson", label.x = 0.3, label.sep = "\n"),
          xlab ="DGCI",
          ylab = "Visual Ratings")



########################
## Initial fit of a random forest model to predict visual ratings
# Get a training and validation dataset for aggregated plots
# dropping the geometry and other irrelevant  variables
plots.greenville05 <- plots.greenville03 %>%
  #filter(!is.na(LAI,GRASSCC)) %>%
  
  #select(-c(PLOTmulti0708, PLOTmulti0809, PLOTmulti0829, PLOTmulti0909, PLOTmulti0926))%>%
  drop_na()%>%
  st_drop_geometry()

### Trying to fit a MVRT
#library(mvpart)
############ Dividing the database in training % and test % n=nrow(Belder.cfv01)
n=nrow(plots.greenville05)
fifth=round(n/5)
reorder = sample(1:n,replace=FALSE)
plots.greenville05.cfv01_4 = plots.greenville05[reorder[1:fifth],] # Test subset
plots.greenville05.cfv01_5 = plots.greenville05[reorder[(fifth+1):n],] # Training subset


## another method for sampling
samp = createDataPartition(as.factor(plots.greenville05$VR_07_28), p = 0.50, list = F)

train = plots.greenville05[samp,]
test = plots.greenville05[-samp,]


# Yet another way
train_data <- plots.greenville05 %>% 
  sample_n(max(0.7*n(), 1))
#Create test set
test_data  <- anti_join(plots.greenville05, train_data, by = 'RepPlot')

test_data<- plots.greenville05[-train_data,]

rf.fit.visual.rating.uas <- randomForest(as.factor(VR_07_28) ~ NDVImulti0728+NDWImulti0728+
                                           NDREI1multi0728,
                                         data = train_data,
                                         ntree=5000,
                                         importance=TRUE)


# rf.fit.visual.rating.uas <- randomForest(as.factor(VR_07_28) ~ NDVImulti0728+NDWImulti0728+
#                                        NDREI1multi0728,
#                                     data = plots.greenville05.cfv01_5,
#                                     ntree=5000,
#                                     importance=TRUE)
# 


# Variable importance
importance(rf.fit.visual.rating.uas)
varImpPlot(rf.fit.visual.rating.uas)

# Model performance
ttc17<-predict(rf.fit.visual.rating.uas, test_data)
nombres.grasses<-c("grass_field","grass_pred")
field.grass.pred<-as.data.frame(cbind(test_data$VR_07_28, ttc17))
colnames(field.grass.pred)<-nombres.grasses

# General prediction

ttc19<- predict(rf.fit.visual.rating.uas, test_data)
plots.greenville03$predturfqua<-predict(rf.fit.visual.rating.uas,
                                        plots.greenville03)

library(caret)
# Obtain regression metrics
predictions <- rf.fit.visual.rating.uas %>% predict(test_data)

confusionMatrix(as.factor(field.grass.pred$grass_pred), 
                as.factor(field.grass.pred$grass_field))






######## Add Dark Color Index DCI + Thermal 
















Plots.longer<- Plots.greenville.field.spectral%>%
  pivot_longer(cols= -1,
               names_pattern = "(.*)(VR07_28_2022|VR08_16_2022|VR09_20_2022|VR04_28_2023|VR05_15_2023|VR05_30_2023|VR06_30_2023
               |VR07_18_2023|VR08_31_2023|VR09_25_2023|VR10_23_2023)$",
               names_to = c(".value", "names")) 


(.*)
