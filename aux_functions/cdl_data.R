####################
# CDL land use data
####################

setwd(paste0(pthDat, "dataCDL/"))

if(!require(dplyr)){install.packages("dplyr", dep=T); library(dplyr)}
if(!require(tibble)){install.packages("tibble", dep=T); library(tibble)}
if(!require(readxl)){install.packages("readxl", dep=T); library(readxl)}
if(!require(datasets)){install.packages("datasets", dep=T); library(datasets)}

years <- as.character(years_select)
states <-  2:50 

crop <- c("Alfalfa","Almonds","Apples","Apricots","Aquaculture","Asparagus","Barley","Barren","Barren","Blueberries","Broccoli","Buckwheat",
          "Cabbage","Camelina","Caneberries","Canola","Cantaloupes","Carrots","Cauliflower","Celery","Cherries","Chick Peas","Christmas Trees",
          "Citrus","Clover/Wildflowers","Corn","Cotton","Cranberries","Cucumbers","Dbl Crop Barley/Corn","Dbl Crop Barley/Sorghum",
          "Dbl Crop Barley/Soybeans","Dbl Crop Corn/Soybeans","Dbl Crop Durum Wht/Sorghum","Dbl Crop Lettuce/Barley","Dbl Crop Lettuce/Cantaloupe",
          "Dbl Crop Lettuce/Cotton","Dbl Crop Lettuce/Durum Wht","Dbl Crop Oats/Corn","Dbl Crop Soybeans/Cotton","Dbl Crop Soybeans/Oats",
          "Dbl Crop WinWht/Corn","Dbl Crop WinWht/Cotton","Dbl Crop WinWht/Sorghum","Dbl Crop WinWht/Soybeans","Dry Beans","Durum Wheat","Eggplants",
          "Fallow/Idle Cropland","Flaxseed","Garlic","Gourds","Grapes","Greens","Honeydew Melons","Hops","Lentils","Lettuce","Millet","Mint",
          "Misc Vegs & Fruits","Mustard","Nectarines","Oats","Olives","Onions","Oranges","Other Crops","Other Hay/Non Alfalfa","Other Small Grains",
          "Other Tree Crops","Peaches","Peanuts","Pears","Peas","Pecans","Peppers","Pistachios","Plums","Pomegranates","Pop or Orn Corn","Potatoes",
          "Prunes","Pumpkins","Radishes","Rape Seed","Rice","Rye","Safflower","Sorghum","Soybeans","Speltz","Spring Wheat","Squash","Strawberries",
          "Sugarbeets","Sugarcane","Sunflower","Sweet Corn","Sweet Potatoes","Switchgrass","Tobacco","Tomatoes","Triticale","Turnips","Vetch",
          "Walnuts","Watermelons","Winter Wheat")

forest <- c("Deciduous Forest","Evergreen Forest","Mixed Forest","Forest")

pasture <- c("Grassland/Pasture","Pasture/Grass")

water <- c("Herbaceous Wetlands","Open Water","Perennial Ice/Snow","Water","Wetlands","Woody Wetlands")

rangeland <- c("Nonag/Undefined","Shrubland","Shrubland","Sod/Grass Seed","Herbs")

developed <- c("Developed","Developed/High Intensity","Developed/Low Intensity","Developed/Med Intensity","Developed/Open Space")


nq <- length(years)*length(states)

cdl_data <- data.frame(matrix(ncol = 9 , nrow = nq))


x <- c("state", "year", "crop", "forest", "pasture", "water", "rangeland", "developed", "total")
colnames(cdl_data) <- x


state_names <- rep(c(state.name[c(1,3:8)], "DC", state.name[c(9:10,12:50)]), length(years))


cdl_data$state <- state_names
cdl_data$year <- rep(years_select, each=length(states))


for(i in 1:length(years)) {
  
  for(j in states) {
    
    df <-  as.data.frame(read_excel(paste0(years[i],"_CDL_Histogram_Summary.xlsx"), 
                                    sheet = j, 
                                    range = cell_cols("A:D")))
    
    cdl_data$crop[(cdl_data$state==state_names[j-1] & cdl_data$year==years[i])] <- sum(df[df$Category %in% crop, "Acreage"], na.rm=T)
    cdl_data$forest[(cdl_data$state==state_names[j-1] & cdl_data$year==years[i])] <- sum(df[df$Category %in% forest, "Acreage"], na.rm=T)
    cdl_data$pasture[(cdl_data$state==state_names[j-1] & cdl_data$year==years[i])] <- sum(df[df$Category %in% pasture, "Acreage"], na.rm=T)
    cdl_data$water[(cdl_data$state==state_names[j-1] & cdl_data$year==years[i])] <- sum(df[df$Category %in% water, "Acreage"], na.rm=T)
    cdl_data$rangeland[(cdl_data$state==state_names[j-1] & cdl_data$year==years[i])] <- sum(df[df$Category %in% rangeland, "Acreage"], na.rm=T)
    cdl_data$developed[(cdl_data$state==state_names[j-1] & cdl_data$year==years[i])] <- sum(df[df$Category %in% developed, "Acreage"], na.rm=T)
    
  }
  
}

cdl_data$total <- apply(cdl_data[ , 3:8], 1, sum)

write.csv(cdl_data, row.names=FALSE, paste0(pthSav, "land.csv"))
