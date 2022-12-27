######################
# combine bee-weather
######################

# load data
colonies    <- read.table(paste0(pthSav, "colonies.csv"), encoding = "UTF-8", header = T)
stressors   <- read.table(paste0(pthSav, "stressors.csv"), encoding = "UTF-8", header = T)
land_dat = read.csv(paste0(pthSav, "land.csv"), encoding = "UTF-8", header = T)
clim <- read.table(paste0(pthSav, "prism.csv"), encoding = "UTF-8", header = T, sep = ",")
clim        <- clim[, 2:ncol(clim)]
# remove grand total
stressors <- stressors[!stressors$state == "UnitedStates",]
stressors$year = as.factor(stressors$year)
colonies <- colonies[!colonies$state == "UnitedStates",]
colonies$year = as.factor(colonies$year)

# sanity check before merging
sum(colonies$period != stressors$period)
sum(colonies$state != stressors$state)
# Merge colonies and stressors
datf = merge(colonies, stressors, by = intersect(names(colonies), names(stressors)))
names(datf)

# prepare climate data
unique(clim$years) %in% unique(datf$year)

# DistrictofColumbia is missing in bee data
indST = unique(clim$state) %in% unique(datf$state)
clim$state = gsub(" ", "", clim$state, fixed = TRUE)
indST = unique(clim$state) %in% unique(datf$state)
unique(clim$state)[!indST]
indST =  unique(datf$state) %in% unique(clim$state)
unique(datf$state)[!indST]
selST = unique(datf$state)[indST]
indST = datf$state %in% selST
# remove states missing in climate
datf$state[indST]
datf = datf[indST,]

# homog. quarters
unique(clim$quarter) %in% unique(datf$period)
per = rep(NA, length(datf$period))
per[datf$period == "jan_mar"] = 1
per[datf$period == "apr_jun"] = 2
per[datf$period == "jul_sep"] = 3
per[datf$period == "oct_dec"] = 4
per = as.factor(per)
datf$period = per
unique(datf$period)

st_t = unique(datf$state)
yr_t = unique(datf$year)
names(clim)[1:3] = names(datf)[3:1]
datF = merge(datf, clim)
names(datF)
# index for climatic variables in use
inddd = c(c(c(1,3), 5:6, 12:14), c(c(1,3), 5:6, 12:14)+19, c(c(1,3), 5:6, 12:14)+38)
datF = cbind.data.frame(datF[, 1:ncol(datf)], datF[, (ncol(datf)+1):ncol(datF)][inddd])
names(datF)

# select bee data
indSel = c(1:6, 8, 9, 11:ncol(datF))
names(datF)[indSel]
X = datF[,indSel]
names(X)

dat = X

######################
# add climatic regions
######################

regionUS = read.csv(paste0(pthSav, "Climate-Regions.csv"), 
                    encoding = "UTF-8", header = T, sep = ";")
regionUS = regionUS[,-3]
regionUS$States = as.factor(gsub("[^a-zA-Z]", "", regionUS$States))
unique(regionUS$Region)
regionUS$Region = as.factor(gsub("[^0-9A-Za-z]", "", regionUS$Region))
indRemReg = regionUS$States %in% unique(dat$state)
regionUS = regionUS[indRemReg, ]
regions = data.frame(state = regionUS$States, region = regionUS$Region)
dat=merge.data.frame(regions, dat)
dat$region = as.factor(dat$region)
str(dat)

######################
# add land use 
######################
head(land_dat)
names(land_dat)
dim(land_dat)

if (decompose_green==0) {
  
  # Approach in Naug (2009)
  green.urban = (land_dat$crop +
                   land_dat$forest +
                   land_dat$pasture +
                   land_dat$rangeland)/land_dat$developed
  land_dat$state = as.factor(gsub("[^0-9A-Za-z]", "", land_dat$state))
  land_dat = cbind.data.frame(state = land_dat$state, year = land_dat$year, green.urban)
  # Missing sates: Delaware, DC, Nevada, NewHampshire, RhodeIsland
  land_dat = land_dat[land_dat$state %in% dat$state, ]
  dat = merge(dat, land_dat, by=c("state","year"))
  
} else{
  
  # decompose crops
  green.urban.pos = (land_dat$forest +   
                       land_dat$pasture +   
                       land_dat$rangeland)/land_dat$developed  
  green.urban.neg = land_dat$crop/land_dat$developed   
  land_dat$state = as.factor(gsub("[^0-9A-Za-z]", "", land_dat$state))
  land_dat = cbind.data.frame(state = land_dat$state, year = land_dat$year, 
                              green.urban.pos, green.urban.neg)
  # Missing sates: Delaware, DC, Nevada, NewHampshire, RhodeIsland 
  land_dat = land_dat[land_dat$state %in% dat$state, ]
  dat = merge(dat, land_dat, by=c("state","year"))
  
}

######################
# full data
######################
names(dat)
str(dat)
dat[, 5:ncol(dat)] = sapply(dat[, 5:ncol(dat)], as.character)
dat[, 5:ncol(dat)] = sapply(dat[, 5:ncol(dat)], as.numeric)
str(dat)

write.csv(dat, row.names=FALSE, paste0(pthDat, "bee_data.csv"))