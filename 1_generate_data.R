
setwd(pthDat)
# store separate datasets
pthSav = paste0(pthDat, "tmp/")

if (savdat==1){
  
  # honeybee data
  source(paste0(mainwd,  "aux_functions/usda_data.R"))
  
  # land-use data
  source(paste0(mainwd,  "aux_functions/cdl_data.R"))
  
  # weather data (too large to be included here as raw data)
  source(paste0(mainwd,  "aux_functions/prism_data.R"))
  
  # combine all datasets
  source(paste0(mainwd,  "aux_functions/combine_data.R"))
}

# load combined data
dat = read.csv(paste0(pthDat, "bee_data.csv"))
names(dat)
dat[,1:4] <- data.frame(apply(dat[1:4], 2, as.factor))
dat[, 5:ncol(dat)] = sapply(dat[, 5:ncol(dat)], as.numeric)

# scale norms 
summary(dat)
indnorm = grepl("norm", names(dat))
dat[, indnorm] = dat[, indnorm] / 10^4
summary(dat)

# # sanity check
# # size of the data
# nrow(dat) == 44*length(years_select)*4 # States*years*quarters
# # note: missing period from USDA data
# dd=dat[dat$year==2019&dat$period==2,]
# dim(dd)
# head(dd)

setwd(mainwd)
