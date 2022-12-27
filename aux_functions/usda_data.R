####################
# USDA colonies data
####################

# Honeybee data (colonies & stressors)
# https://usda.library.cornell.edu/concern/publications/rn301137d?locale=en
# Note: 
# There are repetitions across dataset (same year in different files)-- we kept latest releases
# We are skipping aggregate data for the whole US (see tables on pdf)

setwd(paste0(pthDat, "dataUSDA"))

# local folders path
filpath <- c('BeeColonies-05-12-2016/', 'BeeColonies-08-01-2017/', 
             'BeeColonies-08-01-2018/', 'hcny0819/', 'hcny0820/',
             'hcny0821/', 'hcny0822/')

# files names
filnam <- list()
filnam[[1]] <- c('hcny_p01_t005.csv', 'hcny_p02_t001.csv', 'hcny_p03_t007.csv', 'hcny_p04_t008.csv')
filnam[[2]] <- c('hcny_p04_t005.csv', 'hcny_p05_t001.csv', 'hcny_p06_t007.csv', 'hcny_p07_t008.csv')
filnam[[3]] <- c('hcny_p04_t005.csv', 'hcny_p05_t001.csv', 'hcny_p06_t007.csv', 'hcny_p07_t008.csv')
filnam[[4]] <- c('hcny_p03_t005.csv', 'hcny_p04_t001.csv', 'hcny_p05_t007.csv', 'hcny_p06_t008.csv')
filnam[[5]] <- c('hcny_p03_t005.csv', 'hcny_p04_t001.csv', 'hcny_p05_t007.csv', 'hcny_p06_t008.csv')
filnam[[6]] <- c('hcny_p03_t005.csv', 'hcny_p04_t001.csv', 'hcny_p05_t007.csv', 'hcny_p06_t008.csv')
filnam[[7]] <- c('hcny_p03_t005.csv', 'hcny_p04_t001.csv', 'hcny_p05_t007.csv', 'hcny_p06_t008.csv')

# season and year for each dataaset
periods = c('jan_mar', 'apr_jun', 'jul_sep', 'oct_dec')
years = years_select

# iterate across folders (i.e. release year)
for (j in 1:length(years)) {
  
  # iterate across file (i.e. seasons for a specific year)
  for (i in 1:length(periods)) {
    
    # load data
    filnam_i <- paste0("./", filpath[j], filnam[[j]][i], sep="")
    colonies <- read.csv(filnam_i, encoding = "UTF-8", stringsAsFactors = FALSE,
                         skip = 8, na.strings=c('(X)', '(Z)'), blank.lines.skip=T)
    
    # take only numeric rows
    colonies = colonies[colonies[,2] == 'd', 3:ncol(colonies)]
    # remove empty lines
    colonies = colonies[colonies[,1] != "", ]
    # fix state names (remove possible footnotes and spaces)
    colonies[,1] = gsub("[^a-zA-Z]", "", colonies[,1])
    # add variables for monthly period and year
    colonies$period = periods[i]
    colonies$year = years[j]
    
    # save data
    if (j==1 && i==1) {
      # create final dataframe
      colonies_dat <- colonies
    } else {
      # just append new data if already existing
      colonies_dat <- rbind.data.frame(colonies_dat, colonies, stringsAsFactors=FALSE)
    }
  }
}

# Assign column names
headers_col <- c('state', 
                 'num_colonies_init', 
                 'num_max_colonies', 
                 'num_lost_colonies', 
                 'perc_lost_colonies',
                 'num_added_colonies',
                 'num_renovated_colonies',
                 'perc_renovated_colonies', 
                 'period', 
                 'year')
names(colonies_dat) <- headers_col
# assign row names
row.names(colonies_dat) <- 1:nrow(colonies_dat)
# put 0 in place of -
colonies_dat[colonies_dat == "-" & !is.na(colonies_dat)] = 0

head(colonies_dat)
str(colonies_dat)

####################
# USDA stressors data
####################

# files names
filnam <- list()
filnam[[1]] <- c('hcny_p06_t002.csv', 'hcny_p07_t013.csv', 'hcny_p08_t009.csv', 'hcny_p09_t010.csv')
filnam[[2]] <- c('hcny_p10_t002.csv', 'hcny_p11_t013.csv', 'hcny_p12_t009.csv', 'hcny_p13_t010.csv')
filnam[[3]] <- c('hcny_p10_t002.csv', 'hcny_p11_t013.csv', 'hcny_p12_t009.csv', 'hcny_p13_t010.csv')
filnam[[4]] <- c('hcny_p08_t002.csv', 'hcny_p09_t013.csv', 'hcny_p10_t009.csv', 'hcny_p11_t010.csv')
filnam[[5]] <- c('hcny_p09_t002.csv', 'hcny_p10_t013.csv', 'hcny_p11_t009.csv', 'hcny_p12_t010.csv')
filnam[[6]] <- c('hcny_p09_t002.csv', 'hcny_p10_t013.csv', 'hcny_p11_t009.csv', 'hcny_p12_t010.csv')
filnam[[7]] <- c('hcny_p09_t002.csv', 'hcny_p10_t013.csv', 'hcny_p11_t009.csv', 'hcny_p12_t010.csv')

# iterate across folders (i.e. release year)
for (j in 1:length(years)) {
  
  # iterate across file (i.e. seasons for a specific year)
  for (i in 1:length(periods)) {
    
    # load data
    filnam_i <- paste0("./", filpath[j], filnam[[j]][i], sep="")
    stressors <- read.csv(filnam_i, encoding = "UTF-8", stringsAsFactors = FALSE,
                          skip = 7, na.strings=c('(X)', '(Z)'), blank.lines.skip=T)
    
    # take only numeric rows
    stressors = stressors[stressors[,2] == 'd', 3:ncol(stressors)]
    # remove empty lines
    stressors = stressors[stressors[,1] != "", ]
    # fix state names (remove possible footnotes and spaces)
    stressors[,1] = gsub("[^a-zA-Z]", "", stressors[,1])
    # add variables for monthly period and year
    stressors$period = periods[i]
    stressors$year = years[j]
    
    # save data
    if (j==1 && i==1) {
      # create final dataframe
      stressors_dat <- stressors
    } else {
      # just append new data if already existing
      stressors_dat <- rbind.data.frame(stressors_dat, stressors, stringsAsFactors=FALSE)
    }
  }
}

# Assign column names
headers_col <- c('state', 
                 'varroa_mites', 
                 'other_pests_parasites', 
                 'diseases', 
                 'pesticides',
                 'other',
                 'unknown',
                 'period', 
                 'year')
names(stressors_dat) <- headers_col
# assign row names
row.names(stressors_dat) <- 1:nrow(stressors_dat)
# put 0 in place of -
stressors_dat[stressors_dat == "-" & !is.na(stressors_dat)] = 0

head(stressors_dat)
str(stressors_dat)

fil_nam = c("colonies", "stressors")
dat_nam = list(colonies_dat, stressors_dat)

for (i in 1:length(dat_nam)) {
  tit_i = paste0(pthSav, fil_nam[i], ".csv")
  write.table(dat_nam[[i]], row.names=T, col.names=T, tit_i)
}
