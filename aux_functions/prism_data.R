library(spatialEco)
library(sp)
library(raster)
library(sqldf)
library(DBI)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(VFS)
library(ISOweek)
library(entropy)
library(ptsuite)
library(moments)
library(climdex.pcic)
library(pracma)
library(stringr)

# NOTE: the user has to download PRISM daily data and store them in this path
prism_dir = setwd(paste0(pthDat, "dataPRISM/daily"))
# gridded PRISM points coordinates
input_csv = paste0(pthDat, "dataPRISM/PRISMpoints.csv")
points  = read.csv(input_csv, sep = ",", header = TRUE)
points <- subset( points, select = -X )
uniqState = unique(points$State)

###################################################################
# user-defined function to sample PRISM data

sample_prism <- function(points, prism_dir, dat_start=NULL, dat_end=NULL){
  quality = c("stable", "provisional", "early")
  filename_pattern = "PRISM_.+_.+_4kmD[1-2]_[0-9]{8}_bil.zip"
  filename_tminD1  = "PRISM_tmin_QUALITY_4kmD1_TIMESTAMP_bil.zip"
  filename_tminD2  = "PRISM_tmin_QUALITY_4kmD2_TIMESTAMP_bil.zip"
  filename_tmaxD1  = "PRISM_tmax_QUALITY_4kmD1_TIMESTAMP_bil.zip"  
  filename_tmaxD2  = "PRISM_tmax_QUALITY_4kmD2_TIMESTAMP_bil.zip"
  filename_prec    = "PRISM_ppt_QUALITY_4kmD2_TIMESTAMP_bil.zip"
  
  files <- list.files(path=prism_dir,pattern=filename_pattern, full.names=FALSE, recursive=TRUE)
  
  # find first and last date irrespective of the file quality
  frst_date = NULL
  last_date =  NULL
  for (q in quality) {
    qfiles <- sort(files[grepl(q, files)])
    ind_per = as.Date(substr(qfiles, nchar(qfiles)-15, nchar(qfiles)-8), format="%Y%m%d") >= as.Date(dat_start, format="%Y-%m-%d") & 
      as.Date(substr(qfiles, nchar(qfiles)-15, nchar(qfiles)-8), format="%Y%m%d") <= as.Date(dat_end, format="%Y-%m-%d")
    # proceed only if there are observations in the prespecified range
    if (!sum(ind_per)==0) {
      
      qfiles <- qfiles[ind_per]
      frstfile = qfiles[1]
      lastfile = qfiles[length(qfiles)]
      q_frst_date = as.Date(substr(frstfile, nchar(frstfile)-15, nchar(frstfile)-8), format="%Y%m%d")
      q_last_date = as.Date(substr(lastfile, nchar(lastfile)-15, nchar(lastfile)-8), format="%Y%m%d")
      if (is.null(frst_date) || frst_date > q_frst_date) {
        frst_date = q_frst_date
      }
      if (is.null(last_date) || last_date < q_last_date) {
        last_date = q_last_date
      }
      
      n_days = as.integer(last_date-frst_date + 1)
      results  <- data.frame(idstat=integer(),
                             State=as.character(),
                             lat=double(),
                             lon=double(),
                             date=as.Date(character()),
                             tmin=double(),
                             tmax=double(),
                             prec=double(),
                             stringsAsFactors=FALSE)
      
      for (i in seq(n_days)){
        print(paste0(i,"/",n_days))
        result = points
        date = frst_date + i - 1
        result$date = date
        
        fn_tminD1 = str_replace(filename_tminD1,"TIMESTAMP", strftime(date, format="%Y%m%d"))
        fn_tminD2 = str_replace(filename_tminD2,"TIMESTAMP", strftime(date, format="%Y%m%d"))
        fn_tmaxD1 = str_replace(filename_tmaxD1,"TIMESTAMP", strftime(date, format="%Y%m%d"))
        fn_tmaxD2 = str_replace(filename_tmaxD2,"TIMESTAMP", strftime(date, format="%Y%m%d"))
        fn_prec = str_replace(filename_prec,"TIMESTAMP", strftime(date, format="%Y%m%d"))
        
        path_tminD1 = file.path(prism_dir, "tmin", strftime(date, format="%Y"), fn_tminD1)
        path_tminD2 = file.path(prism_dir, "tmin", strftime(date, format="%Y"), fn_tminD2)
        path_tmaxD1 = file.path(prism_dir, "tmax", strftime(date, format="%Y"), fn_tmaxD1)
        path_tmaxD2 = file.path(prism_dir, "tmax", strftime(date, format="%Y"), fn_tmaxD2)
        path_prec = file.path(prism_dir, "ppt" , strftime(date, format="%Y"), fn_prec)
        
        # find best quality for each datatype
        for (q in quality){
          if  (file.exists(str_replace(path_tminD1,"QUALITY",q))){
            path_tmin = str_replace(path_tminD1,"QUALITY",q)
            fn_tmin = str_replace(fn_tminD1,"QUALITY",q)
            break
          } else {
            if  (file.exists(str_replace(path_tminD2,"QUALITY",q))){
              path_tmin = str_replace(path_tminD2,"QUALITY",q)
              fn_tmin = str_replace(fn_tminD2,"QUALITY",q)
              break
            }
          }
        }
        
        for (q in quality){
          if  (file.exists(str_replace(path_tmaxD1,"QUALITY",q))){
            path_tmax = str_replace(path_tmaxD1,"QUALITY",q)
            fn_tmax = str_replace(fn_tmaxD1,"QUALITY",q)
            break
          } else {
            if  (file.exists(str_replace(path_tmaxD2,"QUALITY",q))){
              path_tmax = str_replace(path_tmaxD2,"QUALITY",q)
              fn_tmax = str_replace(fn_tmaxD2,"QUALITY",q)
              break
            }
          }
        }
        
        for (q in quality){
          if  (file.exists(str_replace(path_prec,"QUALITY",q))){
            path_prec = str_replace(path_prec,"QUALITY",q)
            fn_prec = str_replace(fn_prec,"QUALITY",q)
            break
          }
        }
        
        workdir = tempfile(pattern="climate")
        dir.create(workdir)
        
        if(file.exists(path_tmin)){
          unzip(path_tmin, overwrite = TRUE, junkpaths = TRUE, exdir = workdir)
          result$tmin = extract(
            raster(file.path(workdir, str_replace(fn_tmin, "zip", "bil"))),
            points[c("lon","lat")])
        }  
        if(file.exists(path_tmax)){
          unzip(path_tmax, overwrite = TRUE, junkpaths = TRUE, exdir = workdir)
          result$tmax = extract(
            raster(file.path(workdir, str_replace(fn_tmax, "zip", "bil"))),
            points[c("lon","lat")])
        }  
        if(file.exists(path_prec)){
          unzip(path_prec, overwrite = TRUE, junkpaths = TRUE, exdir = workdir)
          result$prec = extract(
            raster(file.path(workdir, str_replace(fn_prec, "zip", "bil"))),
            points[c("lon","lat")])
        } 
        
        results = rbind(results, result)
        unlink(workdir, recursive = TRUE, force = TRUE)
      }
    }
  }
  return(results)
}

###################################################################
# extract PRISM data and compute spatio-temporal indexes

# define periods 
yrs = as.character(years_select) 

dd = "01"
mm = c("01", "04", "07", "10")
per_init = strcat(strcat(yrs, mm, "-"), dd, "-")
dd = rep(c("31", "30", "30", "31"), length(yrs))
mm = c("03", "06", "09", "12")
per_fin = paste0(strcat(yrs, mm, "-"), "-", dd)

# initialize results
dat = as.data.frame(matrix(NA, length(per_fin) * length(uniqState), 58))

# iterate through States
for (i in 1:length(uniqState)) {
  ind_st_i = points$State == uniqState[i]
  st_i = points[ind_st_i,]
  
  print(uniqState[i])
  
  # iterate through time windows
  for (j in 1:length(per_init)) {
    
    print(paste0(per_init[j],"/",per_fin[j]))
    
    results = sample_prism(st_i, prism_dir, dat_start = per_init[j], dat_end = per_fin[j])
    
    dat[j+(i-1)*(length(per_init)), 1] = mean(results$tmin, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 2] = median(results$tmin, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 3] = sd(results$tmin, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 4] = IQR(results$tmin, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 5] = sqrt(sum(results$tmin^2, na.rm=T))
    dat[j+(i-1)*(length(per_init)), 6] = entropy(discretize(na.omit(results$tmin), numBins = round(length(results$tmin)/1000)))
    dat[j+(i-1)*(length(per_init)), 7] = entropy.ChaoShen(discretize(na.omit(results$tmin), numBins = round(length(results$tmin)/1000)))
    dat[j+(i-1)*(length(per_init)), 8] = entropy.empirical(discretize(na.omit(results$tmin), numBins = round(length(results$tmin)/1000)))
    dat[j+(i-1)*(length(per_init)), 9] = entropy.Dirichlet(discretize(na.omit(results$tmin), numBins = round(length(results$tmin)/1000)), a = 1)
    dat[j+(i-1)*(length(per_init)), 10] = entropy.MillerMadow(discretize(na.omit(results$tmin), numBins = round(length(results$tmin)/1000)))
    dat[j+(i-1)*(length(per_init)), 11] = as.numeric(entropy.shrink(discretize(na.omit(results$tmin), numBins = round(length(results$tmin)/1000)))[1])
    dat[j+(i-1)*(length(per_init)), 12] = skewness(results$tmin, na.rm = T)
    dat[j+(i-1)*(length(per_init)), 13] = kurtosis(results$tmin, na.rm = T)
    dat[j+(i-1)*(length(per_init)), 14] = alpha_geometric_percentile(na.omit(results$tmin)+abs(min(results$tmin, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 15] = alpha_ls(na.omit(results$tmin)+abs(min(results$tmin, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 16] = alpha_mle(na.omit(results$tmin)+abs(min(results$tmin, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 17] = alpha_modified_percentile(na.omit(results$tmin)+abs(min(results$tmin, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 18] = alpha_moment(na.omit(results$tmin)+abs(min(results$tmin, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 19] = alpha_percentile(na.omit(results$tmin)+abs(min(results$tmin, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 20] = mean(results$tmax, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 21] = median(results$tmax, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 22] = sd(results$tmax, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 23] = IQR(results$tmax, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 24] = sqrt(sum(results$tmax^2, na.rm=T))
    dat[j+(i-1)*(length(per_init)), 25] = entropy(discretize(na.omit(results$tmax), numBins = round(length(results$tmax)/1000)))
    dat[j+(i-1)*(length(per_init)), 26] = entropy.ChaoShen(discretize(na.omit(results$tmax), numBins = round(length(results$tmax)/1000)))
    dat[j+(i-1)*(length(per_init)), 27] = entropy.empirical(discretize(na.omit(results$tmax), numBins = round(length(results$tmax)/1000)))
    dat[j+(i-1)*(length(per_init)), 28] = entropy.Dirichlet(discretize(na.omit(results$tmax), numBins = round(length(results$tmax)/1000)), a = 1)
    dat[j+(i-1)*(length(per_init)), 29] = entropy.MillerMadow(discretize(na.omit(results$tmax), numBins = round(length(results$tmax)/1000)))
    dat[j+(i-1)*(length(per_init)), 30] = as.numeric(entropy.shrink(discretize(na.omit(results$tmax), numBins = round(length(results$tmax)/1000)))[1])
    dat[j+(i-1)*(length(per_init)), 31] = skewness(results$tmax, na.rm = T)
    dat[j+(i-1)*(length(per_init)), 32] = kurtosis(results$tmax, na.rm = T)
    dat[j+(i-1)*(length(per_init)), 33] = alpha_geometric_percentile(na.omit(results$tmax)+abs(min(results$tmax, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 34] = alpha_ls(na.omit(results$tmax)+abs(min(results$tmax, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 35] = alpha_mle(na.omit(results$tmax)+abs(min(results$tmax, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 36] = alpha_modified_percentile(na.omit(results$tmax)+abs(min(results$tmax, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 37] = alpha_moment(na.omit(results$tmax)+abs(min(results$tmax, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 38] = alpha_percentile(na.omit(results$tmax)+abs(min(results$tmax, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 39] = mean(results$prec, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 40] = median(results$prec, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 41] = sd(results$prec, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 42] = IQR(results$prec, na.rm=T)
    dat[j+(i-1)*(length(per_init)), 43] = sqrt(sum(results$prec^2, na.rm=T))
    dat[j+(i-1)*(length(per_init)), 44] = entropy(discretize(na.omit(results$prec), numBins = round(length(results$prec)/1000)))
    dat[j+(i-1)*(length(per_init)), 45] = entropy.ChaoShen(discretize(na.omit(results$prec), numBins = round(length(results$prec)/1000)))
    dat[j+(i-1)*(length(per_init)), 46] = entropy.empirical(discretize(na.omit(results$prec), numBins = round(length(results$prec)/1000)))
    dat[j+(i-1)*(length(per_init)), 47] = entropy.Dirichlet(discretize(na.omit(results$prec), numBins = round(length(results$prec)/1000)), a = 1)
    dat[j+(i-1)*(length(per_init)), 48] = entropy.MillerMadow(discretize(na.omit(results$prec), numBins = round(length(results$prec)/1000)))
    dat[j+(i-1)*(length(per_init)), 49] = as.numeric(entropy.shrink(discretize(na.omit(results$prec), numBins = round(length(results$prec)/1000)))[1])
    dat[j+(i-1)*(length(per_init)), 50] = skewness(results$prec, na.rm = T)
    dat[j+(i-1)*(length(per_init)), 51] = kurtosis(results$prec, na.rm = T)
    dat[j+(i-1)*(length(per_init)), 52] = alpha_geometric_percentile(na.omit(results$prec)+abs(min(results$prec, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 53] = alpha_ls(na.omit(results$prec)+abs(min(results$prec, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 54] = alpha_mle(na.omit(results$prec)+abs(min(results$prec, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 55] = alpha_modified_percentile(na.omit(results$prec)+abs(min(results$prec, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 56] = alpha_moment(na.omit(results$prec)+abs(min(results$prec, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 57] = alpha_percentile(na.omit(results$prec)+abs(min(results$prec, na.rm = T))+1)$shape
    dat[j+(i-1)*(length(per_init)), 58] = as.character(uniqState[i])
  }
}

nquarter <- length(yrs)*length(uniqState)
quarter <- rep(1:4, nquarter)
years <- rep(rep(yrs, each = 4), length(uniqState))

dat_final <- cbind(years, quarter, dat[, 58], dat[, -58])

names(dat_final) = c("years", "quarter", "state", "mean.tmin", "median.tmin", "sd.tmin", "iqr.tmin", "norm.tmin", "ent.tmin", 
                     "chao.tmin", "empent.tmin", "diric.tmin", "miller.tmin", "shrink.tmin", "skew.tmin",
                     "kurt.tmin", "alpha.geom.tmin", "alpha.ls.tmin", "alpha.mle.tmin", "alpha.mperc.tmin",
                     "alpha.mom.tmin", "alpha.perc.tmin",
                     "mean.tmax", "median.tmax", "sd.tmax", "iqr.tmax", "norm.tmax", "ent.tmax", 
                     "chao.tmax", "empent.tmax", "diric.tmax", "miller.tmax", "shrink.tmax", "skew.tmax",
                     "kurt.tmax", "alpha.geom.tmax", "alpha.ls.tmax", "alpha.mle.tmax", "alpha.mperc.tmax",
                     "alpha.mom.tmax", "alpha.perc.tmax",
                     "mean.prec", "median.prec", "sd.prec", "iqr.prec", "norm.prec", "ent.prec", 
                     "chao.prec", "empent.prec", "diric.prec", "miller.prec", "shrink.prec", "skew.prec",
                     "kurt.prec", "alpha.geom.prec", "alpha.ls.prec", "alpha.mle.prec", "alpha.mperc.prec",
                     "alpha.mom.prec", "alpha.perc.prec")

write.csv(dat_final, paste0(pthSav, "prism.csv"))
