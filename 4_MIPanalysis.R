setwd(mainwd)

if (length(years_select)==5 && decompose_green==0) {
  # data for 2015-2019 (as in the main text)
  resfolder = paste0(mainwd, "MIPres/15_19")
  tuningfolder = paste0(mainwd, "MIPres/15_19/tuning")
} else if (length(years_select)==7 && decompose_green==0) {
  # data for 2015-2021
  resfolder = paste0(mainwd, "MIPres/15_21")
  tuningfolder = paste0(mainwd, "MIPres/15_21/tuning")
} else if(length(years_select)==7 && decompose_green==1) {
  # data for 2015-2021 and decomposing the green-area index
  resfolder = paste0(mainwd, "MIPres/15_21_decomp")
  tuningfolder = paste0(mainwd, "MIPres/15_21_decomp/tuning")
}

# load plotting functions
source(paste0(mainwd,  "aux_functions/MIPplots.R"))

if(!require(ggplot2)){install.packages("ggplot2", dep=T); library(ggplot2)}
if(!require(GGally)){install.packages("GGally", dep=T); library(GGally)}
if(!require(ggpubr)){install.packages("ggpubr", dep=T); library(ggpubr)}
if(!require(robustbase)){install.packages("robustbase", dep=T); library(robustbase)}
if(!require(xtable)){install.packages("xtable", dep=T); library(xtable)}

par(mfrow=c(1,1))

# focusing on rAIC-solutions for the full dataset
setwd(tuningfolder)
myseed = "1"
MIPname = "MIPIC.csv" 
# proportion and number of points in use
Nte = 1
if (length(years_select)==5) {
  # sample size
  nnn = 674
  # breakdown point/trimming proportions
  bdpt = seq(0, 0.1, by=0.025)
  bdpt = c(bdpt, 0.15)
  allbdps = 0
  my_ylim = c(-900,-400)
} else if(length(years_select)==7) {
  # sample size
  nnn = 828 
  # breakdown point/trimming proportions
  bdpt = seq(0, 0.15, by=0.05)
  allbdps = 2
  my_ylim = c(-1200,-400)
}

#########################
# FIG S16: rAIC
########################

fig = list()
fl = list.files()[grepl(MIPname, list.files())]
fl = fl[grepl(myseed, fl)]
fl = fl[grepl(nnn, fl)]
for (i in 1:length(fl)) {
  dat = read.csv(fl[i])
  names(dat) = c("k", "BIC", "AIC", "time")
  if (i==1) {
    shl = T
  } else {
    shl = F
  }
  fig[[i]] <- plot_solAICgg(dat,  tit=paste0(bdpt[i]*100, "% trimming"), 
                            showlegend = shl, ylims= my_ylim)
}

if (allbdps == 1){
  p <- ggarrange(fig[[1]], fig[[2]], fig[[3]], fig[[4]], fig[[5]], fig[[6]],
                 ncol = 3, nrow = 2)
} else if (allbdps==0){
  p <- ggarrange(fig[[1]], fig[[3]], fig[[5]], fig[[6]],
                 ncol = 4, nrow = 1)
} else if (allbdps==2){
  p <- ggarrange(fig[[1]], fig[[2]], fig[[3]], fig[[4]],
                 ncol = 4, nrow = 1)
}
print(p)
if (length(years_select)==5 && decompose_green==0) {
  print("Figure S16")
  invisible(readline(prompt="Press [enter] to continue"))
  if (savfig==1) {
    ggsave(filename = "rAIC.eps",
           plot = print(p),
           path = imgPth,
           width = 15,
           height = 8,
           units = "in",
           dpi = "retina",
           device = cairo_ps)
  }
}
##################
# beta estimates
##################
# same seed as R version used in Julia through Rcall package
RNGkind(sample.kind = "Rounding")
fl = list.files()[grepl(MIPname, list.files())]
fl = fl[grepl(myseed, fl)]
seedJl= as.numeric(strsplit(fl, "-")[[1]][4])
set.seed(seedJl)

base::load(paste0(pthDat, "beeLM.RData"))

names(Zlog)
oldyl = Zlog$y
yl = oldyl
oldXl = Zlog[, 2:ncol(Zlog)]
Xl = oldXl

# variable in use for our LM
xmat = matrix(NA, ncol(Xl), 1)
xmat[,1] = colnames(Xl)
print(xmat)
xtable(xmat)
if (length(years_select)==5 && decompose_green==0) {
  print("Table S4")
  invisible(readline(prompt="Press [enter] to continue"))
}

# remove reference categories for dummies
# and shuffle/partition the data as in the Julia code
names(Xl)
if (length(years_select)==5){
  Xl = Xl[-c(5,6,18)]
} else if(length(years_select)==7){
  Xl = Xl[-c(7,8,20)]
}
nn = names(Xl)
nall = dim(Xl)[1]
N = floor(Nte*nall)
idx = sample(1:nall,N,replace=FALSE)
Xtest = as.matrix(Xl[-idx,])
Xl = as.matrix(Xl[idx,])
Ytest = as.matrix(yl[-idx])
yl = as.matrix(yl[idx])

fl = list.files()[grepl("B_est.csv", list.files())]
fl = fl[grepl(myseed, fl)]
fl = fl[grepl(nnn, fl)]
nparam = nrow(read.csv(fl[1])) 
solMIP = as.data.frame(matrix(NA, nparam, length(fl)))
solspLTS = solMIP
cont = rep(NA, length(fl))
n = rep(NA, length(fl))
p = rep(NA, length(fl))
k_s = rep(NA, length(fl))
for (i in 1:length(fl)) {
  n[i] = strsplit(fl, "-")[[i]][1]
  cont[i] = strsplit(fl, "-")[[i]][3]
  p[i] = strsplit(fl, "-")[[i]][2]
  solMIP[, i] = read.csv(fl[i])[, 2]
  solspLTS[, i] = read.csv(fl[i])[, 1]
} 
rownames(solMIP) = c("intercept", nn)
rownames(solspLTS) = c("intercept", nn)
# MIP solution (betas and sparsity levels)
solMIP
colSums(solMIP!=0)
solMIPf = solMIP
# sparseLTS solution (betas and sparsity levels)
solspLTS
colSums(solspLTS!=0)

##################
# outlier detection
##################

fl = list.files()[grepl("Phi_est.csv", list.files())]
fl = fl[grepl(myseed, fl)]
fl = fl[grepl(nnn, fl)]
nparam = nrow(read.csv(fl[1])) 
solMIP = as.data.frame(matrix(NA, nparam, length(fl)))
solspLTS = solMIP
cont = rep(NA, length(fl))
n = rep(NA, length(fl))
p = rep(NA, length(fl))
k_s = rep(NA, length(fl))
for (i in 1:length(fl)) {
  n[i] = strsplit(fl, "-")[[i]][1]
  cont[i] = strsplit(fl, "-")[[i]][3]
  p[i] = strsplit(fl, "-")[[i]][2]
  solMIP[, i] = read.csv(fl[i])[, 2]
  solspLTS[, i] = read.csv(fl[i])[, 1]
}
solMIP
hist(rowSums(solMIP), main="Number of times each unit was flagged as non-outliers by MIP")
which(solMIP[,3]==0)
solspLTS
hist(rowSums(solspLTS), main="Number of times each unit was flagged as non-outliers by sparseLTS")
# MIP solution with 10% trimming discussed in the paper
if (length(years_select)==5){
  bdpi = 5
} else if (length(years_select)==7){
  bdpi = 3
}
XX = as.matrix(Xl[which(solMIP[, bdpi]==1), solMIPf[2:nrow(solMIPf), bdpi] != 0])
yy = yl[which(solMIP[, bdpi]==1)]
print(summary(lm(yy~XX)))
print(xtable(summary(lm(yy~XX))))
if (length(years_select)==5 && decompose_green==0) {
  print("Table 1")
} else if (length(years_select)==7 && decompose_green==0) {
  print("Table S13")
  
}
invisible(readline(prompt="Press [enter] to continue"))

# default MIP diagnostics
par(mfrow=c(3,2))
fits = as.matrix(cbind(rep(1, length(yl)), Xl)) %*% solMIPf[, bdpi]
resi = yl - fits
resistd = resi/summary(lm(yy~XX))$sigma
plot(resistd, ylab="MIP residuals", main="MIP std. residuals with outlying cases")
indout=as.logical(solMIP[, bdpi]==0)
points((1:length(yl))[indout], resistd[indout], col="red")
abline(c(0, 0), col="red")
abline(c(qnorm(0.975), 0), col="red",lty=2)
abline(c(qnorm(0.025), 0), col="red",lty=2)
plot(lm(yy~XX))
plot(yy, fits[!indout], xlab="y", ylab="Fitted values", main="Fitted values vs true values")
abline(c(0, 1), col="red")

#########################
# FIG S18
#########################
# custom MIP diagnostics
XXX = as.matrix(Xl[, solMIPf[2:nrow(solMIPf), bdpi] != 0])
yyy = yl
source(paste0(mainwd,  "aux_functions/MIPdiagnosticsplot.R"))
print(figure)
if (length(years_select)==5 && decompose_green==0) {
  print("Figure S18")
  invisible(readline(prompt="Press [enter] to continue"))
  if (savfig==1) {
    ggsave(filename = "diagnostics.eps",
           plot = print(figure),
           path = imgPth,
           width = 3000,
           height = 3000,
           units = "px",
           dpi = "retina",
           device = cairo_ps)
  }
}

# features selected by MIP (with no dummies)
idcols = rep(NA, length(oldyl))
idcols[1:(dim(Xl)[2])] = solMIPf[2:nrow(solMIPf), bdpi] != 0
# features included in MIP modeling (i.e. removing reference categories for dummies)
idcols2 = rep(NA, length(oldyl))
idcols2[1:dim(oldXl)[2]] = colnames(oldXl) %in% colnames(Xl)
# non-outlying cases selected by MIP
idrows=solMIP[, bdpi]==1
# store data matrix
MIPdat = cbind.data.frame("idx"=idx,"sub"=idcols2, "sel"=idcols, 
                          "out"=idrows, "y"=oldyl,
                          oldXl)
if (savdat==1){
  write.csv(MIPdat, paste0(pthDat, "MIPdat.csv"), row.names = F)
}

####################
# Studying the outliers
####################
# setwd(tuningfolder) 
base::load(paste0(pthDat, "beeLMfac.RData"))
names(Zlogfac)
XXX = Zlogfac[idx,c(4, 2, 3, 5)]
names(XXX)
head(XXX[indout,])
head(Xl[indout,])

##########################
# Table S6
#########################
# extreme residuals
aaa = XXX[indout,]
resout = resi[indout]/summary(lm(yy~XX))$sigma
ccc = qnorm(0.975)
print("MIP std res higher than qnorm(0.975)")
print(summary(aaa[resout > ccc, ]))
xtable(summary(aaa[resout > ccc, ]))
print("MIP std res lower than -qnorm(0.975)")
print(summary(aaa[resout <= -ccc, ]))
xtable(summary(aaa[resout <= -ccc, ]))

ccc = qnorm(0.999)
print("MIP std res higher than qnorm(0.999)")
print(summary(aaa[resout > ccc, ]))
xtable(summary(aaa[resout > ccc, ]))
print("MIP std res lower than -qnorm(0.999)")
print(summary(aaa[resout <= -ccc, ]))
xtable(summary(aaa[resout <= -ccc, ]))
if (length(years_select)==5 && decompose_green==0) {
  print("Table S6")
  invisible(readline(prompt="Press [enter] to continue"))
}
# plot residuals
par(mfrow=c(1,1))
plot(resi)
points((1:nnn)[indout& resi<0], resi[indout& resi<0,], col="green")
points((1:nnn)[indout& resi>0], resi[indout& resi>0,], col="red")

##########################
# Figure S20: outliers box plots
#########################
# MIP solution
XXX = as.matrix(Xl[, solMIPf[2:nrow(solMIPf), bdpi] != 0])
yyy = yl
summary(lm(yyy[!indout]~XXX[!indout,]))

# renaming and exclude dummies
ddd = cbind.data.frame("colony_loss"=yyy, XXX, "Units" = indout)
names(ddd)
if (length(years_select)==5) {
  inddd = c(1, 17:28)
} else if (length(years_select)==7) {
  inddd = -c(2:18)
}
ddd = ddd[, inddd]
ccc=0
ddd$Units[ddd$Units == T & (resi/summary(lm(yy~XX))$sigma) > ccc] = "Positive Outliers"
ddd$Units[ddd$Units == T & (resi/summary(lm(yy~XX))$sigma) < -ccc] = "Negative Outliers"
ddd$Units[ddd$Units == F] = "Non-outlying"

if(!require(quantable)){install.packages("quantable", dep=T); library(quantable)}
names(ddd)
tmp = ddd[, 1:(ncol(ddd)-1)]
dim(tmp)
tmp = robustscale(tmp, scale =T,preserveScale=F)
ddd = cbind.data.frame(tmp$data, Units=ddd$Units)
median(ddd$varroa_mites)
mad(ddd$varroa_mites)

# Reshaping data
if(!require(reshape2)){install.packages("reshape2", dep=T); library(reshape2)}
ddl <- melt(ddd, id = "Units")
head(ddl) 
ddl$Units = factor(ddl$Units, levels = c("Positive Outliers","Non-outlying","Negative Outliers"))
p1 = ggplot(ddl, aes(y = variable, x = value, color=Units, fill=Units)) +  # ggplot function
  geom_boxplot(lwd=0.5, width=0.6, alpha=1,
               outlier.size = 3, outlier.alpha = 1, outlier.shape = 21, color="black") +
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#7CAE00")) +
  ylab("Variable") +
  xlab("Value") +
  theme_bw() +
  theme(legend.position = c(0.87, 0.2), legend.title = element_text(size=12), 
        legend.text = element_text(size=12)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text = element_text(colour = 1, size = 5),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.key.height=unit(1, "cm"), 
        legend.key.width=unit(1, "cm")) +
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title.x = element_text(size = 12,face="bold"),
        axis.title.y = element_text(size = 12,face="bold")) +
  theme(axis.text.x = element_text(size=12), # ,angle = 70, vjust = 0.5
        axis.text.y = element_text(size=12)) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) # +
print(p1) 

if (length(years_select)==5 && decompose_green==0) {
  print("Figure S20")
  invisible(readline(prompt="Press [enter] to continue"))
  if (savfig==1) {
    ggsave(filename = "outliers_boxplots.eps",
           plot = print(p1),
           path = imgPth,
           width = 10,
           height = 9,
           # units="mm",
           dpi = "retina",
           device = cairo_ps)
  }
}

#########################
# FIG S19
#########################

# scatter matrix and correlations
ddd2 = ddd[, 1:(ncol(ddd)-1)]
ddd2$period = Zlogfac$period[idx]

p = ggpairs(ddd2, legend = 1, aes(color=period),
        columns=names(ddd2[,1:(ncol(ddd2)-1)]),
        lower=list(continuous='points'),
        diag = list(discrete="barDiag",
                    continuous = wrap("densityDiag", alpha=0.5)),
        upper=list(continuous = wrap("cor", size = 3))) +
  theme(legend.position = "top", legend.title = element_text(size=18),
        legend.text = element_text(size=16)) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
print(p)

if (length(years_select)==5 && decompose_green==0) {
  print("Figure S19")
  invisible(readline(prompt="Press [enter] to continue"))
  if (savfig==1) {
    ggsave(filename = "pairs_selected.eps",
           plot = p,
           path = imgPth,
           width = 12,
           height = 12,
           dpi = "retina",
           device = cairo_ps)
  }
}

# MIP residuals with outliers across trimming levels
par(mfrow=c(2,length(bdpt)/2))
sbdp = bdpt
for (i in 1:ncol(solMIP)) {
  bdpii=i
  resi = yl - as.matrix(cbind(rep(1, length(yl)), Xl)) %*% solMIPf[, bdpii]
  plot(resi, xlab ="MIP residuals", main = paste0(sbdp[bdpii]*100, "% trimming"))
  indout=as.logical(solMIP[, bdpii]==0)
  points((1:length(yl))[indout], resi[indout], col="red")
  abline(0, 0, col="red")
}

###################
# prediction error (on testing data)
###################

if (length(years_select)==5 && decompose_green==0){
  
  setwd(resfolder) 
  
  # subset of trimming levels in use
  bdpsel = seq(0, 0.15, 0.05)
  indbdp = bdpt %in% bdpsel
  # bdpsel = bdpt # take them all
  ntrim = length(bdpt)
  
  fl = list.files()[grepl("OUT.csv", list.files())]
  seedss = 2020:2027
  sold = array(NA, c(length(seedss), 6, ntrim))
  for (j in 1:length(seedss)) {
    tmp = fl[grepl(seedss[j], fl)]  
    for (jj in 1:length(tmp)){
      tmpp = read.csv(tmp[jj])
      sold[j, ,jj] = as.matrix(tmpp)
    }
  }
  
  sol = matrix(NA, ntrim, 6)
  solv = matrix(NA, ntrim, 6)
  for (i in 1:ntrim) {
    sol[i, ] =  colMedians(sold[,,i])
    for (hh in 1:ncol(sold[,,i])){
      solv[i, hh] = mad(sold[, hh,i])
    }
  }
  
  sold = as.data.frame(sol)
  solv = as.data.frame(solv)
  names(sold) = names(tmpp)
  names(solv) = paste0(names(tmpp), "_se")
  
  # take a subset of trimming levels
  bdpt = bdpt[indbdp]
  sold = sold[indbdp, ]
  solv = solv[indbdp, ]
  
  
  ###################
  # Figure S17
  ###################
  # TRMSPE
  nbdp = length(bdpt)
  xlab = "Trimming level"
  est_nam = c("sparseLTS", "MIP")
  soldd = sold[, c(2, 5)]
  names(soldd) = c("sparseLTS median", "MIP median")
  solvv = solv[, c(2, 5)]
  names(solvv) = c("sparseLTS mad", "MIP mad")
  p1 = plot_sol_se_gg(soldd, se=solvv, 
                      x=bdpt, ylab = "TRMSPE", 
                      xlab = expression(k[n]/n),
                      tit = "Trimmed prediction error vs trimming level",
                      showlegend = T)
  p1
  
  # RMSPE (not really informative/useful)
  soldd = sold[, c(1, 4)]
  names(soldd) = c("sparseLTS median", "MIP median")
  solvv = solv[, c(1, 4)]
  names(solvv) = c("sparseLTS mad", "MIP mad")
  p2 = plot_sol_se_gg(soldd, se=solvv, 
                      x=bdpt, ylab = "RMSPE", 
                      xlab = expression(k[n]/n),
                      tit = "Prediction error vs trimming level",
                      showlegend = T)
  p2
  
  # sparsity levels distribution
  fl = list.files()[grepl("B_est.csv", list.files())]
  sold = array(NA, c(length(seedss), 2, 6))
  for (j in 1:length(seedss)) {
    tmp = fl[grepl(seedss[j], fl)]  
    for (jj in 1:length(tmp)){
      tmpp = read.csv(tmp[jj])
      sold[j, ,jj] = as.matrix(colSums(tmpp!=0))
    }
  }
  
  sol = matrix(NA, 6, 2)
  solv = matrix(NA, 6, 2)
  for (i in 1:6) {
    # sol[i, ] =  colMeans(sold[,,i])
    sol[i, ] =  colMedians(sold[,,i])
    for (hh in 1:ncol(sold[,,i])){
      solv[i, hh] = mad(sold[, hh,i])
      # solv[i, hh] = sd(sold[, hh,i])
    }
  }
  sold = as.data.frame(sol)
  solv = as.data.frame(solv)
  names(sold) = names(tmpp)
  names(solv) = paste0(names(tmpp), "_se")
  
  # subset bdps
  sold = sold[indbdp, ]
  solv = solv[indbdp, ]
  
  xlab = "Trimming level"
  est_nam = c("sparseLTS", "MIP")
  soldd = sold
  solvv = solv
  
  names(soldd) = c("sparseLTS median", "MIP median")
  names(solvv) = c("sparseLTS mad", "MIP mad")
  p3 = plot_sol_se_gg(soldd, se=solvv, 
                      x=bdpt, ylab = expression(widehat(k)[p]), 
                      xlab = expression(k[n]/n),
                      tit = "Number of selected features vs trimming level",
                      showlegend = F)
  p3
  
  p <- ggarrange(p1, p3, 
                 ncol = 2, nrow = 1)
  print(p)
  
  print("Figure S17")
  invisible(readline(prompt="Press [enter] to continue"))
  
  if (savfig==1) {
    ggsave(filename = "numfeat_TRMSPE.eps",
           plot = print(p),
           path = imgPth,
           width = 15,
           height = 8,
           units = "in",
           dpi = "retina",
           device = cairo_ps)
  }
}

setwd(mainwd)
