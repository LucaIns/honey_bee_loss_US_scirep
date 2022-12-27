
#######################
# analyze MIP solution further 
#######################

myX = read.csv(paste0(pthDat, "MIPdat.csv"))
names(myX)

idx = myX$idx
indmod = myX$sub[!is.na(myX$sub)]
indsel = myX$sel[!is.na(myX$sel)]
indout = myX$out

base::load(paste0(pthDat, "bee_model.RData"))
names(Z)
ny = Z$y[idx]
nX = Z[idx, 5:ncol(Z)] # region
nXstate = Z[idx, c(4:5,7:ncol(Z))] # state
names(nX)
names(nXstate)
str(nX)

nX = model.matrix(~ ., data=nX, 
                  contrasts.arg = lapply(nX[,sapply(nX, is.factor)], 
                                         contrasts, contrasts=FALSE))
colnames(nX)
nX = nX[, 2:ncol(nX)]

dim(nX)
length(indmod)
nX=nX[, indmod]
colnames(nX)
nX = nX[, indsel]
colnames(nX)

names(nXstate)
if (length(years_select)==5) {
  nXstate = cbind.data.frame(nXstate[,1:3], nX[, -(1:15)])
} else if (length(years_select)==7) {
  nXstate = cbind.data.frame(nXstate[,1:3], nX[, -(1:17)])
}
names(nXstate)

nZ = cbind.data.frame(ny, nX)
names(nZ)

par(mfrow=c(2,2))

# linear model on the selected features (with outliers)
solz = lm(ny~., data = nZ)
summary(solz)
plot(solz)

# linear model on the selected features (without outliers)
# this is the exact solution found by MIP
nZclean = nZ[indout, ]
solzc = lm(ny~., data = nZclean)
summary(solzc)
plot(solzc)

#####################
# model perturbations
#####################
# sign of the "green-area index" flips using State controls as opposed to regions
nZstate = cbind.data.frame(ny, nXstate)
nZstatecl = nZstate[indout, ]

#########################
# Table S10
#########################
solzc = lm(ny~., data = nZstatecl)
print(summary(solzc))
xtable(summary(solzc))
plot(solzc)

print("Table S10")
invisible(readline(prompt="Press [enter] to continue"))

# or also including a squared term it loses significance/flips
if (decompose_green==0) {
  squared.urban=nZclean$green.urban^2  
} else {
  squared.urban=cbind(nZclean$green.urban.pos^2, nZclean$green.urban.neg^2)
}
solzc = lm(ny~.+squared.urban, data = nZclean)
summary(solzc)
solzc = lm(ny~.+squared.urban, data = nZstatecl)
summary(solzc)

#########################
# Table S7
#########################
# the sign of "other pests and parasites" depends also on the strong correlation with "varroa"
# 3D plot
# if(!require(plotly)){install.packages("plotly", dep=T); library(plotly)}
# plot_ly(x=nZ$varroa_mites, z=nZ$ny, y=nZ$other_pests_parasites, 
#         color=indout, type="scatter3d", mode="markers")

# "other pests and parasites" as a single predictor
tlm=lm(ny~other_pests_parasites, data=nZclean)
print(summary(tlm))
xtable(summary(tlm))
plot(tlm)
plot(ny~other_pests_parasites, data=nZ)
points(ny[!indout]~other_pests_parasites[!indout], col="red", data=nZ)

# "varroa mites" as a single predictor
tlm=lm(ny~varroa_mites, data=nZclean)
print(summary(tlm))
xtable(summary(tlm))
plot(tlm)
plot(ny~varroa_mites, data=nZclean)
points(ny[!indout]~varroa_mites[!indout], col="red", data=nZ)

#  both "other pests and parasites" and "varroa mites" as predictors
tlm=lm(ny~other_pests_parasites+varroa_mites, data=nZclean)
print(summary(tlm))
xtable(summary(tlm))
plot(tlm)

# including also "other"
tlm=lm(ny~other_pests_parasites+varroa_mites+other, data=nZclean)
print(summary(tlm))
xtable(summary(tlm))

# note that "green urban" would be non-significant in this mopdel
if (decompose_green==0) {
  tlm=lm(ny~other_pests_parasites+varroa_mites+other+green.urban+other, data=nZclean)
} else {
  tlm=lm(ny~other_pests_parasites+varroa_mites+other+green.urban.pos+green.urban.neg+other, data=nZclean)
}
summary(tlm)

print("Table S7")
invisible(readline(prompt="Press [enter] to continue"))

#######################
# lagged variables
#######################
# create lagged vars
# to be added after re-shuffle
names(nZstate)
Xsort = cbind.data.frame("region"=Z$region[idx], nZstate)
Xsort = Xsort[indout, ]
Xnew = cbind.data.frame(Xsort, matrix(NA, nrow(Xsort), ncol(Xsort)))
names(Xnew) = c(names(Xsort), paste0("lag.", names(Xsort)))
names(Xsort)
ss=as.numeric(Xsort$state)
yy=as.numeric(Xsort$year)
pp=as.numeric(Xsort$period)
for (k in unique(ss)){
  for (i in unique(yy)){
    for (j in unique(pp)){
      
      inds = ss==k & yy==i & pp==j
      
      if (sum(inds)>0){
        
        # Xsort[inds,]
        if (j>1){
          indss = ss==k & yy==i & pp==j-1
        } else if (j==1){
          indss = ss==k & yy==i-1 & pp==4
        }
        if (sum(indss)>0){
          Xnew[inds,] = cbind.data.frame(Xsort[inds,], Xsort[indss,1:ncol(Xsort)])
          # names(Xnew)
          # head(Xnew)
        }
      }
    }
  }
}

head(Xnew)
names(Xnew)
# remove unnecessary lagged variables
if (length(years_select)==5) {
  Xnew=Xnew[,-c(17:21, 32)]
} else if (length(years_select)==7 && decompose_green==0) {
  Xnew=Xnew[,-c(16:20, 30)]
} else if (length(years_select)==7 && decompose_green==1) {
  Xnew=Xnew[,-c(17:21, 31:32)]
}
# Xnew=Xnew[indout,]
dim(Xnew)
Xnew=Xnew[complete.cases(Xnew),]
dim(Xnew)
names(Xnew)

lagy=Xnew$ny
lagX=Xnew[,c(1, 4:ncol(Xnew))]

par(mfrow=c(1,1))
# check correlations
if(!require(corrplot)){install.packages("corrplot", dep=T); library(corrplot)}
corrplot(cor(lagX[, sapply(lagX, is.numeric)], use="pairwise.complete.obs"),
         type="upper", diag = T, # method = "number",
         tl.cex = 0.8)

par(mfrow=c(2,1))
plot(lagX$varroa_mites, lagX$pesticides)
plot(lagX$lag.varroa_mites, lagX$lag.pesticides)

#########################
# Table S11
#########################
par(mfrow=c(2,2))
names(lagX)
# Zlag = cbind.data.frame(lagy, lagX[,c(1:14)]) #
Zlag = cbind.data.frame(lagy, lagX) #
Zlag <- within(Zlag, region <- relevel(region, ref = "Central"))
if (length(years_select)==5) {
  Zlag <- within(Zlag, year <- relevel(year, ref = "2019"))
} else if (length(years_select)==7) {
  Zlag <- within(Zlag, year <- relevel(year, ref = "2021"))
}
Zlag <- within(Zlag, period <- relevel(period, ref = "4"))
tlm=lm(lagy~., Zlag)
print(summary(tlm))
plot(tlm)
xtable(summary(tlm))

print("Table S11")
invisible(readline(prompt="Press [enter] to continue"))

# glmnet would select the lagged terms which are significant in the previous model
lagXdum = model.matrix(~ ., data=lagX, 
                       contrasts.arg = lapply(lagX[,sapply(lagX, is.factor)], 
                                              contrasts, contrasts=FALSE))
colnames(lagXdum)
lagXdum = lagXdum[, 2:ncol(lagXdum)]

if(!require(glmnet)){install.packages("glmnet", dep=T); library(glmnet)}
par(mfrow=c(2,1))
alphENET = 0.8
set.seed(1)
famGLM = "gaussian"
fit = glmnet(as.matrix(lagXdum), unlist(lagy), alpha = alphENET, standardize = T, intercept=T, # dfmax = length(y)/2, #penalty.factor = penfac, 
             standardize.response = T, family = famGLM) 
plot(fit)
fit = cv.glmnet(as.matrix(lagXdum), unlist(lagy), alpha = alphENET, standardize = T, # offset = log(dat$num_max_colonies), 
                intercept=T, # dfmax = length(y)/10, #penalty.factor = penfac, 
                standardize.response = T, family = famGLM, nfolds=10) # 
plot(fit)
tmp_coeffs <- as.matrix(coef(fit, s = "lambda.1se"))
tmp_coeffs

par(mfrow=c(3,2))
Xtmp = lagXdum[, tmp_coeffs[2:length(tmp_coeffs)]!=0]
dim(Xtmp)
colnames(Xtmp)
Ztmp = cbind.data.frame(lagy, Xtmp)
lmfit = lm(lagy~., x=T, data=Ztmp)
summary(lmfit)
plot(lmfit)
tmp_coeffs = coefficients(lmfit)
tmp_coeffs[is.na(tmp_coeffs)] = 0
resLM = Ztmp$lagy - lmfit$x %*% tmp_coeffs
plot(1:length(Ztmp$lagy), resLM)
plot(Ztmp$lagy, lmfit$x %*% tmp_coeffs)
abline(0, 1, col="red")

#########################
# interaction terms
#########################

# interaction matrix
names(nZclean)
dim(nZclean)
if (length(years_select)==5) {
  Xtmp = nZclean[, 17:ncol(nZclean)]
} else if (length(years_select)==7) {
  Xtmp = nZclean[, 19:ncol(nZclean)]
}
names(Xtmp)
Xtmp = model.matrix(~ .^2, data=Xtmp, 
                    contrasts.arg = lapply(Xtmp[,sapply(Xtmp, is.factor)], contrasts,
                                           contrasts=FALSE))
dim(Xtmp)
colnames(Xtmp)
Xtmp = Xtmp[, 2:ncol(Xtmp)]

names(nZclean)
if (length(years_select)==5) {
  Xtmp = as.matrix(cbind(nZclean[, 2:16], Xtmp))
} else if (length(years_select)==7) {
  Xtmp = as.matrix(cbind(nZclean[, 2:18], Xtmp))
}
dim(Xtmp)
colnames(Xtmp)

ytmp = nZclean$ny
Ztmp = cbind.data.frame(ytmp, Xtmp)

par(mfrow=c(1,1))
# check correlations and remove strong ones
names(Ztmp)
if (length(years_select)==5) {
  indinter = 28:ncol(Ztmp)
} else if (length(years_select)==7 && decompose_green==0) {
  indinter = 29:ncol(Ztmp)
} else if (length(years_select)==7 && decompose_green==1) {
  indinter = 30:ncol(Ztmp)
}
corrplot(cor(Ztmp[, indinter], use="pairwise.complete.obs"),
         type="upper", diag = T, # method = "number",
         tl.cex = 0.8)

if(!require(caret)){install.packages("caret", dep=T); library(caret)}
df1 = Ztmp[, indinter]
df2 = cor(df1)
hc = findCorrelation(df2, cutoff=0.7) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = df1[,-c(hc)]
summary(reduced_Data)
colnames(Ztmp)[!colnames(Ztmp) %in% colnames(reduced_Data)]
corrplot(cor(reduced_Data, use="pairwise.complete.obs"), 
         type="upper", diag = T,
         tl.cex = 0.8)

dim(Ztmp)
Ztmp = cbind.data.frame(Ztmp[,1:(min(indinter)-1)], Ztmp[!(!colnames(Ztmp) %in% colnames(reduced_Data))])
dim(Ztmp)
names(Ztmp)

ytmp=Ztmp$ytmp
Xtmp= as.matrix(Ztmp[,2:ncol(Ztmp)])

#########################
# Table S12
#########################
penalty.factor = rep(1, ncol(Xtmp))
colnames(Xtmp)
if (length(years_select)==5) {
  penalty.factor[c(1:26)] = 0
} else if (length(years_select)==7 && decompose_green==0) {
  penalty.factor[c(1:27)] = 0
} else if (length(years_select)==7 && decompose_green==1) {
  penalty.factor[c(1:28)] = 0
} 
alphENET = 0.8
famGLM = "gaussian"
set.seed(2021)
par(mfrow=c(2,2))
fit = glmnet(Xtmp, ytmp, alpha = alphENET, standardize = T,
             intercept=T, penalty.factor = penalty.factor, 
             standardize.response = T, family = famGLM) 
plot(fit)
fit = cv.glmnet(Xtmp, ytmp, alpha = alphENET, standardize = T, 
                intercept=T, penalty.factor = penalty.factor, 
                standardize.response = T, family = famGLM) 
plot(fit)
tmp_coeffs <- as.matrix(coef(fit, s = "lambda.min"))
cbind.data.frame("names"=row.names(tmp_coeffs)[tmp_coeffs!=0],
                 "Estimate"=tmp_coeffs[tmp_coeffs!=0])
resGLM = ytmp - cbind(rep(1, length(ytmp)), Xtmp) %*% tmp_coeffs
plot(1:length(ytmp), resGLM)
plot(ytmp, cbind(rep(1, length(ytmp)), Xtmp) %*% tmp_coeffs)
abline(0, 1, col="red")

# re-fit OLS on top of enet
Ztmp = cbind.data.frame(ytmp, Xtmp[, tmp_coeffs[2:length(tmp_coeffs)] != 0])
lmfit = lm(ytmp~., x=T, data=Ztmp)
print(summary(lmfit))
xtable(summary(lmfit))

print("Table S12")
invisible(readline(prompt="Press [enter] to continue"))

par(mfrow=c(3,2))
plot(lmfit)
tmp_coeffs = coefficients(lmfit)
tmp_coeffs[is.na(tmp_coeffs)] = 0
resLM = Ztmp$ytmp - lmfit$x %*% tmp_coeffs
plot(1:length(Ztmp$ytmp), resLM)
plot(Ztmp$ytmp, lmfit$x %*% tmp_coeffs)
abline(0, 1, col="red")


##########################
# testing nested models
##########################

if(!require(lmtest)){install.packages("lmtest", dep=T); library(lmtest)}

indtst = grepl("norm", colnames(X)) |  grepl("ent.", colnames(X), fixed=T) |  # grepl("sd", colnames(X)) |  
  grepl("skew", colnames(X)) |  grepl("kurt", colnames(X)) |  grepl("alpha", colnames(X))
colnames(X)[indtst]
Xred = X[, !indtst]
colnames(Xred)

names(X)
# remove "states"
X = X[,-1]
Xred = Xred[,-1]

colnames(X)
fmat = cbind.data.frame(y, X)
fullmod = lm(y~., data = fmat)
summary(fullmod)

rmat = cbind.data.frame(y, Xred)
redmod = lm(y~., data=rmat)
summary(redmod)

#########################
# LRT result discussed in the main text
#########################
print("LRT result discussed in the main text")
print(lrtest(fullmod, redmod))
# xtable(lrtest(fullmod, redmod))

#########################
# Table S5: F-tests
#########################
# 1. Gaussian (with MIP outliers)
print("\n\n1. Gaussian (with MIP outliers)")
print(anova(fullmod, redmod, test='F'))
xtable(anova(fullmod, redmod, test='F'))
length(fullmod$coefficients)
length(redmod$coefficients)
nrow(fmat)

# 2. Gaussian (without MIP outliers)
fmatno = cbind.data.frame(y = y[idx][indout], X[idx, ][indout, ])
fullmodno = lm(y~., data = fmatno)
summary(fullmodno)
length(fullmodno$coefficients)
nrow(fmatno)
rmatno = cbind.data.frame(y = y[idx][indout], Xred[idx, ][indout, ])
redmodno = lm(y~., data=rmatno)
summary(redmodno)
length(redmodno$coefficients)

# ANOVA and LRT are significant
print("\n\n2. Gaussian (without MIP outliers)")
print(anova(fullmodno, redmodno, test='F'))
xtable(anova(fullmodno, redmodno, test='F'))
lrtest(fullmodno, redmodno)

# 3. Quasi-Poisson (with MIP outliers)
fmatco = cbind.data.frame(ycount = ycount, X)
fullmodco = glm(ycount ~ . + offset(log(offMax)), quasipoisson(link="log"), data=fmatco)
summary(fullmodco)

rmatco = cbind.data.frame(ycount = ycount, Xred)
redmodco = glm(ycount ~ . + offset(log(offMax)), quasipoisson(link="log"), data=rmatco)
summary(redmodco)

# ANOVA F-test
print("\n\n3. Quasi-Poisson (with MIP outliers)")
print(anova(fullmodco, redmodco, test='F'))
xtable(anova(fullmodco, redmodco, test='F'))

# 4. Quasi-Poisson (without MIP outliers)
fmatcono = cbind.data.frame(ycount = ycount[idx][indout], X[idx, ][indout, ])
fullmodcono = glm(ycount ~ . + offset(log(offMax[idx][indout])), quasipoisson(link="log"), data=fmatcono)
summary(fullmodcono)

rmatcono = cbind.data.frame(ycount = ycount[idx][indout], Xred[idx, ][indout, ])
redmodcono = glm(ycount ~ . + offset(log(offMax[idx][indout])), quasipoisson(link="log"), data=rmatcono)
summary(redmodcono)

cat("\n\n4. Quasi-Poisson (without MIP outliers)")
print(anova(fullmodcono, redmodcono, test='F'))
xtable(anova(fullmodcono, redmodcono, test='F'))

print("Table S5")
invisible(readline(prompt="Press [enter] to continue"))

