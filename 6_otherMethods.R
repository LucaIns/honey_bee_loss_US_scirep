
#########################
# Table S8-S9
#########################

# NOTE
# with_out = 0: keep all points
# with_out = 1: remove outliers selected by MIP

for (with_out in 0:1) {
  
  set.seed(1)
  
  base::load(paste0(pthDat, "bee_model.RData"))
  names(Z)
  
  if (with_out==0){
    
    myX = read.csv(paste0(pthDat, "MIPdat.csv"))
    names(myX)
    idx = myX$idx
    indmod = myX$sub[!is.na(myX$sub)]
    indsel = myX$sel[!is.na(myX$sel)]
    indout = myX$out
    
    Z = Z[idx, ]
    Z = Z[indout, ]
  }
  
  y = Z$y
  ycount = Z$ycount
  offMax = Z$offMax
  
  Xremove = c("ycount", "offMax", "y", "state")
  X = Z[,!(names(Z) %in% Xremove)]
  X = model.matrix(~ ., data=X, 
                   contrasts.arg = lapply(X[,sapply(X, is.factor)], 
                                          contrasts, contrasts=FALSE))
  colnames(X)
  X = X[, 2:ncol(X)]
  # remove dummy reference categories
  if (length(years_select)==5) {
    X = X[, -c(5, 6, 18)]
  } else if (length(years_select)==7) {
    X = X[, -c(7,8,20)]
  }
 
  summary(lm(y~X))
  
  # design matrix with intercept
  Xint = as.matrix(cbind.data.frame(intercept = rep(1,length(y)), X))
  
  par(mfrow=c(3,2))
  
  ##########
  # OLS
  ##########
  lmfit = lm(y~X, x=T)
  summary(lmfit)
  plot(lmfit)
  tmp_coeffs = coefficients(lmfit)
  tmp_coeffs[is.na(tmp_coeffs)] = 0
  resLM = y - lmfit$x %*% tmp_coeffs
  plot(1:length(y), resLM)
  plot(y, lmfit$x %*% tmp_coeffs)
  abline(0, 1, col="red")
  
  betaest = data.frame("OLS"=tmp_coeffs)
  
  ##########
  # glmnet
  ##########
  if(!require(glmnet)){install.packages("glmnet", dep=T); library(glmnet)}
  alphENET = 0.8
  famGLM = "gaussian"
  par(mfrow=c(2,2))
  fit = glmnet(X, y, alpha = alphENET, standardize = T, intercept=T,
               standardize.response = T, family = famGLM) 
  plot(fit)
  fit = cv.glmnet(X, y, alpha = alphENET, standardize = T, intercept=T, 
                  standardize.response = T, family = famGLM, nfolds=10)
  plot(fit)
  tmp_coeffs <- as.matrix(coef(fit, s = "lambda.min"))
  tmp_coeffs
  resGLM = y - Xint %*% tmp_coeffs
  plot(1:length(y), resGLM)
  plot(y, Xint %*% tmp_coeffs)
  abline(0, 1, col="red")
  
  betaest = cbind.data.frame(betaest, "glmnet"=as.numeric(tmp_coeffs))
  
  ##########
  # SCAD
  ##########
  gamP = 3.7
  alpP = 0.8
  if(!require(ncvreg)){install.packages("ncvreg", dep=T); library(ncvreg)}
  fitSCAD <- ncvreg(X, y, penalty="SCAD",gamma=gamP, alpha=alpP, nlambda=100)
  plot(fitSCAD)
  fitSCAD <- cv.ncvreg(X, y, penalty="SCAD",gamma=gamP, alpha=alpP, nlambda=100)
  plot(fitSCAD)
  tmp_coeffs = coefficients(fit)
  tmp_coeffs
  resSCAD = y - Xint %*% tmp_coeffs
  plot(1:length(y), resSCAD)
  plot(y, Xint %*% tmp_coeffs)
  abline(0, 1, col="red")
  
  betaest = cbind.data.frame(betaest, "scad"=as.numeric(tmp_coeffs))
  
  ##########
  # sparse-LTS
  ##########
  par(mfrow=c(2,1))
  sol_sparseLTS = robustHD::sparseLTS(X, y, mode = "lambda", alpha = 1-bdpt[bdpi], 
                                      normalize = T, intercept=T, nsamp=500, model=F, 
                                      crit = "PE", splits = foldControl(10, 10), 
                                      cost = rtRMSPE)
  tmp_coeffs = coefficients(sol_sparseLTS)
  tmp_coeffs
  indwt = as.logical(sol_sparseLTS$wt == 0)
  plot(sol_sparseLTS$residuals)
  points((1:length(y))[indwt], sol_sparseLTS$residuals[indwt], col = "red")
  plot(y, sol_sparseLTS$fitted.values)
  points(y[indwt], sol_sparseLTS$fitted.values[indwt], col = "red")
  
  betaest = cbind.data.frame(betaest, "sparseLTS"=as.numeric(tmp_coeffs))
  
  ##########
  # glmnet-count
  ##########
  alphENET = 0.8
  Xremove = c("num_max_colonies")
  y = ycount
  fit = glmnet(X, ycount, alpha = alphENET, standardize = T, offset = log(offMax), 
               intercept=T, standardize.response = T, family = "poisson") 
  plot(fit)
  fit = cv.glmnet(X, ycount, alpha = alphENET, standardize = T, offset = log(offMax), 
                  intercept=T, standardize.response = T, family = "poisson", nfolds=10) # 
  plot(fit)
  tmp_coeffs <- as.matrix(coef(fit, s = "lambda.min"))
  tmp_coeffs
  
  betaest = cbind.data.frame(betaest, "glmnet_count"=as.numeric(tmp_coeffs))
  
  ##########
  # snet-count
  ##########
  if(!require(mpath)){install.packages("mpath", dep=T); library(mpath)}
  Zcount = cbind.data.frame(ycount, X)
  solNB = cv.glmregNB(ycount ~ ., offset = log(offMax), data= Zcount, 
                      alpha = alphENET, penalty = "snet", standardize=T, 
                      n.cores = 10, parallel = T)
  plot(solNB)
  plot(log(solNB$lambda), solNB$cv, type='l')
  tmp_coeffs = coefficients(solNB)
  
  betaest = cbind.data.frame(betaest, "NB_count"=as.numeric(tmp_coeffs))
  
  ########################
  # aggregate all results
  ########################

  betaest
  solBsign = as.matrix(betaest)
  colnames(solBsign) = names(betaest)
  rownames(solBsign) = c("Intercept", colnames(X))
  indN = solBsign < 0
  indP = solBsign > 0
  solBsign[solBsign == 0] = ""
  solBsign[indN] = "-"
  solBsign[indP] = "+"
  solBsign
  if(!require(xtable)){install.packages("xtable", dep=T); library(xtable)}
  if(!require(stringr)){install.packages("stringr", dep=T); library(stringr)}
  rnam = rownames(solBsign) 
  rnam  = str_replace_all(rnam, "_", " ")
  rnam  = str_replace_all(rnam, ":", " : ")
  rnam  = str_replace_all(rnam, ".tm", "Tm")
  rnam  = str_replace_all(rnam, ".pr", "Pr")
  rnam  = str_replace_all(rnam, ".ge", "Ge")
  rnam  = str_replace_all(rnam, "kur", "Kur")
  rnam  = str_replace_all(rnam, "green.urban", "greenUrban")
  rnam  = str_replace_all(rnam, "region", "")
  rnam = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
              rnam, perl = TRUE)
  rnam  = str_replace_all(rnam, " ", "")
  rownames(solBsign) = rnam
  
  if (with_out==0){
    solBsign_no_out = solBsign
  } else if (with_out==1){
    solBsign_with_out = solBsign
  }
}

print(solBsign_with_out)
print(xtable(solBsign_with_out, type = "latex", 
             display=rep("s",ncol(betaest)+1)), 
      math.style.exponents = TRUE)
print("Table S8")
invisible(readline(prompt="Press [enter] to continue"))

print(solBsign_no_out)
print(xtable(solBsign_no_out, type = "latex", 
             display=rep("s",ncol(betaest)+1)), 
      math.style.exponents = TRUE)
print("Table S9")
invisible(readline(prompt="Press [enter] to continue"))

#########################
# random forest
#########################
# same seed as R 3.5.1 (used in Julia)
RNGkind(sample.kind = "Rounding")
set.seed(1)
base::load(paste0(pthDat, "beeLMfac.RData"))
names(Zlogfac)
dim(Zlogfac)
yl = Zlogfac$y
Xl = Zlogfac[, c(3:ncol(Zlogfac))]
dim(Xl)
names(Xl)

#########################
# FIG S21
#########################
if(!require(ranger)){install.packages("ranger", dep=T); library(ranger)}
set.seed(1)
table.grid.small <- expand.grid(trees = seq(1000, 5000, by=1000), vars = seq(5, 15, 1))
nrep = dim(table.grid.small)[1]
for (i in 1:nrep) {
  test.reclass <- ranger(y = yl, x = Xl, 
                         num.trees = table.grid.small[i,1], mtry=table.grid.small[i,2], 
                         importance="permutation")
}
par(mfrow=c(1,1))
par(mar=c(4.1, 10.8, 1.1, 1))
p = barplot(sort(test.reclass$variable.importance),ylab="",xlab="make",
            las=1, horiz = T, cex.names = 1.2,
            names.arg=names(sort(test.reclass$variable.importance)))
if (savfig==1) {
  cairo_ps(file = paste0(imgPth, "/randomforest.eps"),  width = 16, height = 10)
  par(mar=c(4.1, 10.8, 1.1, 1))
  barplot(sort(test.reclass$variable.importance),ylab="",xlab="make",
          las=1, horiz = T, cex.names = 1.2,
          names.arg=names(sort(test.reclass$variable.importance)))
  dev.off()
}
print("Figure S21")
invisible(readline(prompt="Press [enter] to continue"))
