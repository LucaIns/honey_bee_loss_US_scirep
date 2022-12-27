
if(!require(quantable)){install.packages("quantable", dep=T); library(quantable)}
if(!require(qqplotr)){install.packages("qqplotr", dep=T); library(qqplotr)}
if(!require(rrcov)){install.packages("rrcov", dep=T); library(rrcov)}

pointsize = 1.2
lwid=0.7
lincol="black"
titsiz=12
ksiz=1
legsiz=8
legpos=c(0.85, 0.25)
legtex=8

indtmp=indout
indtmp[indtmp == T] = "Outliers"
indtmp[indtmp == F] = "Non-outlying"
indtmp = factor(indtmp, levels = c("Outliers","Non-outlying"))

hii <- lm.influence(lm(yy~XX), do.coef = FALSE)$hat
tmpvec = rep(NA, length(resi))
tmpvec[!indout] = resistd[!indout] / sqrt(1-hii)
resistdext = tmpvec
plot(fits, resistdext)
points(fits[indout], resistdext[indout], col="red")
tmp=cbind.data.frame(yl, Xl, "Units"=indtmp, resistd, resi, fits, resistdext)

ccc=qnorm(0.975)
p1<-ggplot(tmp, aes(x=1:length(yl), y=resistd, color=Units)) +
  geom_point(size=pointsize, alpha=1)  +
  geom_hline(yintercept=c(ccc, -ccc), size=lwid, linetype='dashed', color='red') +
  geom_hline(yintercept=c(0), size=lwid, linetype='dashed', color=lincol) +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    plot.title = element_text(size=titsiz, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=titsiz-ksiz, face="bold"),
    axis.title.y = element_text(size=titsiz-ksiz, face="bold"),
    axis.text.x = element_text(size=titsiz-ksiz,face="bold"),
    axis.text.y = element_text(size=titsiz-ksiz,face="bold"),
    legend.title = element_text(size=legsiz),
    legend.position = legpos,
    legend.text = element_text(size=legtex),
    legend.background = element_rect(fill="white",
                                     size=0.5, linetype="solid",
                                     colour ="black")) +
  ylab("Scaled residuals") +
  xlab("Index") +
  ggtitle("Scaled Residuals")
p1


p2<-ggplot(tmp, aes(x=fits, y=resi, color=Units)) +
  geom_point(size=pointsize, alpha=1)  +
  geom_hline(yintercept=c(0), size=lwid, linetype='dashed', color=lincol) +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    plot.title = element_text(size=titsiz, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=titsiz-ksiz, face="bold"),
    axis.title.y = element_text(size=titsiz-ksiz, face="bold"),
    axis.text.x = element_text(size=titsiz-ksiz,face="bold"),
    axis.text.y = element_text(size=titsiz-ksiz,face="bold"),
    legend.title = element_text(size=legsiz),
    legend.position = legpos,
    legend.text = element_text(size=legtex),
    legend.background = element_rect(fill="white",
                                     size=0.5, linetype="solid",
                                     colour ="black")) +
  ylab("Residuals") +
  xlab("Fitted values") +
  ggtitle("Residuals vs Fitted") 
p2

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(4)[3]
p3 <- ggplot(data=tmp[!indout,], mapping = aes(sample = resistdext[!indout])) + 
  geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), fill="grey", alpha = 1) +
  stat_qq_line() +
  stat_qq_point(color=cols, size=pointsize) + 
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    plot.title = element_text(size=titsiz, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=titsiz-ksiz, face="bold"),
    axis.title.y = element_text(size=titsiz-ksiz, face="bold"),
    axis.text.x = element_text(size=titsiz-ksiz,face="bold"),
    axis.text.y = element_text(size=titsiz-ksiz,face="bold"),
    legend.title = element_text(size=legsiz),
    legend.position = legpos,
    legend.text = element_text(size=legtex),
    legend.background = element_rect(fill="white",
                                     size=0.5, linetype="solid",
                                     colour ="black")) +
  ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles") +
  ggtitle("Normal Q-Q for non-outlying cases") 
p3

colnames(XXX)
set.seed(seedJl)
if (length(years_select)==5){
  tmpZ = cbind.data.frame(yyy, XXX[,16:26])
} else if (length(years_select)==7){
  tmpZ = cbind.data.frame(yyy, XXX[,18:26])
}
tmpZ = robustscale(tmpZ, scale = T, preserveScale=F)
tmpZ = tmpZ$data
frcont=bdpt[bdpi]
mcd <- rrcov::CovMcd(tmpZ, alpha = 1-frcont, nsamp=50000)
# get mcd estimate of location
mean_mcd <- mcd@raw.center
# get mcd estimate scatter
cov_mcd <- mcd@raw.cov
# get inverse of scatter
cov_mcd_inv <- solve(cov_mcd)
# compute the robust distance
robust_dist <- apply(tmpZ, 1, function(x){
  x <- (x - mean_mcd)
  dist <- sqrt((t(x)  %*% cov_mcd_inv %*% x))
  return(dist)
})
# set cutoff using chi square distribution
threshold <- sqrt(qchisq(p = 0.975, df = ncol(tmpZ)))
# find outliers
outliers <-  which(robust_dist >= threshold)
# sum(outliers %in% idx)
levv = robust_dist >= threshold
typpp = rep(0, length(levv))
typpp[(indout == 0) & (levv ==1)] = "NO-L"
typpp[(indout == 0) & (levv ==0)] = "NO-NL"
typpp[(indout == 1) & (levv ==1)] = "O-L"
typpp[(indout == 1) & (levv ==0)] = "O-NL"
graph = data.frame(Y=resistd,
                   X = robust_dist,
                   type = indout,
                   Units = typpp,
                   lev = levv)
idxx = which(graph$type==1)
graph$type[idxx] = "Non-outlying"
graph$type[-idxx] = "Outlying"
p4= ggplot(graph, aes(x=X, y=Y, shape=Units, color=Units)) +
  theme_bw() +
  geom_point(size=pointsize) +
  geom_hline(yintercept=0,linetype="dashed", color = lincol, size=lwid) +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    plot.title = element_text(size=titsiz, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=titsiz-ksiz, face="bold"),
    axis.title.y = element_text(size=titsiz-ksiz, face="bold"),
    axis.text.x = element_text(size=titsiz-ksiz,face="bold"),
    axis.text.y = element_text(size=titsiz-ksiz,face="bold"),
    legend.title = element_text(size=legsiz),
    legend.position = legpos+c(-0.15,0.6),
    legend.spacing.x = unit(0.05, 'cm'),
    legend.text = element_text(size=legtex-1),
    legend.direction = "horizontal",
    legend.background = element_rect(fill="white",
                                     size=0.5, linetype="solid",
                                     colour ="black")) +
  xlab("Robust distance in X (MCD)") + 
  ylab("Scaled residuals") +
  ggtitle("Residuals vs Outlyingness in X") +
  guides(shape = guide_legend(title.position = "top"))
p4

p5<-ggplot(tmp, aes(x=yl, y=fits, color=Units)) +
  geom_point(size=pointsize, alpha=1)  +
  geom_abline(intercept = 0, slope=1, size=lwid, linetype='dashed', color=lincol) +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    plot.title = element_text(size=titsiz, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=titsiz-ksiz, face="bold"),
    axis.title.y = element_text(size=titsiz-ksiz, face="bold"),
    axis.text.x = element_text(size=titsiz-ksiz,face="bold"),
    axis.text.y = element_text(size=titsiz-ksiz,face="bold"),
    legend.title = element_text(size=legsiz),
    legend.position = 1-legpos,
    legend.text = element_text(size=legtex),
    legend.background = element_rect(fill="white",
                                     size=0.5, linetype="solid",
                                     colour ="black")) +
  ylab("Fitted values") +
  xlab("Response value") +
  ggtitle("Fitted vs Response")
p5

p6<-ggplot(tmp, aes(x=fits, y=sqrt(abs(resistd)), color=Units)) +
  geom_point(size=pointsize, alpha=1)  +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    plot.title = element_text(size=titsiz, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=titsiz-ksiz, face="bold"),
    axis.title.y = element_text(size=titsiz-ksiz, face="bold"),
    axis.text.x = element_text(size=titsiz-ksiz,face="bold"),
    axis.text.y = element_text(size=titsiz-ksiz,face="bold"),
    legend.title = element_text(size=legsiz),
    legend.position = legpos,
    legend.text = element_text(size=legtex),
    legend.background = element_rect(fill="white",
                                     size=0.5, linetype="solid",
                                     colour ="black")) +
  ylab(expression(sqrt(abs("Scaled residuals")))) +
  xlab("Fitted values") +
  ggtitle("Scale-Location")
p6

require("ggpubr")
figure <- ggarrange(p1, p6, p2, p5, p4, p3,
                    labels = c("a", "b", "c", "d", "e", "f"),
                    hjust = 0.0, ncol = 2, nrow = 3)
figure

