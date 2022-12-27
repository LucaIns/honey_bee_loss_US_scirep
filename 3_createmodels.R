if(!require(ggplot2)){install.packages("ggplot2", dep=T); library(ggplot2)}

#################
# pre-processing 
#################

# normalized losses
dat$lost_colonies = dat$num_lost_colonies / dat$num_max_colonies
# tranform percentages in proportions
indperc = names(dat) %in% c("varroa_mites", "other_pests_parasites", "diseases", "pesticides", "other")
dat[, indperc] = dat[, indperc] / 100
  
# create logit transformation for proportions
indlogit = indperc | (names(dat)=="lost_colonies")
dattmp = dat[, indlogit]
dattmp = log(dattmp / (1-dattmp))
dat[, indlogit] = dattmp
  
# remove uninformative predictors for colonies loss
names(dat)
Xremove = c("num_colonies_init", "unknown", "num_added_colonies", "num_renovated_colonies", "alpha.geom.prec")
dat = dat[, !names(dat)%in%Xremove]
names(dat)
  
# reduce skews
if (decompose_green==0) {
  indtrans = grepl("alpha", names(dat)) |  grepl("kurt", names(dat)) |  grepl("green.urban", names(dat))
} else {
  indtrans = grepl("alpha", names(dat)) |  grepl("kurt", names(dat)) |  
    grepl("green.urban.pos", names(dat)) | grepl("green.urban.neg", names(dat))
}
dat[,indtrans] = log(dat[,indtrans])
  
#########################
# FIG S7
#########################
Xremove = c("num_max_colonies", "num_lost_colonies", "lost_colonies")
dathist = dat[,!(names(dat) %in% Xremove)]
dathist = cbind.data.frame(lost_colonies = dat$lost_colonies, dathist)
dathist = dathist[,sapply(dathist, is.numeric)]
names(dathist) = paste0(names(dathist), 
                        "\n(n:",  nrow(dathist)-colSums(sapply(dathist, is.na)), 
                        "; NA:", colSums(sapply(dathist, is.na)), ")")
new_df <- reshape2::melt(dathist)
head(new_df)
p = ggplot(new_df, aes(x = value)) +
  geom_histogram(aes(y = ..density..), 
                 bins=50) +
  geom_density(lwd = 0.2, colour = "red",
               fill = "red", alpha = 0.1) +
  facet_wrap(.~variable,
             scales="free",
             nrow=6,ncol=5) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 8))
print(p)
print("Figure S7")
invisible(readline(prompt="Press [enter] to continue"))
if (savfig==1) {
  ggsave(filename = "distributionPredictors.eps",
         plot = print(p),
         path = imgPth,
         width = 8,
         height = 9,
         dpi = "retina",
         device = cairo_ps)
}

# remove missing data and Inf
dim(dat)
names(dat)
indnum = sapply(dat, is.numeric)
dat <- dat[is.finite(rowSums(dat[, indnum])), ]
dim(dat)
summary(dat)
names(dat)

# offset and response for count models
offMax = dat$num_max_colonies
ycount=dat$num_lost_colonies

# data
y = dat$lost_colonies
Xremove = c("num_max_colonies", "num_lost_colonies", "lost_colonies")
dat = dat[,!(names(dat) %in% Xremove)]
names(dat)

#########################
# FIG S8
#########################
# check correlations and remove strong ones
if(!require(corrplot)){install.packages("corrplot", dep=T); library(corrplot)}
indnum = sapply(dat, is.numeric)
corrplot(cor(dat[, indnum], use="pairwise.complete.obs"),
         type="upper", diag = T,
         tl.cex = 0.8)
if(!require(caret)){install.packages("caret", dep=T); library(caret)}
df1 = dat[, indnum]
df2 = cor(df1)
if (length(years_select)==5) {
  hc = findCorrelation(df2, cutoff=0.9)
} else {
  hc = findCorrelation(df2, cutoff=0.89)
}
hc = sort(hc)
names(df1[,(hc)])
reduced_Data = df1[,-c(hc)]
summary(reduced_Data)
colnames(dat)[!colnames(dat) %in% colnames(reduced_Data)]
p = corrplot(cor(cbind.data.frame(lost_colonies=y, reduced_Data), use="pairwise.complete.obs"), 
             type="upper", diag = T,
             tl.cex = 0.8)
print(p)
print("Figure S8")
invisible(readline(prompt="Press [enter] to continue"))
if (savfig==1) {
  cairo_ps(file = paste0(imgPth, "/corr_plot.eps"))
  corrplot(cor(cbind.data.frame(lost_colonies=y, reduced_Data), 
               use="pairwise.complete.obs"),
           type="upper", diag = T,
           tl.cex = 0.8)  
  dev.off()
}

dim(dat)
indkeep = !(indnum &  !colnames(dat) %in% colnames(reduced_Data))
dat = dat[, indkeep]
dim(dat)
names(dat)
X = dat

# if (length(years_select)==7) {
#   ind20_21 = as.numeric(as.character(dat$year))==2020 | as.numeric(as.character(dat$year))==2021
#   dat_20_21 = dat[ind20_21, ]
#   names(dat_20_21)
#   summary(dat_20_21)
#   y_20_21 = y[ind20_21]
#   dat_20_21 = cbind.data.frame(y_20_21, dat_20_21)
#   save(dat_20_21, file = paste0(pthDat, "dat_20_21.RData"))
# }

#################
# save data
#################

# full data matrix (LM and Poisson)
Z = cbind.data.frame(ycount, offMax, y, X)
names(Z)
dim(Z)
if (savdat==1){
  save(Z, file = paste0(pthDat, "bee_model.RData"))
}

# design matrix used for LM with factors (including "States")
Zlogfac = cbind.data.frame(y, X)
names(Zlogfac)
if (savdat==1){
  save(Zlogfac, file = paste0(pthDat, "beeLMfac.RData"))
}

# design matrix used for LM (with dummies and no States)
names(X)
Xlog = X
Xremove = c("state")
Xlog = Xlog[,!(names(Xlog) %in% Xremove)]
names(Xlog)
Xlog = model.matrix(~ ., data=Xlog, 
                    contrasts.arg = lapply(Xlog[,sapply(Xlog, is.factor)], 
                                           contrasts, contrasts=FALSE))
colnames(Xlog)
Xlog = Xlog[, 2:ncol(Xlog)]
Zlog = cbind.data.frame(y, Xlog)
if (savdat==1){
  save(Zlog, file = paste0(pthDat, "beeLM.RData"))
}

#########################
# FIG S15: Green area index vs LM response per State
#########################

names(Z)
pointsize = 1.5
titsiz=12
ksiz=0
legsiz=10
legtex=8
if (decompose_green==0) {
  p <- ggplot(Z, aes(x=green.urban, y=y, color=state)) +
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
      legend.position =  "top", 
      legend.text = element_text(size=legtex),
      legend.background = element_rect(fill="white",
                                       size=0.5, linetype="solid",
                                       colour ="black")) +
    ylab("Colony loss") +
    xlab("Green area index")

} else{
  ppos <- ggplot(Z, aes(x=green.urban.pos, y=y, color=state)) +
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
      legend.position =  "top", 
      legend.text = element_text(size=legtex),
      legend.background = element_rect(fill="white",
                                       size=0.5, linetype="solid",
                                       colour ="black")) +
    ylab("Colony loss") +
    xlab("Green area index")
  print(ppos)
  
  pneg <- ggplot(Z, aes(x=green.urban.neg, y=y, color=state)) +
    geom_point(size=pointsize, alpha=1)  +
    theme_bw() +
    theme(
      panel.border = element_rect(colour = "black", fill=NA),
      plot.title = element_text(size=titsiz, face="bold", hjust = 0.5),
      axis.title.x = element_text(size=titsiz-ksiz, face="bold"),
      axis.title.y = element_text(size=titsiz-ksiz, face="bold"),
      axis.text.x = element_text(size=titsiz-ksiz,face="bold"),
      axis.text.y = element_text(size=titsiz-ksiz,face="bold"),
      legend.position =  "none") +
    ylab("Colony loss") +
    xlab("Crop area index")
  print(pneg)
  
  library(gridExtra)
  library(gbm)
  
  get_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  ppos_legend <- get_legend(ppos)
  p <- grid.arrange(arrangeGrob(ppos + theme(legend.position="none"), 
                                pneg + theme(legend.position="none"), 
                                ncol=2,heights=c(10, 1)), 
                    ppos_legend, heights=c(2, 1))
}
print(p)
print("Figure S15")
invisible(readline(prompt="Press [enter] to continue"))
if (savfig==1) {
  ggsave(filename = "greenarea.eps",
         plot = print(p),
         path = imgPth,
         width = 9,
         height = 9,
         units = "in",
         dpi = "retina",
         device = cairo_ps)
}

if (decompose_green==1) {
  # study the effects of crops on the green-area index
  ggplot(dat)+
    geom_point(aes(x=green.urban.neg,y=green.urban.pos,colour=state))+
    geom_smooth(method='lm', aes(x=green.urban.neg,y=green.urban.pos,colour=state),
                se=FALSE, size=0.5) +
    theme_bw() +
    xlab("C-index") +
    ylab("FPR-index") +
    theme(legend.position="bottom")
  cor(dat$green.urban.neg, dat$green.urban.pos)
  
  ggplot(dat)+
    geom_point(aes(x=green.urban.neg,y=green.urban.pos+green.urban.neg,colour=state))+
    geom_smooth(method='lm', aes(x=green.urban.neg,y=green.urban.pos+green.urban.neg,colour=state),
                se=FALSE, size=0.5) +
    theme_bw() +
    xlab("C-index") +
    ylab("Green-area index") +
    theme(legend.position="bottom")
  cor(dat$green.urban.neg, dat$green.urban.pos+dat$green.urban.neg)
  cor.test(dat$green.urban.neg, dat$green.urban.pos+dat$green.urban.neg)
  
  ggplot(dat)+
    geom_point(aes(x=green.urban.neg,y=pesticides,colour=state))+
    theme_bw() +
    xlab("C-index") +
    ylab("pesticides") +
    theme(legend.position="bottom")
  cor(dat$green.urban.neg, dat$pesticides)
  
  print("Green-area index decomposition")
  invisible(readline(prompt="Press [enter] to continue"))
  
}