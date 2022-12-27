####################
# descriptive stats
####################

if(!require(ggplot2)){install.packages("ggplot2", dep=T); library(ggplot2)}
if(!require(ggpubr)){install.packages("ggpubr", dep=T); library(ggpubr)}
if(!require(dplyr)){install.packages("dplyr", dep=T); library(dplyr)}
if(!require(egg)){install.packages("egg", dep=T); library(egg)}

datdes=dat

# remove missing data and Inf
indnum = sapply(datdes, is.numeric)
datdes <- datdes[is.finite(rowSums(datdes[, indnum])), ]
dim(datdes)
summary(datdes)

datdes = cbind.data.frame(datdes, Quarter = datdes$period)
datdes = cbind.data.frame(datdes, Year = datdes$year)
datdes = cbind.data.frame(datdes, Region = datdes$region)

datdes$Region <- recode_factor(datdes$Region, 
                               West = "West",
                               Northwest = "Northwest",
                               Southwest = "Southwest",
                               WestNorthCentral = "West North Central",
                               South = "South",
                               Southeast = "Southeast",
                               Central = "Central", 
                               EastNorthCentral = "East North Central", 
                               Northeast = "Northeast")


#########################
# FIG 2
#########################
fsiz = 10
cutoff = median(datdes$num_lost_colonies/datdes$num_max_colonies)
dodge <- position_dodge(width = 1)

myylab = expression(atop(bold(""),
                         atop(textstyle(bold("Normalized")),
                              atop(textstyle(bold("Colony Loss"))))))


p1<-ggplot(datdes, aes(x=Region, y=num_lost_colonies/num_max_colonies, 
                       color=Quarter, fill=Quarter)) +
  geom_hline(yintercept = cutoff, color = "red", linetype = "twodash") +
  geom_boxplot(lwd=0.1, width=0.75, alpha=1,
               outlier.size = 1.5, outlier.alpha = 1, outlier.shape = 21,
               color="black") +
  ylab(myylab) +
  xlab("Climatic Regions\n") +
  theme_bw() +
  theme(legend.position = c(0.05, 0.64), legend.title = element_text(size=fsiz+2), 
        legend.text = element_text(size=fsiz)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text = element_text(colour = 1, size = 5),
        legend.background = element_rect(linetype = 1, size = 0.25, colour = 1),
        legend.key.height=unit(0.4, "cm"), 
        legend.key.width=unit(0.4, "cm")) +
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title.y = element_text(size = fsiz+2,face="bold", 
                                margin = margin(r = -7))) +
  theme(axis.text.y = element_text(size=fsiz)) +
  theme(plot.margin = unit(c(0, 2, 5, 0), "mm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  if (max(years_select)==2021) {
    scale_y_continuous(limits = c(0, 0.675),
                       labels = c(0, format(seq(0.1, 0.60, length=6), nsmall=1)),
                       breaks = seq(0.00, 0.60, length=7),
                       expand = c(0.001,0.001))    
  } else if (max(years_select)==2019) {
    scale_y_continuous(labels = format(seq(0.00, 0.50, length=6), nsmall=2))
  }
# p1

ylabitalics = expression(atop(bold(""),
                              atop(textstyle(bold("Normalized")),
                                   atop(textstyle(bolditalic("Varroa destructor"))))))
cutoff = median(datdes$varroa_mites/100)
dodge <- position_dodge(width = 1)
p2<-ggplot(datdes, aes(x=Region, y=varroa_mites/100, 
                       color=Quarter, fill=Quarter)) +
  geom_hline(yintercept = cutoff, color = "red", linetype = "twodash") +
  geom_boxplot(lwd=0.1, width=0.75, alpha=1,
               outlier.size = 1.5, outlier.alpha = 1, outlier.shape = 21,
               color="black", 
               show.legend = FALSE) +
  ylab(ylabitalics) +
  xlab("Climatic Regions") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text = element_text(colour = 1, size = 5),
        legend.background = element_rect(linetype = 1, size = 0.25, colour = 1),
        legend.key.height=unit(0.25, "cm"), 
        legend.key.width=unit(0.25, "cm")) +
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title.x = element_text(size = fsiz+2,face="bold"),
        axis.title.y = element_text(size = fsiz+2,face="bold", 
                                    margin = margin(r = -7))) +
  theme(axis.text.x = element_text(size=fsiz),
        axis.text.y = element_text(size=fsiz)) +
  theme(plot.margin=grid::unit(c(1,0,1.5,0), "mm")) +
  scale_y_continuous(limits = c(0, 1),
                     labels = c(0, format(seq(0.2, 0.8, length=4), nsmall=1), 1),
                     breaks = seq(0.00, 1, length=6),
                     expand = c(0,0))    
# p2

require(egg)
p <- egg::ggarrange(p1, p2,
                    labels = c("a", "b"),
                    label.args = list(gp = grid::gpar(font = 2, 
                                                      cex = 1.2)),
                    # hjust=0,
                    # heights = c(0.4/0.65, 0.7),
                    ncol = 1, nrow = 2)
p
detach("package:egg", unload=TRUE)
if (savfig==1) {
  ggsave(filename = "region.eps",
         plot = p,
         path = imgPth,
         width = 3300,
         height = 1300,
         units = "px",
         dpi = "retina",
         device = cairo_ps)
}

print("Figure 2")
invisible(readline(prompt="Press [enter] to continue"))
#########################
# FIG 3
#########################
if(!require(zoo)){install.packages("zoo", dep=T); library(zoo)}

ys = unique(as.numeric(as.character(datdes$year)))
lys = length(ys)

datdes$yq <- as.ts(as.yearqtr(paste0(datdes$Year, "-", datdes$Quarter)))

datdes$reg2 = "Others"
tmp = datdes$Region == "Central"
datdes$reg2[tmp] = "Central"
tmp = datdes$Region == "West North Central"
datdes$reg2[tmp] = "West North Central"
datdes$reg2
datdesSmooth = datdes[datdes$reg2!="Others",]
datdes$reg2 = factor(datdes$reg2,
                     levels = c("Others", "Central", "West North Central"),
                     ordered = TRUE)
datdes$yq = as.numeric(datdes$yq)

xl1 = rep(1:4, lys)
xl1 = paste0("Q", xl1)

spanval = 0.2

set.seed(1)

p1 = ggplot(datdesSmooth, aes(x = yq, y = num_lost_colonies/num_max_colonies, 
                              fill = reg2, color = reg2)) +
  geom_line(aes(group=state, color = reg2, linetype = reg2), 
            size = 1) +
  scale_color_manual(name = "Climatic region", values=alpha(c("#E69F00", "#56B4E9"), 0.5)) +
  scale_linetype_manual(name = "Climatic region", values = c("solid", "solid")) +
  guides(colour = guide_legend(override.aes = list(alpha = c(1, 1)))) +
  ylab(myylab) +
  geom_vline(xintercept = datdes$yq[as.numeric(datdes$yq) %% 1 == 0], color = alpha("grey60", 0.5)) +
  theme_bw() +
  theme(legend.position = c(0.12, 0.75), legend.title = element_text(size=fsiz+2),
        legend.text = element_text(size=fsiz),
        legend.key.width = unit(1,"cm")) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_rect(linetype = 1, size = 0.25, colour = 1)) +
  theme(legend.key.size = unit(0.15, 'lines')) +
  theme(axis.title.x = element_text(size = fsiz+2,face="bold"),
        axis.title.y = element_text(size = fsiz+2,face="bold", 
                                    margin = margin(r = -7))) +
  theme(axis.text.x = element_text(size=fsiz),
        axis.text.y = element_text(size=fsiz)) +
  coord_cartesian(ylim = c(0, 0.65), expand = FALSE, clip = "off") +
  theme(plot.margin = unit(c(0, 2, 5, 0), "mm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(labels = c(0, format(seq(0.20, 0.60, length=3), nsmall=1)))
# print(p1)

ltyp = "solid"
p2 = ggplot(datdesSmooth, aes(x = yq, y = num_lost_colonies/num_max_colonies, 
                              color = reg2)) +
  scale_y_continuous(expand = c(0, 0), labels = c(0, format(seq(0.10, 0.40, length=4), nsmall=1))) +
  geom_smooth(formula = y ~ x,
              method="loess", aes(group = reg2),
              data=subset(datdes,reg2=="West North Central"), show.legend = F, # T,
              span = spanval, size = 1, se=F,
              linetype=ltyp) +
  geom_smooth(formula = y ~ x,
              method="loess", aes(group = reg2, ymin = ifelse(..ymin.. < 0, 0, ..ymin..)),
              data=subset(datdes,reg2=="West North Central"), show.legend = F,
              span = spanval, size = 1, se=T,
              linetype=ltyp, fill = "#009E73", alpha=0.2) +
  geom_smooth(formula = y ~ x,
              method="loess", aes(group = reg2),
              data=subset(datdes, reg2=="Central"), show.legend = F, #T,
              span = spanval, size = 1, se=F,
              linetype=ltyp) +
  geom_smooth(formula = y ~ x,
              method="loess", aes(group = reg2),
              data=subset(datdes, reg2=="Central"), show.legend = F,
              span = spanval, size = 1, se=T, 
              linetype=ltyp, fill = "#E69F00", alpha=0.2) +
  scale_color_manual(name = "Climatic region", values=c("#E69F00", "#56B4E9")) + 
  guides(colour = guide_legend(override.aes = list(alpha = c(1, 1)))) +
  ylab(myylab) +
  xlab("Time") +
  geom_vline(xintercept = datdes$yq[as.numeric(datdes$yq) %% 1 == 0], color = alpha("grey60", 0.5)) +
  theme_bw() +
  theme(legend.position = c(0.12, 0.75), legend.title = element_text(size=fsiz+2),
        legend.text = element_text(size=fsiz)) +  
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_rect(linetype = 1, size = 0.25, colour = 1),
        legend.key.width = unit(1, 'cm')) +
  theme(legend.key.size = unit(0.15, 'lines')) +
  theme(axis.title.x = element_text(size = fsiz+2,face="bold"),
        axis.title.y = element_text(size = fsiz+2,face="bold", 
                                    margin = margin(r = -7))) +
  theme(axis.text.x = element_text(size=fsiz),
        axis.text.y = element_text(size=fsiz)) +
  coord_cartesian(ylim = c(0, 0.4), expand = FALSE, clip = "off") +
  annotate(geom = "text", x = seq(ys[1], ys[lys]+1, length=4*lys+1)[-(4*lys+1)], y = -0.02, 
           label = xl1, size = 3.5) +
  annotate(geom = "text", x = 0.5+seq(ys[1], ys[lys]+1, length=lys+1)[-(lys+1)], y = -0.05, 
           label = ys, size = 3.4) +
  annotate(geom = "text", x = 0.5+median(ys), y = -0.09, label = "Time", size = 4.5,fontface ="bold") +
  theme(plot.margin = unit(c(0, 2, 10.5, 0), "mm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
# print(p2)

require(egg)
p <- egg::ggarrange(p1, p2,
                    labels = c("a", "b"),
                    label.args = list(gp = grid::gpar(font = 2, 
                                                      cex = 1.2)),
                    ncol = 1, nrow = 2)
p
detach("package:egg", unload=TRUE)
if (savfig==1) {
  ggsave(filename = "ts.eps",
         plot = p,
         path = imgPth,
         width = 3300,
         height = 1300,
         units = "px",
         dpi = "retina",
         device = cairo_ps)
}

print("Figure 3")
invisible(readline(prompt="Press [enter] to continue"))

#########################
# FIG 4
#########################
p1<-ggplot(datdes, aes(x=varroa_mites/100, y=num_lost_colonies/num_max_colonies, 
                       color=Quarter, fill=Quarter)) +
  geom_point(size=3, alpha=0.5)  +
  geom_smooth(formula = y ~ x, 
              method = "lm", fullrange = T, size=2, alpha=0.25) +
  ylab("Normalized Colony Loss") +
  xlab(expression(bold("Normalized"~bolditalic("Varroa destructor")))) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.80), legend.title = element_text(size=18),
        legend.text = element_text(size=16)) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text = element_text(colour = 1, size = 5),
        legend.background = element_rect(linetype = 1, size = 0.25, colour = 1)) +
  theme(axis.title.x = element_text(size = 20, face="bold"),
        axis.title.y = element_text(size = 20,face="bold")) +
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.65), expand = FALSE, clip = "off") +
  scale_y_continuous(labels = c(0, 0.2, 0.4, 0.6)) +
  scale_x_continuous(labels = c(0, 0.25, 0.5, 0.75, 1))
print(p1)
if (savfig==1) {
  ggsave(filename = "scatter.eps",
         plot = print(p1),
         path = imgPth,
         width = 13.2,
         height = 6.15,
         units = "in",
         dpi = "retina",
         device = cairo_ps)
}

print("Figure 4")
invisible(readline(prompt="Press [enter] to continue"))

#########################
# FIG 5 & S12-S14
#########################
# map
statelab = as.character(datdes$state)
statelab[statelab=="NorthDakota"] = "North Dakota"
statelab[statelab=="SouthDakota"] = "South Dakota"
statelab[statelab=="NewJersey"] = "New Jersey"
statelab[statelab=="NewMexico"] = "New Mexico"
statelab[statelab=="NewYork"] = "New York"
statelab[statelab=="WestVirginia"] = "West Virginia"
statelab[statelab=="SouthCarolina"] = "South Carolina"
statelab[statelab=="NorthCarolina"] = "North Carolina"

if(!require(robustbase)){install.packages("robustbase", dep=T); library(robustbase)}
if(!require(usmap)){install.packages("usmap", dep=T); library(usmap)}

datdes2= datdes
dim(datdes)
datdes2$state = statelab
ns = length(unique(datdes2$state))
ad = as.data.frame(matrix(NA, ns, ncol(datdes2)))
colnames(ad) = names(datdes2)
dim(ad)
for (myper in 1:4){
  for (i in 1:ns) {
    tmp = datdes2[datdes2$period == myper & datdes2$state == unique(statelab)[i], ]
    itmp = sapply(tmp, is.numeric)
    ad[i, itmp] = colMedians(as.matrix(tmp[, itmp]))
    ad$state[i] = unique(statelab)[i]
  }
  ad$lost = ad$num_lost_colonies/ad$num_max_colonies
  labuse = F
  fsiz=12
  p1 = plot_usmap(data = ad, exclude = c("AK", "HI"),
                  values = "lost", color = "black", 
                  labels=labuse, label_color="white", size=0.5) + 
    theme(legend.position = "right") +
    theme(legend.title = element_text(size=fsiz),
          legend.text = element_text(size=fsiz)) +
    scale_fill_continuous(low="darkgreen", high="darkred", 
                          guide="colorbar",na.value="lightgray",
                          name = "Normalized\nColony\nLoss") 
  p1$layers[[2]]$aes_params$size <- 3
  # print(p1)
  
  p2 = plot_usmap(data = ad, exclude = c("AK", "HI"),
                  values = "mean.tmin", color = "black", 
                  labels=labuse, label_color="white", size=0.5) + 
    theme(legend.position = "right") +
    theme(legend.title = element_text(size=fsiz),
          legend.text = element_text(size=fsiz)) +
    scale_fill_continuous(low="blue", high="darkred", 
                          guide="colorbar",na.value="lightgray",
                          name =  "Mean\nMinimum\nTemp.") 
  p2$layers[[2]]$aes_params$size <- 3
  # print(p2)
  
  p3 = plot_usmap(data = ad, exclude = c("AK", "HI"),
                  values = "kurt.tmin", color = "black", 
                  labels=labuse, label_color="white", size=0.5) + 
    theme(legend.position = "right") +
    theme(legend.title = element_text(size=fsiz),
          legend.text = element_text(size=fsiz)) +
    scale_fill_continuous(low="blue", high="darkred", 
                          guide="colorbar",na.value="lightgray",
                          name =  "Kurtosis\nMinimum\nTemp.") 
  p3$layers[[2]]$aes_params$size <- 3
  # print(p3)
  
  p4 = plot_usmap(data = ad, exclude = c("AK", "HI"),
                  values = "skew.tmin", color = "black", 
                  labels=labuse, label_color="white", size=0.5) + 
    theme(legend.position = "right") +
    theme(legend.title = element_text(size=fsiz),
          legend.text = element_text(size=fsiz)) +
    scale_fill_continuous(low="blue", high="darkred", 
                          guide="colorbar",na.value="lightgray",
                          name =  "Skewness\nMinimum\nTemp.")  
  p4$layers[[2]]$aes_params$size <- 2
  # print(p4)
  
  figure <- ggarrange(p1, p2, p3, p4,
                      labels = c("a", "b", "c", "d"),
                      ncol = 2, nrow = 2)
  print(figure)
  
  if (myper==1) {
    print("Figure 5")
  } else if (myper>1) {
    print(paste0("Figure S", myper+10))
  }
  invisible(readline(prompt="Press [enter] to continue"))
  if (savfig==1) {
    ggsave(filename = paste0("map_period", myper, ".eps"),
           plot = print(figure),
           path = imgPth,
           width = 9.2,
           height = 6.15,
           units = "in",
           dpi = "retina",
           device = cairo_ps)
  }
}

#########################
# tests for "Up-scaling weather data" Section
#########################
reddatdes2 = datdes2
reddatdes2$lost = reddatdes2$num_lost_colonies/reddatdes2$num_max_colonies
# pearson correlation between losses and tmin
ycor = reddatdes2$lost
hist(ycor, 30)
xcor = reddatdes2$mean.tmin
hist(xcor, 30)
print(cor.test(ycor,xcor))
plot(xcor, ycor)
summary(lm(ycor~xcor))
abline(lm(ycor~xcor), col="blue")
cor.test(xcor,ycor)
length(ycor)
# t-tests for skewness and kurtosis (higher vs lower losses wrt median)
myper1 = 2
myper2 = 3
reddatdes2 = datdes2[datdes2$period == myper1 | datdes2$period == myper2, ]
reddatdes2$lost = reddatdes2$num_lost_colonies/reddatdes2$num_max_colonies
myind = reddatdes2$lost > median(reddatdes2$lost)
length(myind)
print(t.test(reddatdes2$kurt.tmin[myind], reddatdes2$kurt.tmin[!myind]))
print(t.test(reddatdes2$skew.tmin[myind], reddatdes2$skew.tmin[!myind]))
print("tests for 'Up-scaling weather data' Section")
invisible(readline(prompt="Press [enter] to continue"))

#########################
# FIG S2
#########################
dodge <- position_dodge(width = 0.8)
p<-ggplot(datdes, aes(x=year, y=num_lost_colonies/num_max_colonies, 
                      color=Quarter, fill=Quarter)) +
  geom_boxplot(lwd=0.1, width=0.75, alpha=1,
               outlier.size = 1.5, outlier.alpha = 1, outlier.shape = 21,
               color="black") +
  ylab("Normalized Colony Loss") +
  xlab("Year") +
  theme_bw() +
  theme(legend.position = c(0.92, 0.8), 
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="black")) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) 
print(p)
if (savfig==1) {
  ggsave(filename = "box_year.eps",
         plot = print(p),
         path = imgPth,
         width = 7.5,
         height = 5,
         dpi = "retina",
         device = cairo_ps)
}
print("Figure S2")
invisible(readline(prompt="Press [enter] to continue"))

#########################
# FIG S3-S6
#########################
# extract quarter
for (quart in 1:4) {
  if (quart == 1) {
    ytit = "Normalized Colony Loss - First Quarter"
  } else if (quart == 2){
    ytit = "Normalized Colony Loss - Second Quarter"
  } else if (quart == 3){
    ytit = "Normalized Colony Loss - Third Quarter"
  } else if (quart == 4){
    ytit = "Normalized Colony Loss - Fourth Quarter"
  }
  datdes1 = datdes[datdes$Quarter == quart, ]
  datdes1 = datdes1[order(datdes1$Region), ]
  datdes1$state <- factor(datdes1$state,
                          levels = unique(datdes1$state), ordered = TRUE)
  dodge <- position_dodge(width = 0.5)
  p<-ggplot(datdes1, aes(x=state, y=num_lost_colonies/num_max_colonies, 
                         color=Region, fill=Region)) +
    geom_boxplot(lwd=0.1, width=0.75, alpha=1,
                 outlier.size = 1.5, outlier.alpha = 1, outlier.shape = 21,
                 color="black") +
    coord_flip() +
    ylab(ytit) +
    xlab("State") +
    theme_bw() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)) +
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12)) +
    theme(legend.title = element_text(size=14),
          legend.text = element_text(size=12))
  print(p)
  print(paste0("Figure S", quart+2))
  invisible(readline(prompt="Press [enter] to continue"))
  if (savfig==1) {
    ggsave(filename = paste0("box_states", quart, ".eps"),
           plot = print(p),
           path = imgPth,
           width = 10.5,
           height =  9,
           dpi = "retina",
           device = cairo_ps)
  }
}

#########################
# FIG S9-S11
#########################
# pairs weather indexes

if(!require(GGally)){install.packages("GGally", dep=T); library(GGally)}

names(datdes)
# iterate across tmin, tmax, prec
for(i in 1:3){
  
  if (i==1) {
    datd_i = datdes[,16:22]
    flname = "pairs_tmin.eps"
  } else if (i==2) {
    datd_i = datdes[,23:29]
    flname = "pairs_tmax.eps"
  } else if (i==3) {
    datd_i = datdes[,30:36]
    flname = "pairs_prec.eps"
  }
  
  p = ggpairs(datdes, legend = 1,
              columns=names(datd_i), aes(color= Region),
              lower=list(continuous='points'), 
              diag = list(discrete="barDiag", 
                          continuous = wrap("densityDiag", alpha=0.5)),
              upper=list(continuous='blank')) +
    theme(legend.position = "top", legend.title = 
            element_text(size=14),
          legend.text = element_text(size=12)) +
    theme(axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20))
  print(p)
  print(paste0("Figure S", i+8))
  invisible(readline(prompt="Press [enter] to continue"))
  if (savfig==1) {
    ggsave(filename = flname,
           plot = p,
           path = imgPth,
           width = 8.5,
           height = 9,
           dpi = "retina",
           device = cairo_ps)
  }
}

