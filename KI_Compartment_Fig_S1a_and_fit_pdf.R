# Load necessary libraries
library(RcppCNPy)
library(MASS)
library(fitdistrplus)
library('extRemes')
library(ggplot2)
library(extraDistr)

# Clear working environment
rm(list=ls())

cols = c("#440154", "#472473", "#414487", "#355F8D", "#2A788E", "#21918C", "#22A884", "#44BF70", "#7AD151", "#BDDF26", "#FDE725")


hs_all <- npyLoad("hs_all_0360.npy") # Load in significant wave height data
hs_all <- hs_all[30:37894] # Remove first values, as they look suspiciously like model initialization
hs_all_df <- as.data.frame(hs_all)

hist(hs_all)
plot(density(hs_all))
plot(ecdf(hs_all))
descdist(hs_all, discrete=FALSE, boot=1000)

fit.gamma <- fitdist(hs_all,'gamma')
plot(fit.gamma)
fit.gamma$aic

fit.lnorm <- fitdist(hs_all,'lnorm')
plot(fit.lnorm)
fit.lnorm$aic

fit.weibull <- fitdist(hs_all,'weibull')
plot(fit.weibull)
fit.weibull$aic

plot.legend <- c("gamma", "lognormal", "Weibull")
denscomp(list(fit.gamma, fit.lnorm, fit.weibull), legendtext = plot.legend)
qqcomp(list(fit.gamma, fit.lnorm, fit.weibull), legendtext = plot.legend)
cdfcomp(list(fit.gamma, fit.lnorm, fit.weibull), legendtext = plot.legend)
ppcomp(list(fit.gamma, fit.lnorm, fit.weibull), legendtext = plot.legend)

plotdist(hs_all, histo = TRUE, demp = TRUE)

fit.fevd.GEV <- fevd(hs_all,type="GEV",time.units = "2922/year",span=13)
plot(fit.fevd.GEV)
# fit.fevd.Gumbel <- fevd(hs_all,type="Gumbel")
# plot(fit.fevd.Gumbel)

# x <- 2:4
# devd.GEV <- devd(x, loc = 1.7837346, scale = 0.3451910, shape = -0.1509318, threshold = 0, log = FALSE,type = c("GEV"))
# pevd.GEV <- pevd(x, loc = 1.7837346, scale = 0.3451910, shape = -0.1509318, threshold = 0, log = FALSE,type = c("GEV"))
# 
# plot(pevd.GEV)

ci(fit.fevd.GEV)

hs_all_df$group <- hs_all
hs_all_df$group[hs_all_df$group<=1] <- "a"
hs_all_df$group[which(hs_all_df$group<=1.2 & hs_all_df$group>1)] <- "b"
hs_all_df$group[which(hs_all_df$group<=1.5 & hs_all_df$group>1.2)] <- "c"
hs_all_df$group[which(hs_all_df$group<=1.8 & hs_all_df$group>1.5)] <- "d"
hs_all_df$group[which(hs_all_df$group<=2 & hs_all_df$group>1.8)] <- "e"
hs_all_df$group[which(hs_all_df$group<=2.2 & hs_all_df$group>2)] <- "f"
hs_all_df$group[which(hs_all_df$group<=2.5 & hs_all_df$group>2.2)] <- "g"
hs_all_df$group[which(hs_all_df$group<=2.8 & hs_all_df$group>2.5)] <- "h"
hs_all_df$group[which(hs_all_df$group<=3 & hs_all_df$group>2.8)] <- "i"
hs_all_df$group[which(hs_all_df$group<=3.2 & hs_all_df$group>3)] <- "j"
hs_all_df$group[which(hs_all_df$group<=4 & hs_all_df$group>3.2)] <- "k"
hs_all_df$group <- as.factor(hs_all_df$group)



g1 <- ggplot(data=hs_all_df) + 
  theme_classic()+
  theme(text=element_text(size=22))+
  geom_histogram(aes(x=hs_all,fill=group,color="black"),show.legend = F,binwidth=0.1,size=0.1) +
  geom_vline(xintercept = 2.94, linetype="dashed")+
  scale_fill_manual(values=cols) +
  scale_x_continuous(expand=c(0,0),name = "Significant Wave Height (m)",limits = c(0,4)) +
  scale_y_continuous(expand=c(0,0), name = "Count") + 
  scale_color_manual(values="#565656")
g1

jpeg(filename = "KI_Compartment_Fig_S1a_wave_hist.jpg",height = 4.5,width=7, units="in",res=300)
g1
dev.off()

pdf(file = "KI_Compartment_Fig_S1a_wave_hist.pdf",height = 4.5,width=7)
g1
dev.off()
