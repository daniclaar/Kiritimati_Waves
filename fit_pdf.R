library(RcppCNPy)
library(MASS)
library(fitdistrplus)
library('extRemes')
library(ggplot2)
library(extraDistr)

cols = c("#440154", "#46327E", "#365C8D", "#277F8E", "#1FA187", "#4AC16D", "#A0DA39", "#FDE725")


hs_all <- npyLoad("hs_all_0360.npy")
hs_all <- hs_all[4:37894]
hs_all_df <- as.data.frame(hs_all)

hist(hs_all)
plot(density(hs_all))
f<-fitdistr(hs_all, 'lognormal')


plot(ecdf(hs_all))

# ks.test(hs_all,'rlnorm')
# ks.test(hs_all,'plnorm')
# ks.test(hs_all,'gamma')
# ks.test(hs_all,'pweibull',shape=2)

descdist(hs_all, discrete=FALSE)

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
descdist(hs_all, discrete=FALSE, boot=1000)

fit.fevd.GEV <- fevd(hs_all,type="GEV",time.units = "2922/year",span=13)
plot(fit.fevd.GEV)
fit.fevd.Gumbel <- fevd(hs_all,type="Gumbel")
plot(fit.fevd.Gumbel)
# fit.fevd.Exp <- fevd(data=hs_all_df,x=hs_all,type="Exponential")
# plot(fit.fevd.Gumbel)
# colnames(hs_all_df)
# fit.fevd.Exp <- fevd(mylist,hs_all,type="Exponential")

ci(fit.fevd.GEV)

# quantile(fit.lnorm)
# quantile(fit.lnorm,probs = c(0.666,0.75,0.9,0.95,0.98,0.99,0.999,0.9999))

# The waves in January 2015 were 2.52m, which is >98% of waves during the past 12 years.

hist(hs_all,breaks = 30)

hs_all_df$group <- hs_all
hs_all_df$group[hs_all_df$group<=1] <- "a"
hs_all_df$group[which(hs_all_df$group<=1.2 & hs_all_df$group>1)] <- "b"
hs_all_df$group[which(hs_all_df$group<=1.5 & hs_all_df$group>1.2)] <- "c"
hs_all_df$group[which(hs_all_df$group<=1.8 & hs_all_df$group>1.5)] <- "d"
hs_all_df$group[which(hs_all_df$group<=2 & hs_all_df$group>1.8)] <- "e"
hs_all_df$group[which(hs_all_df$group<=2.2 & hs_all_df$group>2)] <- "f"
hs_all_df$group[which(hs_all_df$group<=2.5 & hs_all_df$group>2.2)] <- "g"
hs_all_df$group[which(hs_all_df$group<=4 & hs_all_df$group>2.5)] <- "h"
hs_all_df$group <- as.factor(hs_all_df$group)



g1 <- ggplot(data=hs_all_df) + 
  theme_classic()+
  geom_histogram(aes(x=hs_all,fill=group,color="black"),show.legend = F,binwidth=0.01,size=0.01) +
  #geom_vline(xintercept = 2.74, linetype="dashed")+
  scale_fill_manual(values=cols) +
  scale_x_continuous(expand=c(0,0),name = "Significant Wave Height (m)",limits = c(0,4)) +
  scale_y_continuous(expand=c(0,0), name = "Count") + 
  scale_color_manual(values="#565656")
g1

jpeg(filename = "wave_hist.jpg",height = 4.5,width=7, units="in",res=300)
g1
dev.off()



fit.rayleigh <- fitdist(hs_all,'rayleigh',start=list(sigma=1))
plot(fit.rayleigh)

f <- fitdist(hs+theta,'weibull')

