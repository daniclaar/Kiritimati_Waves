rm(list=ls())

library(RcppCNPy)
hs_all <- npyLoad("hs_all.npy")
hs_all <- hs_all[3:37894]
tp_all <- npyLoad("tp_all.npy")
tp_all <- tp_all[3:37894]
dp_all <- npyLoad("dp_all.npy")
dp_all <- dp_all[3:37894]
range(dp_all)
range(hs_all)
range(tp_all)

waves <- as.data.frame(cbind(hs_all,tp_all,dp_all))
time <- seq.POSIXt(from=ISOdatetime(2005, 01, 01, 00, 00, 00), to=ISOdatetime(2018, 01, 01, 00, 00, 00), by= "3 hours")

waves2 <- waves[1:37985,]
wave_time <- cbind(waves2,time) # This is pretty shit right now

head(waves)

NNW <- waves[which(waves$dp_all > 315 & waves$dp_all < 355),]
descdist(NNW$hs_all, discrete=FALSE)

fit.gamma.NNW <- fitdist(NNW$hs_all,'gamma')
plot(fit.gamma.NNW)
fit.gamma.NNW$aic

fit.lnorm.NNW <- fitdist(NNW$hs_all,'lnorm')
plot(fit.lnorm.NNW)
fit.lnorm.NNW$aic

fit.weibull.NNW <- fitdist(NNW$hs_all,'weibull')
plot(fit.weibull.NNW)
fit.weibull.NNW$aic

quantile(fit.gamma.NNW)
quantile(fit.gamma.NNW,probs = c(0.666,0.9,0.95,0.98,0.99,0.999))


fit.fevd.GEV.NNW <- fevd(NNW$hs_all,type="GEV",time.units = "hours")
plot(fit.fevd.GEV.NNW)
ci(fit.fevd.GEV.NNW)


NNW$group <- NNW$hs_all
NNW$group[NNW$group<=1] <- "a"
NNW$group[which(NNW$group<=1.2 & NNW$group>1)] <- "b"
NNW$group[which(NNW$group<=1.5 & NNW$group>1.2)] <- "c"
NNW$group[which(NNW$group<=1.8 & NNW$group>1.5)] <- "d"
NNW$group[which(NNW$group<=2 & NNW$group>1.8)] <- "e"
NNW$group[which(NNW$group<=2.2 & NNW$group>2)] <- "f"
NNW$group[which(NNW$group<=2.5 & NNW$group>2.2)] <- "g"
NNW$group[which(NNW$group<=4 & NNW$group>2.5)] <- "h"
NNW$group <- as.factor(NNW$group)


cols = c("#440154", "#46327E", "#365C8D", "#277F8E", "#1FA187", "#4AC16D", "#A0DA39", "#FDE725")


ggplot(data=NNW) + 
  theme_classic()+
  geom_histogram(aes(x=hs_all,fill=group),show.legend = F,binwidth=0.01) +
  geom_vline(xintercept = 2.74, linetype="dashed")+
  scale_fill_manual(values=cols) +
  scale_x_continuous(expand=c(0,0),name = "Significant Wave Height") +
  scale_y_continuous(expand=c(0,0), name = "Count")

ggplot(data=NNW) +
  theme_classic()+
  geom_point(aes(x=hs_all,y=tp_all,color=time))
