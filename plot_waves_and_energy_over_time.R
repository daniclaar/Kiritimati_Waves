# Import necessary libraries
library(RcppCNPy)

# Clear working environment
rm(list=ls())

# Load in and subset data - chose to remove the first 29 values, since they look suspiciously different from the rest of the values, and I expect that they are due to model initialization or similar.
hs_all <- npyLoad("hs_all_0360.npy") # Load in significant wave height
hs_all <- hs_all[30:37894]
tp_all <- npyLoad("tp_all_0360.npy") # Load in wave period
tp_all <- tp_all[30:37894]
dp_all <- npyLoad("dp_all_0360.npy") # Load in wave direction
dp_all <- dp_all[30:37894]
# range(dp_all)
# range(hs_all)
# range(tp_all)

# Bind together significant wave height, wave period, and wave direction
waves <- as.data.frame(cbind(hs_all,tp_all,dp_all))
waves2 <- waves[1:37737,]
time <- seq.POSIXt(from=ISOdatetime(2005, 02, 01, 00, 00, 00), to=ISOdatetime(2018, 01, 01, 00, 00, 00), by= "3 hours")
waves <- cbind(waves2,time)

# Calculate wave energy (rough calculation) and plot
waves$we_all <- (((waves$hs_all)^2)*waves$tp_all)/2
hist(waves$we_all)
npySave('we_all.npy',waves$we_all)

g2 <- ggplot(data=waves) +
  geom_point(aes(x=time, y=we_all,color=dp_all),alpha=0.1)+
  scale_color_gradientn(colours = rainbow(500))+
  scale_y_continuous(name="Wave energy (kW/m)")+
  scale_x_datetime(name="Date",date_breaks = "1 year",date_labels = "%Y")
g2
 
# I am not convinced this time/value matchup is correct! Double check if including!!
jpeg("wave_energy_by_time.jpg",width=7, height=4, units="in", res=300)
g2
dev.off()

g3 <- ggplot(data=waves) +
  geom_point(aes(x=time, y=hs_all,color=dp_all),alpha=0.1)+
  scale_color_gradientn(colours = rainbow(500))+
  scale_y_continuous(name="Significant Wave Height (m)")+
  scale_x_datetime(name="Date",date_breaks = "1 year",date_labels = "%Y")
  #geom_hline(yintercept=1.63) +
  #geom_hline(yintercept=1.74) +
  #geom_hline(yintercept=2.05) +
  #geom_hline(yintercept=2.52) +
  #geom_hline(yintercept=2.72)
g3

# I am not convinced this time/value matchup is correct! Double check if including!!
jpeg("wave_height_by_time.jpg",width=7, height=4, units="in", res=300)
g3
dev.off()

# Plot significant wave height (x) by wave period (y) and color by direction
g4 <- ggplot(data=waves) +
  theme_classic()+
  geom_point(aes(x=hs_all,y=tp_all,color=dp_all),alpha=0.5)+
  scale_color_gradientn(colours = rainbow(500))+
  scale_y_continuous(limits = c(0,20))+
  scale_x_continuous(limits = c(0,4))
g4
jpeg("wave_height_period_direction.jpg",width=7, height=4, units="in", res=300)
g4
dev.off()


g5 <- ggplot(data=waves) +
  theme_classic()+
  geom_point(aes(x=hs_all,y=tp_all,color=we_all),alpha=0.5)+
  scale_color_gradientn(colours = terrain.colors(500))+
  scale_y_continuous(limits = c(0,20))+
  scale_x_continuous(limits = c(0,4))
g5
jpeg("wave_height_period_energy.jpg",width=7, height=4, units="in", res=300)
g5
dev.off()

# Need to double check time series matchup before subsetting and plotting this.
# waves_201501 <- waves[c(29399:29505),]
# 
# ggplot(data=waves_201501) +
#   theme_classic()+
#   geom_point(aes(x=hs_all,y=tp_all,color=dp_all),alpha=0.5)+
#   scale_color_gradientn(colours = rainbow(500))+
#   scale_y_continuous(limits = c(0,20))+
#   scale_x_continuous(limits = c(0,4))
# 
# ggplot(data=waves_201501) +
#   theme_classic()+
#   geom_point(aes(x=hs_all,y=tp_all,color=we_all),alpha=0.5)+
#   scale_color_gradientn(colours = terrain.colors(500))+
#   scale_y_continuous(limits = c(0,20))+
#   scale_x_continuous(limits = c(0,4))
