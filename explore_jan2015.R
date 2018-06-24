# Load necessary libraries
library(RcppCNPy)
library(ggplot2)

# Clear working environment
rm(list=ls())

cols = c("#440154", "#472473", "#414487", "#355F8D", "#2A788E", "#21918C", "#22A884", "#44BF70", "#7AD151", "#BDDF26", "#FDE725")

hs_201501 <- npyLoad("hs_0360.npy") # Load in significant wave height data
hs_201501_df <- as.data.frame(hs_201501)
dp_201501 <- npyLoad("dp_0360.npy") # Load in significant wave height data
dp_201501_df <- as.data.frame(dp_201501)
tp_201501 <- npyLoad("tp_0360.npy") # Load in significant wave height data
tp_201501_df <- as.data.frame(tp_201501)

max(hs_201501)
hist(hs_201501)

hist(tp_201501)
max(tp_201501)

we_201501 <- (((hs_201501)^2)*tp_201501)/2
hist(we_201501)
plot(we_201501)
max(we_201501)

waves_201501 <- as.data.frame(cbind(dp_201501,tp_201501,hs_201501,we_201501))

g4 <- ggplot(data=waves_201501) +
  theme_classic()+
  geom_point(aes(x=hs_201501,y=tp_201501,color=dp_201501),alpha=0.5)+
  scale_color_gradientn(colours = rainbow(249))+
  scale_y_continuous(limits = c(0,20))+
  scale_x_continuous(limits = c(0,4))
g4

