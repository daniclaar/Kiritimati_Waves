library(RcppCNPy)
library(MASS)

hs_201501 <- npyLoad("hs.npy")
hist(hs_201501,xlim=c(0,3.5))

hs_201501_df <- data.frame(hs_201501)
head(hs_201501_df)

hs_201501_df$group <- hs_201501_df$hs_201501
hs_201501_df$group[hs_201501_df$group<=1] <- "a"
hs_201501_df$group[which(hs_201501_df$group<=1.2 & hs_201501_df$group>1)] <- "b"
hs_201501_df$group[which(hs_201501_df$group<=1.5 & hs_201501_df$group>1.2)] <- "c"
hs_201501_df$group[which(hs_201501_df$group<=1.8 & hs_201501_df$group>1.5)] <- "d"
hs_201501_df$group[which(hs_201501_df$group<=2 & hs_201501_df$group>1.8)] <- "e"
hs_201501_df$group[which(hs_201501_df$group<=2.2 & hs_201501_df$group>2)] <- "f"
hs_201501_df$group[which(hs_201501_df$group<=2.5 & hs_201501_df$group>2.2)] <- "g"
hs_201501_df$group[which(hs_201501_df$group<=4 & hs_201501_df$group>2.5)] <- "h"
hs_201501_df$group <- as.factor(hs_201501_df$group)

cols = c("#440154", "#46327E", "#365C8D", "#277F8E", "#1FA187", "#4AC16D", "#A0DA39", "#FDE725")


g1 <- ggplot(data=hs_201501_df) + 
  theme_classic()+
  geom_histogram(aes(x=hs_201501,fill=group,color="black"),show.legend = F,binwidth=0.01,size=0.01) +
  geom_vline(xintercept = 2.74, linetype="dashed")+
  scale_fill_manual(values=cols) +
  scale_x_continuous(expand=c(0,0),name = "Significant Wave Height (m)",limits=c(0,3)) +
  scale_y_continuous(expand=c(0,0), name = "Count") + 
  scale_color_manual(values="#565656")
g1

