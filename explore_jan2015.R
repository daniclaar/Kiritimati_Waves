# Load necessary libraries
library(RcppCNPy)

# Clear working environment
rm(list=ls())

cols = c("#440154", "#472473", "#414487", "#355F8D", "#2A788E", "#21918C", "#22A884", "#44BF70", "#7AD151", "#BDDF26", "#FDE725")

hs_201501 <- npyLoad("hs_0360.npy") # Load in significant wave height data
hs_201501_df <- as.data.frame(hs_201501)

max(hs_201501)
hist(hs_201501)
