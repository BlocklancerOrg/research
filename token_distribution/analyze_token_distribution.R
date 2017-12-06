#merge the files and save merged csv

library(ggplot2)
library(ineq)

symbols <- c("EOS", "ANT", "ATM", "BAT", "BNT")
data_list <- c()
for(symbol in symbols){
  path <- paste("../data/", symbol, sep = "")
  data <- read.csv("../data/EOS")
  data <- cbind(data, symbol = rep("EOS", nrow(data)))
  data_list <- c(data_list, data)
}

merged_data <- do.call(rbind, data_list)
print(merged_data)

