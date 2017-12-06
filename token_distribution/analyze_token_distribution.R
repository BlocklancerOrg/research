#merge the files and save merged csv
library(ggplot2)
library(ineq)
library(dplyr)
library(ineq)

symbols <- c("EOS", "ANT", "ATM", "BAT", "BNT", "CFI", "COB", "CVC","DGD", "EDG", "ENG","FUN", "GNO", "GNT", "ICN","KCS","KIN","KNC", "LINK", "LRC", "MANA","MCAP", "MCO", "MGO", "MLN", "MTL", "OMG","OTN","PLR","PPP","PPT","QRL","RCN","REP","REQ","RLC","SALT","SAN","SNGLS","SNT","STORJ", "SUB","TRST", "TRX", "VERI", "WINGS", "XUC", "ZRX", "ZSC")

for(symbol in symbols){
  path <- paste("../data/", symbol, sep = "")
  data <- read.csv(path)
  data <- cbind(data, symbol = rep(symbol, nrow(data)))
  data_list[[symbol]] <- data
}

merged_data <- bind_rows(data_list)
write.csv(merged_data, file = "../data/merged.csv", sep = ",", row.names = FALSE)

ggplot(merged_data, aes(x = X, y = tokens, group = symbol, color = symbol)) + geom_line() + scale_y_log10() +
  theme_bw()

lc_list <- list()
ginis <- data.frame(symbol = character(), gini = numeric())
for(symbol in symbols){
  Lc.p <- Lc(as.vector(data_list[[symbol]][["tokens"]]))
  lc <- data.frame(p = Lc.p["p"], L = Lc.p["L"], symbol = rep(symbol,length(Lc.p["p"])))
  lc_list[[symbol]] <- lc
  gini_coeff <- ineq(as.vector(data_list[[symbol]][["tokens"]]), type = "Gini")
  ginis <- rbind(ginis, data.frame(symbol = symbol, gini = gini_coeff))
}

merged_lcs <- bind_rows(lc_list)
write.csv(merged_lcs, file = "../data/lorenz.csv", sep = ",", row.names = FALSE)
write.csv(ginis, file = "../data/ginis.csv", sep = ",", row.names = FALSE)

ggplot(merged_lcs, aes(x = p, y = L, group = symbol, color = symbol)) + 
  geom_line() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_bw() +
  facet_wrap(~symbol, nrow = 10, ncol = 5)

ggplot(ginis, aes (x = symbol, y = gini, color = symbol)) + 
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
