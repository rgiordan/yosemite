
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/nls.html
library(stats)
library(ggplot2)


setwd("~/Documents/yosemite/")
load("combined_data.Rdata")
data$year <- sub("\\.csv$", "", sub("^UPLY", "", data$file))
head(data)

ggplot(data) +
  geom_point(aes(x=DOY, y=pixAvg, color=year)) +
  geom_vline(aes(xintercept=265)) +
  geom_vline(aes(xintercept=50))

d <- subset(data, as.numeric(DOY) < 265 & as.numeric(DOY) > 50)

d1 <- subset(d, file="UPLY2010.csv")
head(d1)
