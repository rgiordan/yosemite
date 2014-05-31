
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/nls.html
library(stats)
library(ggplot2)

# Load and clean the data
setwd("~/Documents/yosemite/")
load("combined_data.Rdata")
data$year <- sub("\\.csv$", "", sub("^UPLY", "", data$file))
head(data)

ggplot(data) +
  geom_point(aes(x=DOY, y=pixAvg, color=year)) +
  geom_vline(aes(xintercept=265)) +
  geom_vline(aes(xintercept=50))

d <- subset(data, as.numeric(DOY) < 265 & as.numeric(DOY) > 50)
d <- subset(d, !is.na(pixAvg))


# Look at how constant the variance is, transform to something more equivariant
d1 <- subset(d, year=="2010")
log.offset <- 7
d1$log.pix.avg <- log(d1$pixAvg + log.offset)
window <- 30
d1$day.window <- floor((d1$DOY - min(d1$DOY)) / window)
dagg <- aggregate(d1[c("pixAvg", "log.pix.avg")], by=d1["day.window"], sd)
d1 <- merge(d1, dagg, by="day.window", suffixes=c("", ".sd"))
ggplot(d1) +
  geom_point(aes(x=DOY, y=pixAvg)) +
  geom_point(aes(x=DOY, y=pixAvg.sd, color="sd"))
ggplot(d1) +
  geom_point(aes(x=DOY, y=log.pix.avg)) 
ggplot(d1) +
  geom_point(aes(x=DOY, y=log.pix.avg.sd, color="sd")) + expand_limits(y = 0)

# Make a column "y" in the original data set that has been log-transformed
d$y <- log(d$pixAvg + log.offset)
ggplot(d) + geom_point(aes(x=DOY, y=y, color=year))

##################
# Fit a sigmoid
d1 <- subset(d, year=="2012")
qplot(d1$DOY, d1$y, geom="point")
mu.start=190
sigma.start=25
scale.start=2.5
min.val.start <- 2
start.list <- list(mu=mu.start, sigma=sigma.start, scale=scale.start, min.val=min.val.start)

# Set these intelligently by looking at a full year
kMinVal <- 2.1
kScale <- 2.3

Sigmoid <- function(x, mu, sigma, scale, min.val) {
  # TODO(soldenwa): Make sure this all makes sense to you.
  #                 See it in your dreams, live it when you wake.
  return(min.val + scale / (1 + exp((x - mu) / sigma)))
}

BoundedSigmoid <- function(x, mu, sigma) {
  # TODO(soldenwa): Make sure this all makes sense to you.
  #                 See it in your dreams, live it when you wake.
  return(kMinVal + kScale / (1 + exp((x - mu) / sigma)))
}

# Plot the starting fit
start.fit <- Sigmoid(x=d1$DOY, mu=start.list$mu, sigma=start.list$sigma,
                     min.val=start.list$min.val, scale=start.list$scale)
ggplot(d1) + 
  geom_point(aes(x=DOY, y=y)) +
  geom_line(aes(x=DOY, y=start.fit))

# Fit the data
fit <- nls(data=d1,
           formula=y ~ Sigmoid(x=DOY, min.val=min.val, scale=scale, mu=mu, sigma=sigma),
           start=start.list)
bounded.fit <- nls(data=d1,
                   formula=y ~ BoundedSigmoid(x=DOY, mu=mu, sigma=sigma),
                   start=start.list[c("mu", "sigma")])
summary(fit)
summary(bounded.fit)


# Plot the final fit
fit.vals <- as.list(summary(fit)$coefficients[, "Estimate"])
final.fit <- Sigmoid(x=d1$DOY, mu=fit.vals$mu, sigma=fit.vals$sigma,
                     min.val=fit.vals$min.val, scale=fit.vals$scale)
ggplot(d1) + 
  geom_point(aes(x=DOY, y=y)) +
  geom_line(aes(x=DOY, y=final.fit))

# Plot the bounded final fit
bounded.fit.vals <- as.list(summary(bounded.fit)$coefficients[, "Estimate"])
final.bounded.fit <- BoundedSigmoid(x=d1$DOY,
                                    mu=bounded.fit.vals$mu, sigma=bounded.fit.vals$sigma)
ggplot(d1) + 
  geom_point(aes(x=DOY, y=y)) +
  geom_line(aes(x=DOY, y=final.bounded.fit))


