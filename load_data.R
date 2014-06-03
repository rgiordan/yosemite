library(reshape2)

setwd("~/Documents/yosemite")

# Read the data.
data.list <- list()
watersheds <- c("BALA", "EMLA", "MILY", "UPLY")
years <- as.character(2010:2013)
for (watershed in watersheds) {
  for (year in years) {
    this.data <- read.csv(sprintf("%s%s.csv", watershed, year))
    this.data$watershed <- watershed
    this.data$year <- year
    names(this.data)[1] <- "day"
    data.list[[watershed]][[year]]<- this.data
  }
}

# Inspect the data
# Different years have different numbers of pixels?!?
for (watershed in watersheds) {
  print("-----------")
  print(watershed)
  for (year in years){
    print(year)
    print(head(data.list[[watershed]][[year]]))
    print(ncol(data.list[[watershed]][[year]]))
    print(sum(is.na(data.list[[watershed]][[year]])))
  }
}


# Combine the years, removing observations >= 100
watershed.list <- list()
for (watershed in watersheds) {
  print(watershed)
  watershed.list[[watershed]] <- do.call(rbind, lapply(years, function(year) {
    melt(data.list[[watershed]][[year]], id.vars=c("year", "day", "watershed"))
  }))
  watershed.list[[watershed]] <- subset(watershed.list[[watershed]], value < 100)
} 
all.watersheds <- do.call(rbind, watershed.list)

ggplot(watershed.list[[2]]) +
  geom_point(aes(x=day, y=value, color=variable, group=variable)) +
  facet_grid(~ year) +
  theme(legend.position="none")

if (F) {
  # Kinda slow
  ggplot(all.watersheds) +
    geom_point(aes(x=day, y=value, color=variable, group=variable)) +
    facet_grid(watershed ~ year) +
    theme(legend.position="none")
}


ggplot(subset(all.watersheds, watershed=="UPLY" & year == 2011)) +
  geom_line(aes(x=day, y=value, color=variable, group=variable)) +
  facet_grid(watershed ~ year) +
  theme(legend.position="none")


data.mean <- aggregate(all.watersheds["value"],
                       by=all.watersheds[c("watershed", "year", "day")],
                       mean)
data.sd <- aggregate(all.watersheds["value"],
                     by=all.watersheds[c("watershed", "year", "day")],
                     sd)
data.agg <- merge(data.mean, data.sd, by=c("watershed", "year", "day"),
                  suffixes=c("", ".sd"))

ggplot(data.agg) +
  geom_line(aes(x=day, y=value)) +
  facet_grid(watershed ~ year)

