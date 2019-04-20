# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                    DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./data/rawData/sbt.RData')
ys <- colnames(sbt)[-ncol(sbt)]
nY <- length(ys)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                CHECK OUTLIERS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data in data.frame
dat <- sbt[, ys, drop = T]

# Visualize outliers
graphicsutils::plot0(xlim = c(0,5), ylim = c(-20, 20))
mtext('Outliers', 3, font = 2)
for(i in 1:length(ys)) boxplot(dat[, i], add = T, at = i, frame = F, range = 3)
axis(1, at = 1:length(ys), labels = ys)

# Identify and modify outliers in the dataset
for(i in 1:length(ys)) {
  out <- boxplot.stats(dat[, i], coef = 3)$stat[c(1,5)]
  cap <- quantile(dat[, i], probs=c(.05, .95), na.rm = T)
  dat[, i][dat[, i] < out[1]] <- cap[1]
  dat[, i][dat[, i] > out[2]] <- cap[2]
}

# Visualize outliers again
graphicsutils::plot0(xlim = c(0,5), ylim = c(-20, 20))
mtext('Outliers', 3, font = 2)
for(i in 1:length(ys)) boxplot(dat[, i], add = T, at = i, frame = F, range = 3)
axis(1, at = 1:length(ys), labels = ys)

# Replace data in sf object
sbt[, ys] <- dat


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   ANOMALIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform values between -.5 and +.5 to NA
sbt[, 1:nY] <- apply(sbt[, 1:nY, drop = T], 2, function(x) ifelse(x > -.5 & x < .5, NA, x))

# Positive & negative anomalies
pos <- neg <- sbt
pos[, 1:nY] <- apply(pos[, 1:nY, drop = T], 2, function(x) ifelse(x < 0, NA, x))
neg[, 1:nY] <- apply(neg[, 1:nY, drop = T], 2, function(x) ifelse(x > 0, NA, x))

# Transform negative anomalies as positive values
neg[, 1:nY] <- apply(neg[, 1:nY, drop = T], 2, abs)

# Transform NAs back to 0 to be able to measure annual means per grid cell
pos[, 1:nY] <- apply(pos[, 1:nY, drop = T], 2, function(x) ifelse(is.na(x), 0, x))
neg[, 1:nY] <- apply(neg[, 1:nY, drop = T], 2, function(x) ifelse(is.na(x), 0, x))

# Mean of all annual indices as the anomaly index
pos$PositiveSBT <-rowMeans(pos[, 1:nY, drop = T])
neg$NegativeSBT <-rowMeans(neg[, 1:nY, drop = T])

# Change file names
posSBT <- pos
negSBT <- neg

# Select only mean
posSBT <- posSBT[, 'PositiveSBT']
negSBT <- negSBT[, 'NegativeSBT']

# Select values > 0
posSBT <- posSBT[posSBT$PositiveSBT > 0, ]
negSBT <- negSBT[negSBT$NegativeSBT > 0, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(posSBT, file = './Data/Driver/PositiveSBT.RData')
save(negSBT, file = './Data/Driver/NegativeSBT.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 VISUALIZE DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png('./Figures/PositiveSBT.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(posSBT[, 'PositiveSBT'], pch = 20, cex = .5)
dev.off()

png('./Figures/NegativeSBT.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(negSBT[, 'NegativeSBT'], pch = 20, cex = .5)
dev.off()
