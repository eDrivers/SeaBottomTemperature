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
