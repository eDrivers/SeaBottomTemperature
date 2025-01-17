# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize comes from DFO and cannot be shared
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './Data/RawData'

# Data will need to be archived to Zenodo with restricted access and downloaded
# using an access token.
# Eventually it would ideally be part of the SLGO web portal

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# File name
fileName <- dir(output, pattern = '.zip')

# Unzip kmz file
unzip(zipfile = paste0(output, '/', fileName),
      exdir = output)

# Get folder names
ys <- dir(output)
ys <- ys[!is.na(as.numeric(ys))]

# Create list to store data
sbt <- vector('list',length(ys))

# Load files
for(i in 1:length(ys)) {
  filePath <- dir(paste0(output,
                         '/',
                         ys[i],
                         '/'),
                  full.names = T)
  sbt[[i]] <- read.table(filePath)
}

# Identify unique coordinates
xy <- matrix(ncol = 2)
for(i in 1:length(sbt)) xy <- rbind(xy, sbt[[i]][, 1:2])
xy <- unique(xy) %>% na.omit()

# Join all data in a single dataset
for(i in 1:length(sbt)) xy <- dplyr::left_join(xy, sbt[[i]], by = c('V1' = 'V1', 'V2' = 'V2'))
colnames(xy)[3:(ncol(xy))] <- ys



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 SPATIAL OBJECT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spatial object and transform projection
sbt <- xy %>%
       st_as_sf(coords = c("V1", "V2"), crs = 4326) %>%
       st_transform(crs = 32198)

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
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(sbt, file = './data/rawData/sbt.RData')
