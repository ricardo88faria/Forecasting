#!/usr/bin/env Rscript

#packages:
library(rNOMADS) 
library(GEOmap)
library(aqfig)


#limpeza ambiente e objetos:
rm(list=ls())
cat("\014")

#####################################
print("Programado por Ricardo Faria")
#####################################


## MAKE GLOBAL MAPS OF TEMP AND WIND SPEED WITH GFS

save.fig <- TRUE #If TRUE, save as postscript, if FALSE, display

#Get libraries for weather data and global mapping
library(rNOMADS) 
library(GEOmap)
library(aqfig)

#Get latest GFS model run
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 1)

#Get analysis (0 hour) forecast
model.parameters <- ParseModelPage(urls.out[1])
latest.pred <- model.parameters$pred[1:6]

#Where in the atmosphere to get data
levels <- c("2 m above ground", "10 m above ground", "300 mb")

#Get temperature and wind
variables <- c("TMP", "UGRD", "VGRD")

#Get the data
grib.info <- GribGrab(urls.out[1], latest.pred[1], levels, variables)

#Read it into R
Sys.setenv(PATH = "/Users/ricardofaria/Documents/testes_grib2nc")
grib.data <- ReadGrib(grib.info$file.name, levels, variables)
#Sys.unsetenv("PATH")

resolution <- c(0.5, 0.5) #Resolution of the model

#Make an array for easier manipulation
atmos <- ModelGrid(grib.data, resolution)

#Set up display
#First, plot temperature at 2 m above ground
li <- which(atmos$levels == "2 m above ground")
vi <- which(atmos$variables == "TMP")
colormap <- rev(rainbow(500, start = 0 , end = 5/6))

if(save.fig) {
  postscript("world_surface_temp.eps")
}

image(atmos$x + 180, sort(atmos$y), atmos$z[li,vi,,], col = colormap,
      xlab = "Longitude", ylab = "Latitude",
      main = paste("World Temperature at Ground Level (deg C):", atmos$fcst.date, "GMT"))

#Plot coastlines
plotGEOmap(coastmap, border = "black", add = TRUE,
           MAPcol = NA, shiftlon = 180)

#Plot legend, convert to Celsius
vertical.image.legend(col=colormap, zlim = range(atmos$z[li,vi,,] - 273.15))

if(save.fig) {
  dev.off()
}

if(save.fig) {
  postscript("world_surface_wind.eps")
}

#Next, plot wind magnitude at 10 m above ground
li <- which(atmos$levels == "10 m above ground")
vi.zonal <- which(atmos$variables == "UGRD") #East-West wind
vi.merid <- which(atmos$variables == "VGRD") #North-South wind

wind.mag <- sqrt(atmos$z[li,vi.zonal,,]^(2) + atmos$z[li,vi.merid,,]^(2))
colormap <- rev(rainbow(500, start = 0 , end = 5/6))

image(atmos$x + 180, sort(atmos$y), wind.mag, col = colormap,
      xlab = "Longitude", ylab = "Latitude",
      main = paste("World Winds at Ground Level (km/hr):", atmos$fcst.date, "GMT"))

#Plot coastlines
plotGEOmap(coastmap, border = "black", add = TRUE, 
           MAPcol = NA, shiftlon = 180)

#Plot legend, convert to km/hr
vertical.image.legend(col=colormap, zlim = range(wind.mag * 3.6))

if(save.fig) {
  dev.off()
}

if(save.fig) {
  postscript("world_strato_wind.eps")
}

#Finally, plot winds at 300 mb
li <- which(atmos$levels == "300 mb")
wind.mag <- sqrt(atmos$z[li,vi.zonal,,]^(2) + atmos$z[li,vi.merid,,]^(2))
colormap <- rev(rainbow(500, start = 0 , end = 5/6))

image(atmos$x + 180, sort(atmos$y), wind.mag, col = colormap,
      xlab = "Longitude", ylab = "Latitude",
      main = paste("World Winds at 300 mb (km/hr):", atmos$fcst.date, "GMT"))

#Plot coastlines
plotGEOmap(coastmap, border = "black", add = TRUE, 
           MAPcol = NA, shiftlon = 180)

#Plot legend, convert to km/hr
vertical.image.legend(col=colormap, zlim = range(wind.mag * 3.6))
if(save.fig) {
  dev.off()
}