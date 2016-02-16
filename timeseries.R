## -------------- STIDF --------- ##
library(sp)
library(spacetime)
## daily temperatures for Croatia:
data(HRtemp08)
## format the time column:
HRtemp08$ctime <- as.POSIXct(HRtemp08$DATE, format="%Y-%m-%dT%H:%M:%SZ")
## create a STIDF object:
sp <- SpatialPoints(HRtemp08[,c("Lon","Lat")])
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
HRtemp08.st <- STIDF(sp, time = HRtemp08$ctime, data = HRtemp08[,c("NAME","TEMP")])
## subset to first 500 records:
HRtemp08_jan <- HRtemp08.st[1:500]
str(HRtemp08_jan)
## Not run: 
plotKML(HRtemp08_jan[,,"TEMP"], LabelScale = .4)