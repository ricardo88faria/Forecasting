#!/usr/bin/env Rscript

#packages:
library(rNOMADS)
library(lubridate)
library(fields)
library(ncdf)
library(GEOmap)
library(rasterVis)


#limpeza ambiente e objetos:
rm(list=ls())
cat("\014")

#####################################
print("Programado por Ricardo Faria")
#####################################


#verificacao da hora a fazer download
data_tempo <- Sys.time()
data <- format(data_tempo, "%Y%m%d")
hora <- format(data_tempo, "%H%M")

if (hora >= 0500 && hora < 1100){
  hora = "00"
} else if (hora >= 1100 && hora < 1700){
  hora = "06"
}  else if (hora >= 1700 && hora < 2200){
  hora = "12"
}  else {
  hora = "18"
}

#lista das horas e datas e a fazer:
#1-if existir file nao sacar
forecast_hour <- c("00", "03", "06", "09", "12", "15", "18", "21", "24")
forecast_hour_real <- as.POSIXct(hora, format="%H") + hours(as.numeric(forecast_hour))
forecast_hour_real_n <- format(forecast_hour_real, "%H")
forecast_data_real <- format(forecast_hour_real, "%Y%m%d")

#download file a fazer:
for (i in 1:length(forecast_hour)){
  link_ncep <- paste("ftp://ftp.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.", data, hora, "/gfs.t", hora, "z.sfluxgrbf", forecast_hour[i], ".grib2", sep = "")
  file_name_ncep <- paste("temp_", forecast_data_real[i],"-", forecast_hour_real_n[i],".grib2", sep = "")
  download.file(link_ncep, file_name_ncep, mode="w")
}

var_list <- c("LAND:","TMP:2","CRAIN:","PRATE:","SPFH:","VGRD:","UGRD:","TCDC:low","PWAT:")
var_list_clean <- c("LAND","TMP","CRAIN","PRATE","SPFH","VGRD","UGRD","TCDC","PWAT")

for (i in 1:length(forecast_hour)){
  #transform .grib to .nc atravez do terminal
  wgrib2 <- "./wgrib2"
  file_name_ncep <- paste("temp_", forecast_data_real[i],"-", forecast_hour_real_n[i],".grib2", sep = "")

    for (j in 1:length(var_list)) {
    save_file <- paste("temp_", var_list_clean[j], ".nc", sep="")
    command_TMP <- paste(wgrib2, " -s ", file_name_ncep, " | grep ", var_list[j], " | ", wgrib2, " -i ", file_name_ncep, " -netcdf ", save_file)
    system(command_TMP)
    }

  
  #ler netcdf
  nc_LAND <-open.ncdf("temp_LAND.nc")
  nc_TMP <-open.ncdf("temp_TMP.nc")
  nc_CRAIN <-open.ncdf("temp_CRAIN.nc")
  nc_PRATE <-open.ncdf("temp_PRATE.nc")
  nc_SPFH <-open.ncdf("temp_SPFH.nc")
  nc_VGRD <-open.ncdf("temp_VGRD.nc")
  nc_UGRD <-open.ncdf("temp_UGRD.nc")
  nc_TCDC <-open.ncdf("temp_TCDC.nc")
  nc_PWAT <-open.ncdf("temp_PWAT.nc")
  
  #variaveis
  day <- get.var.ncdf(nc_LAND,"time")
  
  x <- get.var.ncdf(nc_LAND,"longitude")
  long_limit <- c(320,360)
  #x <- x[x>320]
  #x <- x[x<360]
  #x_rot <- x - 180
  
  y <- get.var.ncdf(nc_LAND,"latitude")
  lat_limit <- c(25,45)
  #y <- y[y>25]
  #y <- y[y<45]
  
  variav_LAND <- get.var.ncdf(nc_LAND,"LAND_surface")
  
  variav_TMP <- get.var.ncdf(nc_TMP,"TMP_2maboveground") 
  variav_TMP <- variav_TMP - 273.15
  
  variav_CRAIN <- get.var.ncdf(nc_CRAIN,"CRAIN_surface")
  variav_PRATE <- get.var.ncdf(nc_PRATE,"PRATE_surface")
  
  variav_SPFH <- get.var.ncdf(nc_SPFH,"SPFH_2maboveground") 
  
  variav_VGRD <- get.var.ncdf(nc_VGRD,"VGRD_10maboveground")
  variav_UGRD <- get.var.ncdf(nc_UGRD,"UGRD_10maboveground")
  
  variav_TCDC <- get.var.ncdf(nc_TCDC,"TCDC_lowcloudlayer")
  
  variav_PWAT <- get.var.ncdf(nc_PWAT,"PWAT_entireatmosphere_consideredasasinglelayer_") 
  

  #gragicos
  rgb.palette.clouds <- colorRampPalette(c("royalblue1", "lightskyblue", "lightsteelblue1", "papayawhip","snow"), space = "rgb")
  rgb.palette <- colorRampPalette(c("snow1","snow2","snow3","seagreen","orange","firebrick"), space = "rgb")
  
  #grafico TMP
  name_png = paste("TMP_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_TMP,xlim=long_limit, ylim=lat_limit,col=rgb.palette(200),axes=T,main=as.expression(paste("Temperatura de 3 horas, a 2M do solo, ",forecast_data_real[1],"-", forecast_hour_real_n[1], ":00 UTC",sep="")),legend.lab="o C")
  #adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()

  
  #grafico CRAIN
  name_png = paste("CRAIN_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_CRAIN,xlim=long_limit, ylim=lat_limit,col=rgb.palette.clouds(100),axes=T,main=as.expression(paste("CRAIN ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="adimensional")
  #adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
  #grafico PRATE
  name_png = paste("PRATE_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_PRATE,xlim=long_limit, ylim=lat_limit,col=rgb.palette.clouds(100),axes=T,main=as.expression(paste("PRATE ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="kg/m^2/s")
  #adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
  #grafico SPFH
  name_png = paste("HR_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_SPFH,xlim=long_limit, ylim=lat_limit,col=rgb.palette(200),axes=T,main=as.expression(paste("SPFH media de 3 horas, a 2M do solo, ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="%")
  #adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
  #grafico UGRD
  name_png = paste("UGRD_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_UGRD,xlim=long_limit, ylim=lat_limit,col=rgb.palette(200),axes=T,main=as.expression(paste("UGRD media de 3 horas, a 2M do solo, ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="m/s")
  #adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
  #grafico VGRD
  name_png = paste("VGRD_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_VGRD,xlim=long_limit, ylim=lat_limit,col=rgb.palette(200),axes=T,main=as.expression(paste("VGRD media de 3 horas, a 2M do solo, ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="m/s")
  #adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
  #grafico TCDC
  name_png = paste("TCDC_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_TCDC,xlim=long_limit, ylim=lat_limit,col=rgb.palette(200),axes=T,main=as.expression(paste("Cobertura de nuvens, media de 3 horas, ", forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="%")
  #arrow.plot(as.vector(x) ,as.vector(y) , u = as.vector(variav_UGRD), v = as.vector(variav_VGRD),col=rgb.palette(200),axes=T,main=as.expression(paste("vetor do vento media de 3 horas, a 2M do solo, ",hora, ":00 UTC",sep="")),legend.lab="%")
  #adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
  #grafico PWAT
  name_png = paste("PWAT_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_PWAT,xlim=long_limit, ylim=lat_limit,col=rgb.palette(200),axes=T,main=as.expression(paste("Precipitacao, media de 3 horas, ", forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="kg/m^2")
  #arrow.plot(as.vector(x) ,as.vector(y) , u = as.vector(variav_UGRD), v = as.vector(variav_VGRD),col=rgb.palette(200),axes=T,main=as.expression(paste("vetor do vento media de 3 horas, a 2M do solo, ",hora, ":00 UTC",sep="")),legend.lab="kg/m^2")
  #adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()

}

#open_file <- paste("open ",name_tiff)
#system(open_file)
system("rm -rf temp_*.nc")
#system("rm -rf LAND_20*")
