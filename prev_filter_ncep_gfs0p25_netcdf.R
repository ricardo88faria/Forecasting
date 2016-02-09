#!/usr/bin/env Rscript

#linha de erro
options(error=traceback)

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

if (hora >= "0500" && hora < "1100"){
  hora = "00"
} else if (hora >= "1100" && hora < "1700"){
  hora = "06"
}  else if (hora >= "1700" && hora < "2200"){
  hora = "12"
}  else if (hora >= "2200" && hora < "0500"){
  hora = "18"
}

#lista das horas e datas e a fazer:
#1-if existir file nao sacar

forecast_hour <- c("00", "03", "06", "09", "12", "15", "18", "21", "24")
forecast_hour_real <- as.POSIXct(hora, format="%H") + hours(as.numeric(forecast_hour))
forecast_hour_real_n <- format(forecast_hour_real, "%H")
forecast_data_real <- format(forecast_hour_real, "%Y%m%d")

#download land and forecast data

for (i in 1:length(forecast_hour)){
  link_ncep <- paste("http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?file=gfs.t", hora, "z.pgrb2.0p25.f0", forecast_hour[i], "&lev_surface=on&var_LAND=on&lev_2_m_above_ground=on&lev_300_mb=on&lev_350_mb=on&lev_400_mb=on&lev_450_mb=on&lev_500_mb=on&var_APCP=on&var_CRAIN=on&var_DSWRF=on&var_GFLUX=on&var_CLWMR=on&var_RH=on&var_TMP=on&var_UGRD=on&var_VGRD=on&subregion=&leftlon=320&rightlon=360&toplat=45&bottomlat=25&dir=%2Fgfs.", data, hora, sep = "")
  file_name_ncep <- paste("temp_", forecast_data_real[i], forecast_hour_real_n[i],".grb", sep = "")
  download.file(link_ncep, file_name_ncep, mode="wb")
}

#ciclo para tratar varios ficheiros

#transform .grib para .nc atravez do terminal
wgrib2 <- "./wgrib2"
#Sys.setenv(PATH = "/Users/ricardofaria/Documents/testes_grib2nc")
#Sys.unsetenv("PATH")

for (i in 1:length(forecast_hour)){
  
  save_file_name_ncep <- paste("temp_", forecast_data_real[i], forecast_hour_real_n[i], ".nc", sep = "")
  grib_file <- paste("temp_", forecast_data_real[i], forecast_hour_real_n[i],".grb", sep = "")
  command <- paste(wgrib2, grib_file, " -netcdf temp.nc") #, save_file_name_ncep)
  system(command)
  

#ler netcdf
  nc_file <- open.ncdf("temp.nc")

#variaveis
  variav_LAND <- get.var.ncdf(nc_file,"LAND_surface")

  #variav_CRAIN <- get.var.ncdf(nc_file,"CRAIN_surface")
    
  variav_TMP <- get.var.ncdf(nc_file,"TMP_2maboveground") 
  variav_TMP <- variav_TMP - 273.15
  
  variav_RH <- get.var.ncdf(nc_file,"RH_2maboveground") 
  
  variav_CLWMR <- get.var.ncdf(nc_file,"CLWMR_300mb")
  variav_CLWMR <- variav_CLWMR + get.var.ncdf(nc_file,"CLWMR_350mb")
  variav_CLWMR <- variav_CLWMR + get.var.ncdf(nc_file,"CLWMR_400mb")
  variav_CLWMR <- variav_CLWMR + get.var.ncdf(nc_file,"CLWMR_450mb")
  variav_CLWMR <- variav_CLWMR + get.var.ncdf(nc_file,"CLWMR_500mb")
  
  variav_VGRD <- get.var.ncdf(nc_file,"VGRD_300mb")
  
  variav_UGRD <- get.var.ncdf(nc_file,"UGRD_300mb")
  
#  variav_UVGRD <- (((variav_VGRD^2)+(variav_UGRD^2))^(1/2))
#  df <- expand.grid(x, y)
#  z <- as.vector(variav_UVGRD)
#  df$z <- with(df, z)
#  wind_raster <- rasterFromXYZ(df)
#  vectorplot(wind_raster)
  
  day <- get.var.ncdf(nc_file,"time")
  x <- get.var.ncdf(nc_file,"longitude")
#x_rot <- x - 180
  y <- get.var.ncdf(nc_file,"latitude")

  rgb.palette.clouds <- colorRampPalette(c("royalblue1", "lightskyblue", "lightsteelblue1", "papayawhip","snow"), space = "rgb")
  rgb.palette <- colorRampPalette(c("snow1","snow2","snow3","seagreen","orange","firebrick"), space = "rgb")

#grafico crain
  #name_png = paste("crain_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  #png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  #image.plot(x ,y,variav_CRAIN,col=rgb.palette(200),axes=T,main=as.expression(paste("Precipitacao de 3 horas, a 2M do solo, ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="o C")
#adiciona contornos do land 
#contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
  #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  #dev.off()
  
#grafico temp
  name_png = paste("temp_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)

  image.plot(x ,y,variav_TMP,col=rgb.palette(200),axes=T,main=as.expression(paste("Temperatura de 3 horas, a 2M do solo, ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="o C")
#adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
#plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
#grafico CLWMR
  name_png = paste("CLWMR_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_CLWMR,col=rgb.palette.clouds(100),axes=T,main=as.expression(paste("CLWMR ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="m")
#adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
#plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
#grafico RH
  name_png = paste("HR_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_RH,col=rgb.palette(200),axes=T,main=as.expression(paste("HR media de 3 horas, a 2M do solo, ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="%")
#adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
#plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
#grafico UGRD
  name_png = paste("UGRD_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_UGRD,col=rgb.palette(200),axes=T,main=as.expression(paste("UGRD media de 3 horas, a 2M do solo, ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="m/s")
#adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
#plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
#grafico VGRD
  name_png = paste("VGRD_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_VGRD,col=rgb.palette(200),axes=T,main=as.expression(paste("VGRD media de 3 horas, a 2M do solo, ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="m/s")
#adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
#plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
#grafico vetor wind
  name_png = paste("VUGRD_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png, width = 14000, height = 9000, units = "px", res = 1000)
  
  image.plot(x ,y,variav_VGRD,col=rgb.palette(200),axes=T,main=as.expression(paste("VGRD media de 3 horas, a 2M do solo, ", forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="m/s")
#arrow.plot(as.vector(x) ,as.vector(y) , u = as.vector(variav_UGRD), v = as.vector(variav_VGRD),col=rgb.palette(200),axes=T,main=as.expression(paste("vetor do vento media de 3 horas, a 2M do solo, ",hora, ":00 UTC",sep="")),legend.lab="m/s")
#adiciona contornos do land 
  contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30") 
#plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
  dev.off()
  
  
#open_file <- paste("open ",name_png)
#system(open_file)
#mov_file <- paste("mv temp.nc ",save_file_name_ncep)
#system(mov_file)
}

