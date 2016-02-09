#!/usr/bin/env Rscript

#packages:
library(rNOMADS)
library(lubridate)
library(fields)
library(ncdf)
library(ncdf4)
library(GEOmap)
library(rworldmap)
library(rworldxtra)
library(maptools)
library(mapdata)
library(maps)
library(RgoogleMaps)
library(rasterVis)
library(reshape2)
library(ggplot2)
library(grid)
library(OceanView)
library(RNetCDF)
library(animation)
library(plotly)
library(raster)
library(rgdal)
library(plotKML)


#limpeza ambiente e objetos:
rm(list=ls())
cat("\014")

#####################################
cat("Programado por Ricardo Faria \n
    necessário ter instalado o wgrib2 na raiz do programa e o Imagemagick(convert)")
#####################################

#Resolucao info
##0p25 ~ 27km
##T1534, with equivalent grid-resolution of 13 km  in use!!!!!!!!!!!

#sist. de coordenadas, projecao e coordenadas (N-S, E-O)
proj <- CRS('+proj=longlat +datum=WGS84')
coord_N <- 45
coord_S <- 25
coord_real_E <- 0
coord_real_O <- -40
coord_E <- 360 + coord_real_E
coord_O <- 360 + coord_real_O

coords_NS <- paste(coord_S, ":", coord_N, sep="")
coords_EO <- paste(coord_O, ":", coord_E, sep="")

#mapa
#maps_PRT <- readShapeLines("maps/PRT_adm1.shp", proj4string=proj)

#cores dos graficos
rgb.palette.clouds <- colorRampPalette(c("lightskyblue1", "snow1", "snow2", "snow3", "lightsteelblue3"," snow4"), space = "rgb")
rgb.palette.rain <- colorRampPalette(c("snow1", "lightsteelblue1", "yellowgreen", "orange", "tomato1", "violetred4"), space = "rgb")
rgb.palette.wind <- colorRampPalette(c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), space = "rgb") #"royalblue1",
rgb.palette.heat <- colorRampPalette(c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), space = "rgb") #"snow2",
rgb.palette.cat <- colorRampPalette(c("yellow1", "orange", "tomato1", "tomato4"), space = "rgb")
rgb.palette.ctp <- colorRampPalette(c("orchid3", "violetred", "blue"), space = "rgb")
rgb.palette.rad <- colorRampPalette(c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), space = "rgb")
rgb.palette.hr <- colorRampPalette(c("burlywood4", "burlywood", "darkseagreen", "palegreen2", "steelblue1", "royalblue3"), space = "rgb")
rgb.palette.soil <- colorRampPalette(c("firebrick", "sienna1", "orange", "seagreen", "steelblue1", "royalblue3"), space = "rgb")


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
}  else {
  hora = "18"
}

#lista das horas e datas e a fazer:
forecast_hour <- c("06",
                   "09",
                   "12",
                   "15",
                   "18",
                   "21",
                   "24",
                   "27",
                   "30",
                   "33",
                   "36",
                   "39",
                   "42",
                   "45",
                   "48",
                   "51",
                   "54",
                   "57")

forecast_hour_real <- as.POSIXct(hora, format="%H") + hours(as.numeric(forecast_hour))
forecast_hour_real_n <- format(forecast_hour_real, "%H")
forecast_data_real <- format(forecast_hour_real, "%Y%m%d")

#download file a fazer:
for (i in 1:length(forecast_hour)){
  source("download_ftp_ncep_gfs0p25.R")
}

var_list <- c("LAND:",
              "TMP:2",
              "DSWRF:",
              "CPRAT:",
              "PRATE:",
              "SPFH:",
              "VGRD:",
              "UGRD:",
              "TCDC:",
              "WATR:",
              #"SOILW:",
              "SOILL:",
              "SOILM:")

var_list_clean <- c("LAND",
                    "TMP",
                    "DSWRF",
                    "CPRAT",
                    "PRATE",
                    "SPFH",
                    "VGRD",
                    "UGRD",
                    "TCDC",
                    "WATR",
                    #"SOILW",
                    "SOILL",
                    "SOILM")

for (i in 1:length(forecast_hour)){
  #transform .grib to .nc atravez do terminal, ter wgrib2 instalado, importante!!!!!
  wgrib2 <- "./wgrib2"   #### tratar do rotate
  
  #./wgrib2  -s  temp_20160112-18.grib2  | grep  LAND:  |  ./wgrib2  -i  temp_20160112-18.grib2 -grib test.grib2
  #wgrib2 test.grib2 -small_grib 10:20 -20:20 small.test.grib2
  #wgrib2 small.test.grib2 -netcdf test.nc
  
  file_name_ncep <- paste("Downloads/temp_", forecast_data_real[i],"-", forecast_hour_real_n[i],".grib2", sep = "")
  
  for (j in 1:length(var_list)) {
    save_file_grib <- paste("temp/TEMP_", var_list_clean[j], ".grib2", sep="")
    save_file_grib1 <- paste("temp/TEMP_", var_list_clean[j], "_1.grib2", sep="")
    save_file_nc <- paste("temp/temp_", var_list_clean[j], ".nc", sep="")
    command_TMP1 <- paste(wgrib2, " -s ", file_name_ncep, " | grep ", var_list[j], " | ", wgrib2, " -i ", file_name_ncep, " -grib ", save_file_grib)
    system(command_TMP1)
    command_TMP2 <- paste(wgrib2, save_file_grib," -small_grib ", coords_EO, coords_NS, save_file_grib1)
    system(command_TMP2)
    command_TMP3 <- paste(wgrib2, save_file_grib1, " -netcdf ", save_file_nc)
    system(command_TMP3)
  }
  
  #WAFS transform .grib to .nc atravez do terminal, ter wgrib2 instalado, importante!!!!!
  file_name_ncep_WAFS <- paste("Downloads/temp_WAFS_", forecast_data_real[i],"-", forecast_hour_real_n[i],".grib2", sep = "")
  
  save_file_nc_WAFS <- paste("temp/temp_WAFS.nc", sep="")
  file_name_ncep_WAFS1 <- paste("temp/TEMP_WAFS_ncep.grib2", sep="")
  command_TMP2 <- paste(wgrib2, file_name_ncep_WAFS," -small_grib ", coords_EO, coords_NS, file_name_ncep_WAFS1)
  system(command_TMP2)
  command_TMP3 <- paste(wgrib2, file_name_ncep_WAFS1, " -netcdf ", save_file_nc_WAFS)
  system(command_TMP3)
  
  #abrir/ler netcdf
  nc_LAND <-open.ncdf("temp/temp_LAND.nc")         #Land contour
  nc_TMP <-open.ncdf("temp/temp_TMP.nc")           #Temperature at 2m [K]
  nc_DSWRF <-open.ncdf("temp/temp_DSWRF.nc")       #Downward Short-Wave Rad. Flux [W/m^2]
  nc_CPRAT <-open.ncdf("temp/temp_CPRAT.nc")       #Convective Precipitation Rate at surface [kg/m^2/s] == [mm/s]
  nc_PRATE <-open.ncdf("temp/temp_PRATE.nc")       #Precipitation Rate at surface [kg/m^2/s] == [mm/s]
  nc_SPFH <-open.ncdf("temp/temp_SPFH.nc")         #Maximum specific humidity at 2m [kg/kg]
  nc_VGRD <-open.ncdf("temp/temp_VGRD.nc")         #V-Component of Wind at 10m [m/s]  N-S
  nc_UGRD <-open.ncdf("temp/temp_UGRD.nc")         #U-Component of Wind at 10m [m/s]  E-O
  nc_TCDC <-open.ncdf("temp/temp_TCDC.nc")         #Total Cloud Cover at low middle and high bottom level [%]
  nc_WATR <-open.ncdf("temp/temp_WATR.nc")         #Water Runoff at surface [kg/m^2] == [mm]
  #nc_SOILW <-open.ncdf("temp/temp_SOILW.nc")       #Volumetric Soil Moisture Content [Fraction]
  nc_SOILL <-open.ncdf("temp/temp_SOILL.nc")       #Liquid Volumetric Soil Moisture (non Frozen) [Proportion]
  nc_SOILM <-open.ncdf("temp/temp_SOILM.nc")       #Soil Moisture Content [kg/m^2]
  
  
  res <- try(open.ncdf("temp/temp_WAFS.nc"), silent = TRUE)
  
  if (class(res) == "try-error") {
    cat(" W A R N I N G : saltando open netcdf WAFS, nao ha mais previsoes WAFS")
  } else {
    nc_WAFS <-open.ncdf("temp/temp_WAFS.nc")         #WAFS
  }
  
  #variaveis
  day <- get.var.ncdf(nc_LAND,"time")
  
  x <- get.var.ncdf(nc_LAND,"longitude")
  #long_limit <- c(coord_O,coord_E)
  #x <- x[x>320]
  #x <- x[x<360]
  #x_rot <- x - 180
  
  y <- get.var.ncdf(nc_LAND,"latitude")
  #lat_limit <- c(coord_S,coord_N)
  #y <- y[y>25]
  #y <- y[y<45]
  
  variav_LAND <- get.var.ncdf(nc_LAND,"LAND_surface")
  
  variav_TMP <- get.var.ncdf(nc_TMP,"TMP_2maboveground") #Temperature [K]
  variav_TMP <- variav_TMP - 273.15                      #Temperature [o C]
  
  variav_DSWRF <- get.var.ncdf(nc_DSWRF,"DSWRF_surface") #Downward Short-Wave Rad. Flux [W/m^2]
  
  variav_CPRAT <- get.var.ncdf(nc_CPRAT,"CPRAT_surface") #Convective Precipitation Rate [kg/m^2/s] == [mm/s]
  variav_PRATE <- get.var.ncdf(nc_PRATE,"PRATE_surface") #Precipitation Rate [kg/m^2/s] == [mm/s]
  variav_PREC <- (variav_PRATE + variav_CPRAT)*3600             #[kg/m^2/s] = [mm/h]
  #variav_PREC[variav_PREC <= 0.02 ] = 0
  
  #specific humidity (SPFH) to relative humidity (HR)
  variav_SPFH <- get.var.ncdf(nc_SPFH,"SPFH_2maboveground")
  
  #funcao de qair specific humidity, temp degrees C, pressure= 1013.25 mb, rh relative humidity
  qair2rh <- function(qair, temp, press = 1013.25){
    es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
    e <- qair * press / (0.378 * qair + 0.622)
    rh <- e / es
    rh[rh > 1] <- 1
    rh[rh < 0] <- 0
    return(rh)
  }
  
  variav_HR <- qair2rh(variav_SPFH, variav_TMP, press = 1013.25)*100
  
  variav_VGRD <- get.var.ncdf(nc_VGRD,"VGRD_10maboveground") #N-S [m/s]
  variav_UGRD <- get.var.ncdf(nc_UGRD,"UGRD_10maboveground") #E-O [m/s]
  variav_WINDM <- sqrt(variav_VGRD^(2) + variav_UGRD^(2))    #magnitude [m/s]
  
  #u <- melt(variav_UGRD,value.name = "u")
  #v <- melt(variav_VGRD,value.name = "v")
  #variav_WINDM <- merge(u, v)
  #variav_WINDM$x <- x[variav_WINDM[,1]]
  #variav_WINDM$y <- y[variav_WINDM[,2]]
  
  #df <- expand.grid(x=x, y=y)
  #data <- as.vector(variav_UGRD)
  #df$z <- with(df, data)
  #r <- rasterFromXYZ(df, crs=proj)
  #vectorplot(r, par.settings=RdBuTheme())
  
  variav_TCDC <- get.var.ncdf(nc_TCDC,"TCDC_entireatmosphere_consideredasasinglelayer_")      #Total Cloud Cover  [%]
  variav_TCDC_low <- get.var.ncdf(nc_TCDC,"TCDC_lowcloudlayer")                               #Total Cloud Cover  [%]
  variav_TCDC_middle <- get.var.ncdf(nc_TCDC,"TCDC_middlecloudlayer")                         #Total Cloud Cover  [%]
  variav_TCDC_high <- get.var.ncdf(nc_TCDC,"TCDC_highcloudlayer")                             #Total Cloud Cover  [%]
  variav_TCDC_tot <- (variav_TCDC_low + variav_TCDC_middle + variav_TCDC_high)/3              #Total Cloud Cover  [%]
  
  variav_WATR <- get.var.ncdf(nc_WATR,"WATR_surface")                           #Water Runoff at surface [kg/m^2] == [mm]
  
  #funcao para calcular Richardson Number, CAPE(from grib), Uave6000 &  Uave500 [m/s]
  Ri <- function(CAPE, Uave6000, Uave500){
    S <-  sqrt((1/2)*((Uave6000 - Uave500)^2))
    ri <- CAPE/(S^2)
    return(ri)
  }
  
  #variav_RI <- Ri(variav_CAPE, U6000, U500)
  
  
  #variav_SOILW_0_0p1 <- get.var.ncdf(nc_SOILW,"SOILW_0M0D1mbelowground")
  #variav_SOILW_0p1_0p4 <- get.var.ncdf(nc_SOILW,"SOILW_0D1M0D4mbelowground")
  #variav_SOILW_0p4_1 <- get.var.ncdf(nc_SOILW,"SOILW_0D4M1mbelowground")
  #variav_SOILW_1_2 <- get.var.ncdf(nc_SOILW,"SOILW_1M2mbelowground")
  #variav_SOILW <- variav_SOILW_0_0p1 + variav_SOILW_0p1_0p4 + variav_SOILW_0p4_1 + variav_SOILW_1_2
  
  variav_SOILL_0_0p1 <- get.var.ncdf(nc_SOILL,"SOILL_0M0D1mbelowground")
  variav_SOILL_0p1_0p4 <- get.var.ncdf(nc_SOILL,"SOILL_0D1M0D4mbelowground")
  variav_SOILL_0p4_1 <- get.var.ncdf(nc_SOILL,"SOILL_0D4M1mbelowground")
  variav_SOILL_1_2 <- get.var.ncdf(nc_SOILL,"SOILL_1M2mbelowground")
  variav_SOILL <- variav_SOILL_0_0p1 + variav_SOILL_0p1_0p4 + variav_SOILL_0p4_1 + variav_SOILL_1_2
  
  variav_SOILM <- get.var.ncdf(nc_SOILM,"SOILM_0M2mbelowground")
  
  
  variav_CAT_400mb <- get.var.ncdf(nc_WAFS,"CAT_400mb")               #Clear Air Turbulence (CAT) [%]
  variav_CAT_350mb <- get.var.ncdf(nc_WAFS,"CAT_350mb")
  variav_CAT_300mb <- get.var.ncdf(nc_WAFS,"CAT_300mb")
  variav_CAT_250mb <- get.var.ncdf(nc_WAFS,"CAT_250mb")
  variav_CAT_200mb <- get.var.ncdf(nc_WAFS,"CAT_200mb")
  variav_CAT_150mb <- get.var.ncdf(nc_WAFS,"CAT_150mb")
  
  
  variav_CTP_700mb <- get.var.ncdf(nc_WAFS,"CTP_700mb")               #In-Cloud Turbulence [%]
  variav_CTP_600mb <- get.var.ncdf(nc_WAFS,"CTP_600mb")
  variav_CTP_500mb <- get.var.ncdf(nc_WAFS,"CTP_500mb")
  variav_CTP_400mb <- get.var.ncdf(nc_WAFS,"CTP_400mb")
  variav_CTP_300mb <- get.var.ncdf(nc_WAFS,"CTP_300mb")
  
  variav_CBHE <- get.var.ncdf(nc_WAFS,"CBHE_entireatmosphere")*100
  x_WAFS <- get.var.ncdf(nc_WAFS,"longitude")
  y_WAFS <- get.var.ncdf(nc_WAFS,"latitude")
  
  
  #fechar netcdf files
  close.ncdf(nc_LAND)
  close.ncdf(nc_TMP)
  close.ncdf(nc_DSWRF)
  close.ncdf(nc_CPRAT)
  close.ncdf(nc_PRATE)
  close.ncdf(nc_SPFH)
  close.ncdf(nc_VGRD)
  close.ncdf(nc_UGRD)
  close.ncdf(nc_TCDC)
  close.ncdf(nc_WATR)
  ##close.ncdf(nc_SOILW)
  close.ncdf(nc_SOILL)
  close.ncdf(nc_SOILM)
  
  #graficos
  #NOTA graficos: col=rgb.palette.rain(99),  nlev = 100
  
  png_var <- c("TMP",
               "PREC",
               "DSWRF",
               "HR",
               "TCDC_tot",
               "WATR",
               #"SOILW",
               "SOILL",
               "SOILM")
  
  color_var <- list(rgb.palette.heat,
                    rgb.palette.rain,
                    rgb.palette.rad,
                    rgb.palette.hr,
                    rgb.palette.clouds,
                    rgb.palette.rain,
                    #rgb.palette.soil,
                    rgb.palette.soil,
                    rgb.palette.soil)
  
  titulo_var <- c(paste("Temperatura, a 2M do solo, no instante de tempo ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""),
                  paste("Precipitacao no solo, media de 3 horas ate ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""),
                  paste("Fluxo de Radiacao Global, media de 3 horas ate  ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""),
                  paste("Humidade Relativa, no instante de tempo ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""),
                  paste("Cobertura de nuvens de toda a atmosfera, media de 3 horas ate ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""),
                  paste("Quantidade de agua que flui sobre o solo (\"Water runoff\"), media de 3 horas ate ", forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""),
                  #paste("Proporcao de volume de misturado no solo ate 2 metros de profundidade, no instante de tempo ", forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""),
                  paste("Racio de volume de agua liquida misturada no solo ate 2 metros de profundidade, no instante de tempo ", forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""),
                  paste("Quantidade de agua misturada no solo ate 2 metros de profundidade, no instante de tempo ", forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep=""))
  
  #Volumetric Soil Moisture Content
  #Liquid Volumetric Soil Moistur
  #Soil Moisture Content [kg/m^2]
  
  legend_var <- c("[o C]",
                  "[mm/h]",
                  "[W/m^2]",
                  "[%]",
                  "[%]",
                  "[mm]",
                  #"[%]",
                  "[%]",
                  "[kg/m^2]")
  
  
  #cores caso ultrapasse limites
  
  if (max(variav_TMP)  >= 40) {
    max_TMP_col = max(variav_TMP)
  } else {
    max_TMP_col = 40
  }
  
  if (max(variav_PREC)  >= 15) {
    max_PREC_col = max(variav_PREC)
  } else {
    max_PREC_col = 15
  }
  
  if (max(variav_WATR, na.rm = T)  >= 20) {
    max_WATR_col = max(variav_WATR, na.rm = T)
  } else {
    max_WATR_col = 20
  }
  
  
  levels_var <- list(seq(-10, max_TMP_col, 1),
                     seq(0, max_PREC_col, 0.2),
                     seq(0, 1000, 20),
                     seq(0, 100, 2),
                     seq(0, 300, 5),
                     seq(0, max_WATR_col, 0.5),
                     seq(0, 2, 0.02),
                     seq(0, 2, 0.02),
                     seq(0, 1500, 50))
  
  
  for (j in 1:length(png_var)) {
    
    ##filled contour grafs
    variav_name <- paste("variav_", png_var[j], sep="")
    
    name_png = paste("Images/", png_var[j], "_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
    png(name_png, width = 7000, height = 4500, units = "px", res = 500)  #width = 14000, height = 9000, units = "px", res = 1000)
    
    filled.contour(x, y, get(variav_name), color = color_var[[j]], levels = levels_var[[j]], # nlevels = 400, #axes = F #(12), nlev=13,
                   plot.title = title(main = as.expression(paste(titulo_var[j])), xlab = 'Longitude [°]', ylab = 'Latitude [°]'),
                   plot.axes = {axis(1); axis(2); plot(getMap(resolution = "high"), add = T);grid()},
                   key.title = title(main =  as.expression(paste(legend_var[j]))))
    
    dev.off()
    
    ##level plot grafs
    #name_png = paste("images/", png_var[j], "_", forecast_data_real[i], "-", forecast_hour_real_n[i], ".png", sep = "")
    #png(name_png, width = 7000, height = 4500, units = "px", res = 500)  #width = 14000, height = 9000, units = "px", res = 1000)
    
    #image.plot(x , y, get(variav_name), col=color_var[j], xlab='Longitude [°]', ylab='Latitude [°]', axes = T, main = titulo_var[j], legend.lab = legend_var[j])
    #adiciona contornos do land
    #grid()
    #plot(getMap(resolution = "high"), add = T)
    #map('world', fill=F, col='grey50', add = TRUE)
    #contour(x , y, variav_LAND, add=TRUE, lwd=1, labcex=1, levels=0.99,drawlabels=FALSE,col="grey30")
    #plotGEOmap(coastmap, border = "black", add = TRUE, MAPcol = NA, shiftlon = 0)
    #dev.off()
    
  }
  
  #grafico WINDM
  
  if (max(variav_WINDM)  >= 35) {
    max_WINDM_col = max(variav_WINDM)
  } else {
    max_WINDM_col = 35
  }
  
  name_png_WINDM = paste("images/WINDM_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
  png(name_png_WINDM, width = 7000, height = 4500, units = "px", res = 500)
  
  filled.contour(x, y, variav_WINDM, color = rgb.palette.wind, levels = seq(0, max_WINDM_col, 1),  #nlevels = 100, #axes = F #(12), nlev=13,
                 plot.title = title(main = as.expression(paste("Direcao e intensidade de Vento, no instante de tempo ", forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),
                                    xlab = 'Longitude [°]', ylab = 'Latitude [°]'),
                 plot.axes = {axis(1); axis(2); plot(getMap(resolution = "high"), add = T); quiver2D(variav_UGRD, variav_VGRD, x = x, y = y, add= T, scale =1.2, arr.max = 0.2, by = 5, type = "triangle", col = "grey50"); grid()},
                 key.title = title(main = as.expression(paste("[m/s]"))))
  
  #image.plot(x ,y,variav_WINDM, col=rgb.palette.wind(400), xlab='Longitude [°]', ylab='Latitude [°]',axes=T,main=as.expression(paste("Velocidade e direcao do vento,a 2M do solo, no instante de tempo ",forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")),legend.lab="[m/s]")
  #grid()
  #plot(getMap(resolution = "high"), add = T)
  #quiver2D(variav_UGRD, variav_VGRD, x = x, y = y, add= T, scale =1.2, arr.max = 0.2, by = 5, type = "triangle", col = "grey50")  #colvar = variav_WINDM, col = rgb.palette.wind(200)
  
  dev.off()
  
  
  #Graficos WAFS
  
  res <- try(open.ncdf("temp/temp_WAFS.nc"), silent = TRUE)
  
  if (class(res) == "try-error") {
    cat(" W A R N I N G : saltando graficos WAFS, nao ha mais previsoes WAFS")
  } else {
    
    png_var_WAFS <- c("CBHE",
                      "CAT_150mb",
                      "CAT_200mb",
                      "CAT_300mb",
                      "CAT_400mb",
                      #"CTP_300mb",
                      "CTP_500mb",
                      "CTP_600mb",
                      "CTP_700mb")
    
    titulo_var_WAFS <- c("Tipo de nuvens Cumulonimbus ",
                         "Turbulencia a 150mb (13503 metros), no instante ",
                         "Turbulencia a 200mb (Altura de voo 390 ou 11770 metros), no instante ",
                         "Turbulencia a 300mb (Altura de voo 300 ou 9160 metros), no instante ",
                         "Turbulencia a 400mb (Altura de voo 240 ou 7182 metros), no instante ",
                         #"Turbulencia nas nuvens a 300mb (Altura de voo 300 ou 9160 metros), no instante ",
                         "Turbulencia nas nuvens a 500mb (Altura de voo 180 ou 5572 metros), no instante ",
                         "Turbulencia nas nuvens a 600mb (Altura de voo 140 ou 4205 metros), no instante ",
                         "Turbulencia nas nuvens a 700mb (Altura de voo 100 ou 3011 metros), no instante ")
    
    
    color_var <- list(rgb.palette.clouds,
                      rgb.palette.cat,
                      rgb.palette.cat,
                      rgb.palette.cat,
                      rgb.palette.cat,
                      rgb.palette.ctp,
                      rgb.palette.ctp,
                      rgb.palette.ctp)
    
    levels_var <- list(seq(0, 100, 1),
                       seq(5, 100, 1),
                       seq(5, 100, 1),
                       seq(5, 100, 1),
                       seq(5, 100, 1),
                       seq(0.005, 0.1, 0.002),
                       seq(0.005, 0.1, 0.002),
                       seq(0.005, 0.1, 0.002))
    
    for (j in 1:length(png_var_WAFS)) {
      
      variav_name <- paste("variav_", png_var_WAFS[j], sep="")
      
      name_png = paste("images/", png_var_WAFS[j], "_", forecast_data_real[i],"-", forecast_hour_real_n[i], ".png", sep = "")
      png(name_png, width = 7000, height = 4500, units = "px", res = 500)
      
      filled.contour(x_WAFS, y_WAFS, get(variav_name), color = color_var[[j]], levels = levels_var[[j]], #(12), nlev=13,
                     plot.title=title(main=as.expression(paste(titulo_var_WAFS[j], forecast_data_real[i],"-", forecast_hour_real_n[i], ":00 UTC",sep="")), xlab='Longitude [°]', ylab='Latitude [°]'),
                     plot.axes={axis(1); axis(2);map('worldHires', add=TRUE);grid()},
                     key.title = title(main =  as.expression(paste("[%]"))))
      
      dev.off()
      
    }
    
  }
  
  system("rm -rf temp/temp_*.nc")
  system("rm -rf temp/TEMP_*.grib2")
  
}

#GIFs

gifs_var <- c("TMP_",
              "PREC_",
              "DSWRF_",
              "HR_",
              "WINDM_",
              "TCDC_tot_",
              "WATR_",
              "CBHE_",
              "CAT_150mb_",
              "CAT_200mb_",
              "CAT_300mb_",
              "CAT_400mb_",
              #"CTP_300mb_",
              "CTP_500mb_",
              "CTP_600mb_",
              "CTP_700mb_")

for (i in 1:length(gifs_var)) {
  
  gif_name <- paste("GIFs/", gifs_var[i], data, "-", forecast_hour_real_n[1], ".gif", sep="")
  
  system(paste("convert -verbose -resize 30% -delay 80 -loop 0", paste("Images/", gifs_var[i],"*", sep=""), gif_name))
  
}

print(Sys.time() - data_tempo)

cat("Programado por Ricardo Faria \n
    Finalizado")
