#ficheiro <- paste("ll", file_name_ncep)
#if (as.logical(system(ficheiro)) = "TRUE"){

#NOMADS NCEP list ~ 111.32km
#link_ncep <- paste("http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?file=gfs.t", hora, "z.pgrb2.0p25.f0", forecast_hour[i], "&lev_10_m_above_ground=on&lev_2_m_above_ground=on&lev_entire_atmosphere=on&lev_entire_atmosphere_%5C%28considered_as_a_single_layer%5C%29=on&lev_high_cloud_layer=on&lev_low_cloud_layer=on&lev_middle_cloud_layer=on&lev_surface=on&var_ACPCP=on&var_APCP=on&var_CPRAT=on&var_HGT=on&var_LAND=on&var_PRATE=on&var_RH=on&var_SPFH=on&var_TCDC=on&var_TMP=on&var_UGRD=on&var_VGRD=on&var_WATR=on&subregion=&leftlon=320&rightlon=360&toplat=45&bottomlat=25&dir=%2Fgfs.", data, hora, sep = "")

#GFS WAFS ftp
link_ncep_WAFS <- paste("ftp://ftp.ncep.noaa.gov/data/nccf/com/gfs/prod/gfs.", data, hora, "/WAFS_blended_", data, hora, "f", forecast_hour[i], ".grib2", sep = "")
file_name_ncep_WAFS <- paste("Downloads/temp_WAFS_", forecast_data_real[i],"-", forecast_hour_real_n[i],".grib2", sep = "")
#download.file(link_ncep_WAFS, file_name_ncep_WAFS, mode="curl")
system(paste("wget ", link_ncep_WAFS, "-O", file_name_ncep_WAFS))

#GFS NCEP ftp T1534 Semi-Lagrangian grid ~ 13km
link_ncep <- paste("ftp://ftp.ncep.noaa.gov/data/nccf/com/gfs/prod/gfs.", data, hora, "/gfs.t", hora, "z.sfluxgrbf", forecast_hour[i], ".grib2", sep = "")
file_name_ncep <- paste("Downloads/temp_", forecast_data_real[i],"-", forecast_hour_real_n[i],".grib2", sep = "")
#download.file(link_ncep, file_name_ncep, mode="curl")
system(paste("wget ", link_ncep, "-O", file_name_ncep))
#} else {
#}
