## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# Here we comment out the Download_tif function calls to avoid them from being run in R package tests. They can be used shown:
#Download raster files
#library(LSTModis)
#Download_tif(username=".",password=".",start_date="2017-12-19",end_date="2017-12-21", option=1,path_files="/Users/piyushgupta/Desktop")

## ------------------------------------------------------------------------
#Download raster files
#library(LSTModis)
#Download_tif(username=".",password=".",start_date="2017-12-19",end_date="2017-12-21", option=2,path_files="/Users/piyushgupta/Desktop")

## ------------------------------------------------------------------------
df<-Compute_ModisLST(path_to_tif="/Users/piyushgupta/Dropbox2/Dropbox/PIYUSH.DEC2017/MODIS-PROJECT/Surf_Temp_Daily_005dg_v6/LST_Day_CMG",
                     path_to_shapefiles="/Users/piyushgupta/Dropbox2/Dropbox/PIYUSH.DEC2017/MODIS-PROJECT/shapefiles/DISTRICT.shp",
                     path_mod_shapefile="/Users/piyushgupta/Desktop/",
                     aggregate="weekly")

