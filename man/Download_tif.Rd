% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Download_tif.R
\name{Download_tif}
\alias{Download_tif}
\title{Download tif files}
\usage{
Download_tif(username = "abc", password = "**", start_date, end_date,
  option = 1, path_files)
}
\arguments{
\item{username}{Character String; Username}

\item{password}{Character String; Password}

\item{start_date}{Character String; The start date}

\item{end_date}{Character String; The end date}

\item{option}{Numeric value; Takes value 1 or 2 : 1-USE default options
2-USE GUI for personalized options}

\item{path_files}{Character String; Path where the user wants tif files to be stored}
}
\description{
Function automates download of the .tif files (raster files).The defualt parameters include :-
Product: "Surf_Temp_Daily_005dg (M*D11C1)",
sensor: "Terra",
prod_version: "6",
out_format: "GTiff",
MODIStspVersion: "1.3.3.1",
timeseries_format: "ENVI Meta Files".
Original MODIS Layers: Daytime land surface temperature
Quality Indicators: Mandatory QA flag(day)
The script doesn't download the .hdf files.
}
\examples{
pwd<-getwd()
Download_tif(username="abc",password="**",
                           start_date="2017-12-19",
                           end_date="2017-12-21",
                           option=2,
                           path_files=pwd)
}
