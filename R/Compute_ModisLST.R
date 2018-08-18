#Instructions:-
#' @title  Compute MODIS Land Surface Temperature
#' @description Function automates the computation of MODIS Land Surface Temperature.It creates Shapefiles with Temperature columns appended and it is stored at the path provided by the user.This requires the user to download the raster files first.
#' The user must use Download_tif to download the required raster files.
#' @import jsonlite
#' @import stringr
#' @import MODIStsp
#' @import sp
#' @import methods
#' @export Compute_ModisLST
#' @param path_to_tif Character String; Path to the folder that contains the tif files
#' @param path_to_shapefiles ;Character String or SpatialPolygonsDataFrames object; The path to the shapefile (.shp) or the shapefile object
#' @param path_mod_shapefile ;Character String ; The path where the user wants the Modified Shapefile to be stored
#' @param aggregate ;Character String ; Aggregate values  1. daily
#'                                                        2. weekly
#'                                                        3. monthly
#'                                                        4. yearly
#' @return Dataframe with new columns appended with Land Surface Temperature in Celcius and modified shapefiles.
#' @examples shape_dsn <- system.file("vectors", package = "LSTModis")[1]
#' @examples tif_dsn <- system.file("pictures", package = "LSTModis")[1]
#' @examples Shapefile <- rgdal::readOGR(dsn=shape_dsn, layer="Shapefile")
#' @examples pwd <- getwd()
#' @examples df<-Compute_ModisLST(path_to_tif = tif_dsn ,
#'           path_to_shapefiles = Shapefile ,
#'           path_mod_shapefile = pwd,
#'           aggregate = "weekly")
Compute_ModisLST<-function(path_to_tif,path_to_shapefiles,path_mod_shapefile,aggregate){



  l<-c("daily","weekly","monthly","yearly")

  if(aggregate %in% l ){



    #Calling rasterVector function from Rscript rasterVectorMODIS.R
    df<-rasterVector(directory=path_to_tif,shapefile=path_to_shapefiles,  differentiatingString="MOD11C1")


      shape <- path_to_shapefiles


    shape_frame<-data.frame(shape)

    # Processing data further and adding columns to the shapefile.
    myfun<-function(temp)
    {
      t<-NULL
      t<-list()
      temp<-data.frame(temp)
      day_no<-unique(temp$day_no)
      t[[day_no]]<-temp$pixels.mean
      return(t)
    }


    myData<-data.frame(lapply(df, myfun))
    names(myData) = gsub(pattern = "X", replacement = "", x = names(myData))

    #Calculating the Temperatures using scaling factor=0.02 and the add-factor=0
    newData<-myData*.02-273.15
    columns<-compute(newData,aggregate)

    final<-cbind(shape_frame,columns)

    shape@data<-final
    path<-paste(path_mod_shapefile,sep = "","Shapefile")


    if (requireNamespace("raster", quietly = TRUE)) {
      raster::shapefile(shape,path,overwrite=TRUE)

    }
    return(final)



  }else{
    cat(" please enter a valid aggregate" ,"...", sep = "")
  }


}


rasterVector <-   function(directory = ".",
                           shapefile=".",
                           differentiatingString=NULL,...) {

  #load the required libraries

  #Check for character class inputs
  list_char<-c("directory","differentiatingString")

  for(i in list_char){
    if(!(is.character(get(i)))){
      stop(paste(i," not a CHARACTER entry. Please enter valid ",i))
    }
  }

  #Check if the raster files directory exists
  if(!(dir.exists(directory))){
    stop(paste("Invalid raster files directory, ",directory," does not exist. Please enter valid directory"))
  }

  #Check whether shapefile is provided as address or an object
  if(class(shapefile) == "SpatialPolygonsDataFrame")
  {
    shape<-shapefile
  }
  else
  {
    #Check if shapefile exists
        shape <- shapefile
  }

  #Save the current directory
  initial.directory <- getwd()

  #Change the directory to the directory that contains the .tif files (raster files)
  setwd(directory)

  #Extract the files that are raster in the directory i.e contain the extension *.tif
  rasterFiles <- list.files(pattern= "*.tif$")

  #Use the data frame of the shapefile for further manipulations
  df <- shape@data

  cat("Performing the Raster calculations ", "...", sep = "")
  temp<-grep(differentiatingString, rasterFiles, value = TRUE)

  if (requireNamespace("raster", quietly = TRUE)) {
    raster_list<-lapply(temp,raster::raster)
    ras1 <- lapply(raster_list,raster::crop, shape)
  }



  shape<- sp::spTransform(shape, CRS(proj4string(ras1[[1]])))
  proj4string(shape) <- proj4string(ras1[[1]])

  #Use the external csv file to take in the vector which has names of functions to be used to calculate zonal
  # statistics
  csv.file<- list.files(system.file(package = 'LSTModis'), recursive = T, full.names = T,pattern = ".csv")

  csv.file.read <- read.csv(csv.file, header=T,stringsAsFactors = FALSE)

  #Extract and calculate the zonal statistics based on the argument 'func' provided
  #inside the shapefile boudnaries
  #For each year a new column gets added to the data frame containing the stats of pixels for that
  #region

  cat("Constructing Dataframe" ,"...", sep = "")
  Constr_df<-function(ras){

    for(k in 1:nrow(csv.file.read)){
      if(csv.file.read[k,2]==1){

        if (requireNamespace("raster", quietly = TRUE)) {
          df[[paste0("pixels", ".", csv.file.read[k,1])]] <-
            c(raster::extract(ras, shape, fun = methods::getFunction(csv.file.read[k,1]), na.rm = TRUE))
        }

      }
    }

    name<-substr(names(ras),nchar(names(ras))-7,nchar(names(ras)))
    df$day_no<-name

    return(df)
  }

  dat<-lapply(ras1, Constr_df)
  cat("\nZonal statistics calculations complete\n")

  #set the working directory back to the original one
  setwd(initial.directory)

  #return the data frame with new columns containing zonal statistics for each year
  return(dat)
}

compute<-function(newData,aggregate){

  if(aggregate=="weekly"){
    cat(" Aggregating by week" ,"...", sep = "")
    days<-names(newData)
    years<-unique((substr(days,1,4)))
    pos<-list()
    ref<-list()
    dates<-list()
    weeks<-list()
    dat<-list()
    loop_fun<-function(year){
      pos[[year]]<- as.integer(substr(days[grep(year,days)],6,8))
      ref[[year]]<-paste(year,sep = "-","01-01")
      refs<-data.frame(ref)
      names(refs) = gsub(pattern = "X", replacement = "", x = names(refs))
      ref_date<-refs[1,c(year)]
      dates[[year]] <-as.Date(pos[[year]]-1, origin = as.character(ref_date))
      weeks[[year]]<-strftime(dates[[year]], format = "%V")
      day_list<-list()
      year<-year
      day_list<-pos[[year]]
      day_list<-stringr::str_pad(day_list, 3, pad = "0")
      day_list<-paste0(year,sep="_",day_list)
      week_list<-weeks[[year]]
      t<-paste0(day_list,sep=":",week_list)
      return(t)

    }

    dat<-lapply(years, loop_fun)
    mydat<-data.frame(unlist(dat))
    names(mydat)<-c("format")
    mydat$original<-substr(mydat$format,1,8)
    mydat$year<-substr(mydat$format,1,4)
    mydat$week<-paste0(mydat$year,sep="_",substr(mydat$format,10,12))
    split_frame_week<-split(mydat$format,mydat$week)

    tempfun_week<-function(l){
      w<-substr(l,10,12)
      y<-substr(l,1,4)
      w_y<-paste0(y,sep="_",w)
      days<-substr(l,1,8)
      temp<-data.frame(newData[,days])
      d<-data.frame()
      d<-data.frame(rowMeans(temp, na.rm=TRUE ))
      names(d)<-as.character(unique(w_y))
      return(d)
    }
    tempdat_week<-data.frame(lapply(split_frame_week,tempfun_week))
    names(tempdat_week) = gsub(pattern = "X", replacement = "", x = names(tempdat_week))
    return(tempdat_week)


  }else if(aggregate=="monthly"){
    cat(" Aggregating by month" ,"...", sep = "")
    days<-names(newData)
    years<-unique((substr(days,1,4)))
    pos<-list()
    ref<-list()
    dates<-list()
    months<-list()
    dat<-list()
    loop_fun<-function(year){
      pos[[year]]<- as.integer(substr(days[grep(year,days)],6,8))
      ref[[year]]<-paste(year,sep = "-","01-01")
      refs<-data.frame(ref)
      names(refs) = gsub(pattern = "X", replacement = "", x = names(refs))
      ref_date<-refs[1,c(year)]
      dates[[year]] <-as.Date(pos[[year]]-1, origin = as.character(ref_date))
      months[[year]]<-substr(dates[[year]],6,7)
      day_list<-list()
      day_list<-pos[[year]]
      day_list<-stringr::str_pad(day_list, 3, pad = "0")
      day_list<-paste0(year,sep="_",day_list)
      month_list<-months[[year]]
      t<-paste0(day_list,sep=":",month_list)
      return(t)
    }
    dat<-lapply(years, loop_fun)
    mydat<-data.frame(unlist(dat))
    names(mydat)<-c("format")
    mydat$original<-substr(mydat$format,1,8)
    mydat$year<-substr(mydat$format,1,4)
    mydat$month<-paste0(mydat$year,sep="_",substr(mydat$format,10,12))
    split_frame_month<-split(mydat$format,mydat$month)
    tempfun_month<-function(l){
      m<-substr(l,10,12)
      y<-substr(l,1,4)
      m_y<-paste0(y,sep="_",m)
      days<-substr(l,1,8)
      temp<-data.frame(newData[,days])
      d<-data.frame()
      d<-data.frame(rowMeans(temp, na.rm=TRUE ))
      names(d)<-as.character(unique(m_y))
      return(d)
    }
    tempdat_month<-data.frame(lapply(split_frame_month,tempfun_month))
    names(tempdat_month) = gsub(pattern = "X", replacement = "", x = names(tempdat_month))
    return(tempdat_month)

  }else if(aggregate=="yearly"){
    cat(" Aggregating by year" ,"...", sep = "")
    days<-names(newData)
    years<-substr(days,1,4)
    dat<-list()
    dat<-paste0(days,sep=":",years)

    mydat<-data.frame(unlist(dat))
    names(mydat)<-c("format")
    mydat$original<-substr(mydat$format,1,8)
    mydat$year<-substr(mydat$format,1,4)
    split_frame_year<-split(mydat$format,mydat$year)
    tempfun_year<-function(l){

      y<-substr(l,1,4)
      days<-substr(l,1,8)
      temp<-data.frame(newData[,days])
      d<-data.frame()
      d<-data.frame(rowMeans(temp, na.rm=TRUE ))
      names(d)<-as.character(unique(y))
      return(d)
    }
    tempdat_year<-data.frame(lapply(split_frame_year,tempfun_year))
    names(tempdat_year) = gsub(pattern = "X", replacement = "", x = names(tempdat_year))
    return(tempdat_year)

  }else if(aggregate=="daily"){
    cat(" Aggregating by date" ,"...", sep = "")
      return(newData)
  }



}


