#Instructions:-
  #Using MODIStsp()
  #Refer to the .json file created by the MODIStsp function to know the settings made in the GUI .
#' @title Download tif files
#' @description Function automates download of the .tif files (raster files).The defualt parameters include :-
#' Product: "Surf_Temp_Daily_005dg (M*D11C1)",
#' sensor: "Terra",
#' prod_version: "6",
#' out_format: "GTiff",
#' MODIStspVersion: "1.3.3.1",
#' timeseries_format: "ENVI Meta Files".
#' Original MODIS Layers: Daytime land surface temperature
#' Quality Indicators: Mandatory QA flag(day)
#' The script doesn't download the .hdf files.
#' @param username Character String; Username
#' @param password Character String; Password
#' @param start_date Character String; The start date
#' @param end_date Character String; The end date
#' @param option Numeric value; Takes value 1 or 2 : 1-USE default options
#'                                                   2-USE GUI for personalized options
#' @param path_files Character String; Path where the user wants tif files to be stored
#' @examples pwd<-getwd()
#' @examples Download_tif(username="abc",password="**",
#'                            start_date="2017-12-19",
#'                            end_date="2017-12-21",
#'                            option=2,
#'                            path_files=pwd)
#'@import jsonlite
#'@import stringr
#'@import MODIStsp
#'@import sp
#'@import xml2
#'@export Download_tif
#'@importFrom raster crop extract shapefile
#'@importFrom utils read.csv
Download_tif<-function(username="abc",password="**",start_date,end_date,option=1,path_files){



  #If the user inputs no username/password
  if(is.null(username)||is.null(password)){

    cat("please register on the website https://lpdaac.usgs.gov/")

  }else{

    if(option==1){

      #options_path is the path where is the .json file stored
      options_file<- list.files(system.file(package = 'LSTModis'), recursive = T, full.names = T,pattern = ".json")

      #Writing the start_date and end_date into the .json file previously obtained.

      txt<-readChar(options_file, file.info(options_file)$size)

      pos_start <-regexpr('"start_date\": ', txt)
      if(pos_start[1]==-1){
        pos_start <-regexpr('\"start_date\":', txt)
        replace_what_start<-substr(txt,pos_start[1],pos_start[1]+26)
        replace_with_start<-paste0('\"start_date\":[',sep= '\"',start_date)
        replace_with_start<-paste0(replace_with_start,sep= '\"',"]")
      }else{
        replace_what_start<-substr(txt,pos_start[1],pos_start[1]+24)
        replace_with_start<-paste0('\"start_date\": ',sep= '\"',start_date)

      }

      txt<-gsub(replace_what_start, replacement = replace_with_start, txt,fixed = TRUE)

      pos_end <-regexpr('"end_date\": ', txt)
      if(pos_end[1]==-1){
        pos_end <-regexpr('"end_date\":', txt)
        replace_what_end<-substr(txt,pos_end[1],pos_end[1]+22)
        replace_with_end<-paste0('\"end_date\":[',sep= '\"',end_date)
      }else{
        replace_what_end<-substr(txt,pos_end[1],pos_end[1]+22)
        replace_with_end<-paste0('\"end_date\": ',sep= '\"',end_date)
      }

      txt<-gsub(replace_what_end, replacement = replace_with_end, txt,fixed = TRUE)


      pos_out_folder <-regexpr('"out_folder": ', txt)
      if(pos_out_folder[1]==-1){
        pos_out_folder <-regexpr('"out_folder":["', txt,fixed = TRUE)
        i<-1
        while(1){
          s<-substr(txt,pos_out_folder[1],pos_out_folder[1]+i)

          var <-grepl(',', s)
          if(var==TRUE){
            break()
          }
          i<-i+1
        }


        replace_what_out_folder<-substr(txt,pos_out_folder[1],pos_out_folder[1]+i)
        replace_with_out_folder<-paste0('\"out_folder":["',sep= '',path_files)
        replace_with_out_folder<-paste0(replace_with_out_folder,sep= '\"',"],")

      }else{

        pos_out_folder <-regexpr('"out_folder": ', txt,fixed = TRUE)
        i<-1
        while(1){
          s<-substr(txt,pos_out_folder[1],pos_out_folder[1]+i)

          var <-grepl(',', s)
          if(var==TRUE){
            break()
          }
          i<-i+1
        }


        replace_what_out_folder<-substr(txt,pos_out_folder[1],pos_out_folder[1]+i)
        replace_with_out_folder<-paste0('"out_folder": ',sep= '\"',path_files)
        replace_with_out_folder<-paste0(replace_with_out_folder,sep= '\"',",")

      }

      txt<-gsub(replace_what_out_folder, replacement = replace_with_out_folder, txt,fixed = TRUE)


      pos_out_folder_mod <-regexpr('"out_folder_mod": ', txt)
      if(pos_out_folder_mod[1]==-1){
        pos_out_folder_mod <-regexpr('"out_folder_mod":["', txt,fixed = TRUE)
        i<-1
        while(1){
          s<-substr(txt,pos_out_folder_mod[1],pos_out_folder_mod[1]+i)

          var <-grepl(',', s)
          if(var==TRUE){
            break()
          }
          i<-i+1
        }


        replace_what_out_folder_mod<-substr(txt,pos_out_folder_mod[1],pos_out_folder_mod[1]+i)
        replace_with_out_folder_mod<-paste0('\"out_folder_mod":["',sep= '',path_files)
        replace_with_out_folder_mod<-paste0(replace_with_out_folder_mod,sep= '\"',"],")

      }else{

        pos_out_folder_mod <-regexpr('"out_folder_mod": ', txt,fixed = TRUE)
        i<-1
        while(1){
          s<-substr(txt,pos_out_folder_mod[1],pos_out_folder_mod[1]+i)

          var <-grepl(',', s)
          if(var==TRUE){
            break()
          }
          i<-i+1
        }


        replace_what_out_folder_mod<-substr(txt,pos_out_folder_mod[1],pos_out_folder_mod[1]+i)
        replace_with_out_folder_mod<-paste0('"out_folder_mod": ',sep= '\"',path_files)
        replace_with_out_folder_mod<-paste0(replace_with_out_folder_mod,sep= '\"',",")

      }

      txt<-gsub(replace_what_out_folder_mod, replacement = replace_with_out_folder_mod, txt,fixed = TRUE)



      pos_user <-regexpr('"user": ', txt)
      if(pos_user[1]==-1){
        pos_user <-regexpr('"user":["', txt,fixed = TRUE)
        i<-1
        while(1){
          s<-substr(txt,pos_user[1],pos_user[1]+i)

          var <-grepl(',', s)
          if(var==TRUE){
            break()
          }
          i<-i+1
        }


        replace_what_user<-substr(txt,pos_user[1],pos_user[1]+i)
        replace_with_user<-paste0('\"user":["',sep= '',username)
        replace_with_user<-paste0(replace_with_user,sep= '\"',"],")

      }else{

        pos_user <-regexpr('"user": ', txt,fixed = TRUE)
        i<-1
        while(1){
          s<-substr(txt,pos_user[1],pos_user[1]+i)

          var <-grepl(',', s)
          if(var==TRUE){
            break()
          }
          i<-i+1
        }


        replace_what_user<-substr(txt,pos_user[1],pos_user[1]+i)
        replace_with_user<-paste0('"user": ',sep= '\"',username)
        replace_with_user<-paste0(replace_with_user,sep= '\"',",")

      }

      txt<-gsub(replace_what_user, replacement = replace_with_user, txt,fixed = TRUE)


      pos_password <-regexpr('"password": ', txt)
      if(pos_password[1]==-1){
        pos_password <-regexpr('"password":["', txt,fixed = TRUE)
        i<-1
        while(1){
          s<-substr(txt,pos_password[1],pos_password[1]+i)

          var <-grepl(',', s)
          if(var==TRUE){
            break()
          }
          i<-i+1
        }


        replace_what_password<-substr(txt,pos_password[1],pos_password[1]+i)
        replace_with_password<-paste0('\"password":["',sep= '',password)
        replace_with_password<-paste0(replace_with_password,sep= '\"',"],")

      }else{

        pos_password <-regexpr('"password": ', txt,fixed = TRUE)
        i<-1
        while(1){
          s<-substr(txt,pos_password[1],pos_password[1]+i)

          var <-grepl(',', s)
          if(var==TRUE){
            break()
          }
          i<-i+1
        }


        replace_what_password<-substr(txt,pos_password[1],pos_password[1]+i)
        replace_with_password<-paste0('"password": ',sep= '\"',password)
        replace_with_password<-paste0(replace_with_password,sep= '\"',",")

      }

      txt<-gsub(replace_what_password, replacement = replace_with_password, txt,fixed = TRUE)



      if (requireNamespace("jsonlite", quietly = TRUE)) {
        t<-jsonlite::fromJSON(txt = txt)
        temp<-jsonlite::toJSON(t)
      }


      write(temp, options_file)

      # Feeding the .json file into MODIStsp()
      # Will create the folder Surf_Temp_Daily_005g_v6 and LST_Day_CMG with the .tif files


      if (requireNamespace("MODIStsp", quietly = TRUE)) {
        MODIStsp::MODIStsp(gui = FALSE,options_file = options_file)

      }

    }else if(option==2){


      # MODIStsp() generates a (i) .json file in the current working directory
      #                       (ii) creates folder Surf_Temp_Daily_005g_v6 and the .tif files
      if (requireNamespace("MODIStsp", quietly = TRUE)) {
        MODIStsp::MODIStsp()

      }


    }


  }



}

