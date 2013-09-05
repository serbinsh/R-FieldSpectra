#--------------------------------------------------------------------------------------------------#
# A set of helper functions
#--------------------------------------------------------------------------------------------------#


# TODO: add difference functions, concatenate function, smooth function, etc.


#--------------------------------------------------------------------------------------------------#
##'
##' Read settings file for spectra import and processing
##' 
##' @name settings
##' @title parse settings file used for spectra file import and processing
##' @param input.file settings file containing information needed for spectra processing
##'
##' @examples
##' \dontrun{
##' settings <- settings()
##' settings <- settings('/home/$USER/settings.xml')
##' }
##'
##' @export
##'
##' @author Shawn P. Serbin
##'
settings <- function(input.file=NULL){
  settings.xml <- NULL
  
  ### Parse input settings file
  if (!is.null(input.file) && file.exists(input.file)) {
    settings.xml <- xmlParse(input.file)  
    # convert the xml to a list
    settings.list <- xmlToList(settings.xml)
  
  } else {
    print("***** WARNING: no settings file defined *****")
  }
  
  # make sure something was loaded
  if (is.null(settings.xml)) {
    #log.error("Did not find any settings file to load.")
    stop("Did not find any settings file to load.")
  }
  
  ### Remove comment or NULL fields
  settings.list <- settings.list[settings.list !="NULL" ]
  
  # Return settings file as a list
  #invisible(settings.list) # invisible option
  return(settings.list)
  
} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name extract.metadata
##' @title Extract metadata for each spectra sample.  Works on a single spectra or a directory.
##' 
##' @param file.dir File directory or filename of single spectra for processing
##' @param out.dir Output directory for meta-data information file
##' @param instrument What instrument was used to collect spectra.  Options: ASD, SE
##' @param in.file.ext [Optional] Input file extension. E.g. .asd (ASD) or .sed (Spectral Evolution).
##' Default for ASD instruments is .asd.  Default for Spectral Evolution instruments is .sed
##' @param output.file.ext [Optional] Output file extension of meta-data information file. Default .csv
##' @param settings.file [Optional] Spectral settings file
##' 
##' @export
##' 
##' @author Shawn P. Serbin
##' 
extract.metadata <- function(file.dir=NULL,out.dir=NULL,instrument=NULL,in.file.ext=NULL,
                             output.file.ext=".csv",settings.file=NULL){
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  #
  if (is.null(settings.file) && is.null(file.dir)){
    print("*********************************************************************************")
    stop("******* ERROR: No input file or directory given in settings file or function call. *******")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (!is.null(settings.file$spec.dir)){
    file.dir <- settings.file$spec.dir
  } 
  
  # Run appropriate function for meta-data extraction
  do.call(paste("extract.metadata",tolower(instrument),sep="."),args = list(file.dir,out.dir,
                                                                            in.file.ext,
                                                                            output.file.ext))


}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name extract.metadata.asd
##' @title Extract metadata from raw binary ASD files 
##' 
##' @author Shawn P. Serbin
##' 
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name extract.metadata.se
##' @title Extract metadata from Spectral Evolution files 
##' 
##' @author Shawn P. Serbin
##' 
extract.metadata.se <- function(file.dir,out.dir,in.file.ext,output.file.ext){
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  # Check for custom output file extension
  if (is.null(in.file.ext)) {
    in.file.ext <- ".sed"
  } else {
    in.file.ext <- in.file.ext
  }
  
  print("Processing file(s)")
  
  out.head <- c("File_Name","Instrument","Detectors","Measurement", "Date", "Time", "Temperature",
                "Battery_Voltage","Averages","Integration","Dark_Mode","Radiometric_Calibration")
  
  # Determine if running on single file or directory
  check <- file.info(file.dir)
  if (check$isdir) {
    se.files.names <- list.files(path=file.dir,pattern=in.file.ext,full.names=FALSE)
    se.files.names <- unlist(strsplit(se.files.names,".sed"))
    se.files <- list.files(path=file.dir,pattern=in.file.ext,full.names=TRUE)
    out.file.name <- "Spectra"
    
  } else {
    se.files <- file.dir
    out.file.name <- unlist(strsplit(file.dir,dlm))
    out.file.name <- out.file.name[length(out.file.name)]                
    out.file.name <- unlist(strsplit(out.file.name,".sed"))
    se.files.names <- unlist(strsplit(out.file.name,".sed"))
  }

  # Build empty metadata dataframe
  inst <- rep(NA,length(se.files));detec <- rep(NA,length(se.files));meas <- rep(NA,length(se.files))
  date <- rep(NA,length(se.files));time <- rep(NA,length(se.files));temp <- rep(NA,length(se.files))
  batt <- rep(NA,length(se.files));avg <- rep(NA,length(se.files));int <- rep(NA,length(se.files))
  dm <- rep(NA,length(se.files)); radcal <- rep(NA,length(se.files))
  
  # Run metadata extraction
  for (i in 1:length(se.files)){
    data.line <- strsplit(system(paste("grep -n","Data", se.files[i]),intern=TRUE)[2],":")[[1]]
    data.line <- as.numeric(data.line[1])
    file.head <- readLines(se.files[i],n=data.line-1)

    inst[i] <- gsub(" ","",(strsplit(file.head[4],":")[[1]])[2])
    detec[i] <- gsub(" ","",(strsplit(file.head[5],":")[[1]])[2])
    meas[i] <- gsub(" ","",(strsplit(file.head[6],":")[[1]])[2])
    date[i] <- gsub(" ","",(strsplit(file.head[7],":")[[1]])[2])
    time[i] <- gsub(" ","",(strsplit(file.head[8],"Time:")[[1]])[2])
    temp[i] <- gsub(" ","",(strsplit(file.head[9],":")[[1]])[2])
    batt[i] <- gsub(" ","",(strsplit(file.head[10],":")[[1]])[2])
    avg[i] <- gsub(" ","",(strsplit(file.head[11],":")[[1]])[2])
    int[i] <- gsub(" ","",(strsplit(file.head[12],":")[[1]])[2])
    dm[i] <- gsub(" ","",(strsplit(file.head[13],":")[[1]])[2])
    radcal[i] <- gsub(" ","",(strsplit(file.head[15],":")[[1]])[2])
    rm(data.line,file.head)
  }
  
  # Create output
  out.metadata <- data.frame(se.files.names,inst,detec,meas,date,time,temp,batt,avg,int,dm,radcal)
  names(out.metadata) <- out.head
  
  write.csv(out.metadata,paste(out.dir,"/",out.file.name,".metadata",output.file.ext,sep=""),
            row.names=FALSE)
  
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name concat.spectra
##' @title Concatenate a directory of spectra files into a single .csv file. Works on a single 
##' directory or a series of directories
##' 
##' @param file.dir directory of spectra files to process.  Currently works on spectra files
##' formatted where each row is a wavelength and associated spectral observation.
##' @param out.dir output directory for concatenated spectra files. If not set then the output
##' directory defaults to file.dir (input directory)
##' @param out.filename filename for concatenated spectra files
##' @param in.file.ext file extension for individual spectra files. Defaults to ".csv"
##' @param out.file.ext option to set the output extension. Defaults to ".csv"
##' @param transpose option to transpose the spectra to/from row/column major output.
##' This will be applied to both the returned dataframe (optional) and output file. TRUE/FALSE. Default FALSE
##' @param spec.dataframe option toreturn a R dataframe containing the concatenated 
##' spectral data.  TRUE/FALSE
##' 
##' @examples
##' \dontrun{
##' concat.spectra()
##' spectra <- concat.spectra(file.dir=file.dir,out.dir=out.dir,out.filename="concat.spectra",
##' out.file.ext=".csv",transpose=FALSE,spec.dataframe=FALSE)
##' }
##' 
##' @export
##' 
##' @author Shawn P. Serbin
##' 
concat.spectra <- function(file.dir=NULL,out.dir=NULL,out.filename=NULL,in.file.ext=".csv",
                           out.file.ext=".csv",transpose=FALSE,spec.dataframe=FALSE){
  
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  ### Check for proper input
  if (is.null(file.dir)){
    print("*********************************************************************************")
    stop("******* ERROR: No input file directory given.  Please correct. *******")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  }
  
  ### Check for output directory
  if (is.null(out.dir)){
    out.dir <- file.dir
  } else {
    out.dir <- out.dir
  }
  if (!file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
  
  ### Check for output filename
  if (!is.null(out.filename)){
    out.filename <- out.filename
  } else {
    out.filename <- "Concat_Spectra"
  }
  ### Remove existing file if present
  unlink(paste(out.dir,out.filename,out.file.ext,sep=""),recursive=FALSE,force=TRUE)
  
  ### Get file list
  in.files <- list.files(path=file.dir,pattern=in.file.ext,full.names=FALSE)
  in.files.full <- list.files(path=file.dir,pattern=in.file.ext,full.names=TRUE)
  num.files <- length(in.files)
  spec.names <- unlist(strsplit(in.files,paste("\\",in.file.ext,sep="")))
  
  ### Stop if no files found
  if (num.files<1) stop("****** No files found in directory: ",file.dir," ******")
  
  ### Display info to the terminal
  tmp  <- unlist(strsplit(file.dir,dlm))
  current <- tmp[length(tmp)]
  print(paste("------- Processing directory: ",current))
  print(paste("------- Number of files: ",num.files))
  flush.console() #<--- show output in real-time
  
  spec.file <- read.csv(paste(file.dir,dlm,in.files[1],sep=""))
  in.waves <- spec.file[,1]
  dims <- dim(spec.file)
  if (dims[2]>dims[1]) stop("***** ERROR: Input spectral files are not properly formatted for this function *****")
  
  in.spec <- array(data=NA,dim=c(num.files,max(dims)))
  ### Read in files to concatenate
  for (i in 1:num.files){
    spec.file <- read.csv(paste(file.dir,dlm,in.files[i],sep=""))   
    in.spec[i,] <- t(spec.file[,2])
    rm(spec.file)
  }
  
  ### Concatenate
  # If transpose requested
  if (transpose){
    # Convert to Spectra,Wave_x, Wave_x
    if (dims[1]>dims[2]){
      out.spec <- array(data=NA,dim=c(num.files,max(dims)+1))
      out.spec <- as.data.frame(out.spec)
      out.spec[,1] <- spec.names
      dims <- dim(out.spec)
      out.spec[,2:dims[2]] <- as.data.frame(in.spec)
      names(out.spec) <- c("Spectra",paste("Wave_",in.waves,sep="")) 
    }
  } else {
    out.spec <- array(data=NA,dim=c(max(dims),num.files+1))
    out.spec[,1] <- in.waves
    out.spec[,2:(num.files+1)] <- t(in.spec)
    out.spec <- as.data.frame(out.spec)
    names(out.spec) <- c("Wavelength",spec.names)
  }
  
  ### Output concatenated data
  write.csv(out.spec,paste(out.dir,dlm,out.filename,out.file.ext,sep=""),row.names=FALSE)
  
  ### Output dataframe if requested
  if (spec.dataframe){
    return(out.spec)
  }
  
} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name transpose.spectra
##' @title Transpose spectra file(s) from/to row or column major format.  Works with either a 
##' single spectrum or directory of spectral observations.
##' 
##' @param file.dir directory of spectra files to process
##' @param input.file name of input spectra file. If ommitted then the function transposes
##' all files in the input file.dir
##' @param out.dir output directory for transposed spectra file(s)
##' @param out.filename output filename of transposed spectra file(s). If using a directory then
##' the original filenames are appended with the .t suffix
##' @param in.file.ext file extension for individual spectra files. Defaults to ".csv"
##' @param out.file.ext option to set the output extension. Defaults to ".csv"
##' @param header TRUE/FALSE. Do/does the file(s) have a header line?
##' 
##' @author Shawn P. Serbin
##' 
transpose.spectra <- function(file.dir=NULL,input.file=NULL,out.dir=NULL,out.filename=NULL,
                              in.file.ext=".csv",out.file.ext=".csv",header=FALSE){
  print("*** NOT YET IMPLEMENTED ***")
} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name smooth.spectra
##' @title Smooth spectra file with Savitsky-Golay smoothing filter.  Works with either a 
##' single spectrum or array of spectral observations in row-major format.
##' 
##' @param file.dir Directory of spectra files to apply sgolay smoothing filter.
##' @param input.file Spectra file to apply sgolay smoothing filter. If not set then all files in 
##' file.dir are processed.
##' @param out.dir Output directory for smoothed spectra files. If not set then processed
##' spectra will be output to file.dir
##' @param out.filename Output filename for processed spectra file.  If not set then the original 
##' filename(s) will be modified with the .sg suffix.
##' @param header Does the spectra file(s) have a header line? Default = TRUE
##' @param p SG filter order.  Default 1.
##' @param n SG filter length. Needs to be an odd value.  Default 21
##' @param length Apply sgolay smoothing filter to the entire (default=full) or subset of 
##' the spectrum.  Define subset limits as (wavelength1,wavelength2), e.g. (2000,2500) 
##' in nanometers [NOT YET IMPLEMENTED]
##' @param file.ext File extension for spectra files.  Default '.csv'
##' 
##' @export
##' 
##' @examples
##' \dontrun{
##' smooth.spectra()
##' smooth.spectra(file.dir=file.dir,input.file=input.file,out.dir=out.dir,out.filename=out.filename,
##'                 header=TRUE,p=1,n=21,length=full)
##' }
##' 
##' @author Shawn P. Serbin
##' 
smooth.spectra <- function(file.dir=NULL,input.file=NULL,out.dir=NULL,out.filename=NULL, 
                           header=TRUE,p=NULL,n=NULL,length='full',file.ext='.csv'){
  
  # TODO: Update with ability to transpose spectra on the fly to work with this filter
  # TODO: Enable checking of whether input is a single file or a directory of files (Done)
  # TODO: Automatic generation of output filenames with the .sg suffix indicating SG filter spec
  # TODO: Enable smoothing of either entire or subset of spectra
  # TODO: Allow for generation of wavelength info on the fly bases on start/end wavelengths and 
  # wavelength steps
  # TODO: Allow for adding a header to output file(s).  Something like "Wavelength", "Spectra"
  
  ok = require(signal) ; if (! ok) 
    stop("*** Package signal is not available.  This package is needed for running the smooth.spectra() function ***")
  #sgolayfilt(x, p = 3, n = p + 3 - p%%2, m = 0, ts = 1)
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  # create output directory if it doesn't already exist
  if (!is.null(out.dir)){
    if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
  }
  
  if (is.null(out.dir)){
    out.dir <- file.dir
  }
  
  # Set defaults if sgfilt settings are not specified.
  if (is.null(p)){
    p <- 1
  }
  
  if (is.null(n)){
    n <- 21
  }
  
  # Read in files without header
  if (header==FALSE){
    if (!is.null(input.file)){
      if (is.null(out.filename)){
        print("")
        stop("***** Please specify output filename *****")
        print("")
      } else {
        spectra <- input.file[,2] # temp.Need to enable on-the-fly transpose
        spectra2 <- sgolayfilt(spectra,p=p,n=n)
        waves <- input.file[,1]
      }

    } else {
      # Display info to the terminal
      tmp  = unlist(strsplit(file.dir,dlm))
      current = tmp[length(tmp)]
      print(paste("------- Processing directory: ",current))
      flush.console() #<--- show output in real-time    
      
      #--------------------- Setup -----------------------#
      ascii.files  = list.files(path=file.dir,pattern=file.ext,full.names=FALSE)
      num.files  = length(ascii.files)
      print(num.files)
      
      #-------------------------- Start loop --------------------------#
      j <- 1 # <--- Numeric counter for progress bar
      pb <- txtProgressBar(min = 0, max = num.files, char="*",width=70,style = 3)
      for (i in 1:num.files){
        tmp <- unlist(strsplit(ascii.files[i],paste("\\",file.ext,sep="")))  # <--- remove file extension from file name
        spec.file <- read.csv(paste(file.dir,dlm,ascii.files[i],sep=""))
        waves <- spec.file[,1]
        spectra <- spec.file[,2]
        spectra2 <- sgolayfilt(spectra,p=p,n=n)
      } # End for loop
    }
  } # End of if/else

  # Read in files with a header line
  if (header){
    print(" NOT YET IMPLEMENTED ")
    
  } # End if/else
  
  # Create output object
  out.spec <- data.frame(waves,spectra2)
  write.csv(out.spec,paste(out.dir,dlm,out.filename,file.ext,sep=""),row.names=FALSE)
  #return(spectra2)
  
} # End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################