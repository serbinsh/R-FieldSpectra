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
##' @param out.dir Output directory for metadata information file
##' @param instrument What instrument was used to collect spectra.  Current options: ASD, SE
##' @param spec.file.ext [Optional] Input spectra file extension. E.g. .asd (ASD) or .sed (Spectral Evolution).
##' Default for ASD instruments is .asd.  Default for Spectral Evolution instruments is .sed
##' @param output.file.ext [Optional] Output file extension of metadata information file. Default .csv
##' @param tz [Optional] Set the timezone of the spectra file collection.  Used to covert spectra collection 
##' time to UTC.  If unused it is assumed that the correct timezone is the current system timezone.
##' @param intern logical. [Optional] Keep meta-data output as an internal object (TRUE) or write to file (FALSE)
##' @param settings.file [Optional] Spectral settings file
##' 
##' @export
##' 
##' @examples
##' # ASD
##' file <- system.file("extdata/PM01_TIAM_B_LC_REFL00005.asd",package="FieldSpec")
##' extract.metadata(file,instrument="ASD",intern=TRUE)
##' 
##' # Spectral Evolution
##' file <- system.file("extdata/cvars_grape_leaf1_lc_rg_01236.sed",package="FieldSpec")
##' extract.metadata(file,instrument="SE",intern=TRUE)
##' 
##' @author Shawn P. Serbin
##' 
extract.metadata <- function(file.dir=NULL,out.dir=NULL,instrument=NULL,spec.file.ext=NULL,
                             output.file.ext=".csv",tz=NULL,intern=FALSE,settings.file=NULL){
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  # Input directory
  if (is.null(settings.file) && is.null(file.dir)){
    print("*********************************************************************************")
    stop("ERROR: No input file or directory given in settings file or function call.")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (!is.null(settings.file$spec.dir)){
    file.dir <- settings.file$spec.dir
  }
  
  # Output directory
  if (is.null(settings.file) && is.null(out.dir) && intern==FALSE) { 
    print("*********************************************************************************")
    stop("ERROR: No output directory given in settings file or function call.")          
  } else if (!is.null(out.dir)){
    out.dir <- out.dir
  } else if (!is.null(settings.file$output.dir)){
    out.dir <- settings.file$output.dir
  }
  
  # Instrument
  if (is.null(settings.file) && is.null(instrument) && is.null(spec.file.ext)){ 
    print("*********************************************************************************")
    stop("ERROR: No instrument defined in settings file or function call.")
  } else if (!is.null(instrument)){
    instrument <- instrument
  } else if (!is.null(settings.file$instrument$name)){
    inst <- c("ASD","ASD","ASD","SE","SE","SE")
    temp <- tolower(settings.file$instrument$name)
    index <- pmatch(temp,c("asd","fieldspec","fieldspec 3","se","spectral evolution","evolution"))
    instrument <- inst[index]
  } else if (spec.file.ext==".asd") {
    instrument <- "ASD"
  } else if (spec.file.ext==".sed") {
    instrument <- "SE"
  }
  
  # Input file extension
  if (is.null(settings.file) && is.null(spec.file.ext)){ 
    print("*********************************************************************************")
    if(instrument=="ASD") (spec.file.ext=".asd")
    if(instrument=="SE") (spec.file.ext=".sed")
    warning("WARNING: No input file extension defined in settings file or function call.")
    warning(paste0("WARNING: Using default: ", spec.file.ext) )
  } else if (!is.null(spec.file.ext)){
    spec.file.ext <- spec.file.ext
  } else if (!is.null(settings.file$options$spec.file.ext)){
    spec.file.ext <- settings.file$options$spec.file.ext
  }
  
  # Custom output file extension
  if (!is.null(settings.file$options$output.file.ext)){
    output.file.ext <- settings.file$options$output.file.ext
  }
  
  # Run appropriate function for meta-data extraction
  do.call(paste("extract.metadata",tolower(instrument),sep="."),args = list(file.dir=file.dir,out.dir=out.dir,
                                                                            spec.file.ext=spec.file.ext,
                                                                            output.file.ext=output.file.ext,
                                                                            tz=tz,intern=intern))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name extract.metadata.asd
##' @title Extract metadata from raw binary ASD files. Called from extract.metadata
##' @param file.dir File directory or filename of single spectra for processing
##' @param out.dir Output directory for meta-data information file
##' @param spec.file.ext [Optional] Input spectra file extension. E.g. .asd (ASD) or .sed (Spectral Evolution).
##' Default for ASD instruments is .asd.  Default for Spectral Evolution instruments is .sed
##' @param output.file.ext [Optional] Output file extension of meta-data information file. Default .csv
##' @param tz [Optional] Set the timezone of the spectra file collection.  Used to covert spectra collection 
##' time to UTC.  If unused it is assumed that the correct timezone is the current system timezone.
##' @param intern logical. [Optional] Keep meta-data output as an internal object (TRUE) or write to file (FALSE)
##' 
##' @author Shawn P. Serbin
##' 
extract.metadata.asd <- function(file.dir,out.dir,spec.file.ext,output.file.ext,tz,intern){
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  # Check for custom output file extension
  if (is.null(spec.file.ext)) {
    spec.file.ext <- ".asd"
  } else {
    spec.file.ext <- spec.file.ext
  }
  
  if (intern==FALSE){
    print("------- Processing file(s) -------")
  }

  # Determine if running on single file or directory
  check <- file.info(file.dir)
  if (check$isdir) {
    asd.files.names <- list.files(path=file.dir,pattern=spec.file.ext,full.names=FALSE)
    asd.files.names <- unlist(strsplit(asd.files.names,spec.file.ext))
    asd.files <- list.files(path=file.dir,pattern=spec.file.ext,full.names=TRUE)
    out.file.name <- "Spectra"
    
  } else {
    asd.files <- file.dir
    out.file.name <- unlist(strsplit(file.dir,dlm))
    out.file.name <- out.file.name[length(out.file.name)]                
    out.file.name <- unlist(strsplit(out.file.name,spec.file.ext))
    asd.files.names <- unlist(strsplit(out.file.name,spec.file.ext))
  }
  
  # Define locations in file header
  # Defined using Indico Version 8 File Format Standards (http://support.asdi.com/Document/Viewer.aspx?id=95)
  ## offset to get to structure
  offsets <- c(0,3,160,178,179,181,182,186,187,191,195,199,200,201,202,203,204,206,
               334,390,394,396,398,400,402,406,410,414,418,420,421,425,427,429,431,
               432,436,438,440,442,444,448,452)     
  ## size of structure
  info.size <- c(3,157,18,1,1,1,4,1,4,4,4,1,1,1,1,1,2,128,56,4,2,2,2,2,4,4,4,4,2,1,
                 4,2,2,2,1,4,2,2,2,2,4,4,27)          
  out.metadata <- data.frame(array(data=NA,dim=c(length(asd.files),length(offsets))))
  
  # Build empty metadata vars
  file.ver <- rep(NA,length(asd.files));comments <- rep(NA,length(asd.files));spec.dow <- rep(NA,length(asd.files))
  spec.doy <- rep(NA,length(asd.files));daylight.savings <- rep(NA,length(asd.files));
  #spec.date <- rep(as.POSIXct(paste(as.Date("1970-01-01"), "01:01:01",sep=" "),tz=tz),length(asd.files))
  spec.date <- rep(paste(as.Date("1970-01-01"), "01:01:01",sep=" "),length(asd.files))
  program.ver <- rep(NA,length(asd.files));spec.file.ver <- rep(NA,length(asd.files));dc.corr <- rep(NA,length(asd.files))
  dc.time <- rep(NA,length(asd.files));ref.time <- rep(NA,length(asd.files));data.type <- rep(NA,length(asd.files))
  ch1.wavelength <- rep(NA,length(asd.files));wavelength.step <- rep(NA,length(asd.files));data.format <- rep(NA,length(asd.files))
  old.dc.count <- rep(NA,length(asd.files));old.ref.count <- rep(NA,length(asd.files)); old.sample.count <- rep(NA,length(asd.files))
  channels <- rep(NA,length(asd.files));int.time <- rep(NA,length(asd.files));foreoptic.deg <- rep(NA,length(asd.files))
  dcc.value <- rep(NA,length(asd.files));calib.series <- rep(NA,length(asd.files));inst.number <- rep(NA,length(asd.files))
  ymin <- rep(NA,length(asd.files));ymax <- rep(NA,length(asd.files));xmin <- rep(NA,length(asd.files));xmax <- rep(NA,length(asd.files))
  dyn.range <- rep(NA,length(asd.files));xmode <- rep(NA,length(asd.files));flags <- rep(NA,length(asd.files))
  dc.count <- rep(NA,length(asd.files));ref.count <- rep(NA,length(asd.files));sample.count <- rep(NA,length(asd.files))
  inst.type <- rep(NA,length(asd.files));bulb <- rep(NA,length(asd.files));swir1.gain <- rep(NA,length(asd.files))
  swir2.gain <- rep(NA,length(asd.files));swir1.offset <- rep(NA,length(asd.files));swir2.offset <- rep(NA,length(asd.files))
  splice1 <- rep(NA,length(asd.files));splice2 <- rep(NA,length(asd.files));ref.flag <- rep(NA,length(asd.files))
  
  ## Run metadata extraction
  for (i in 1:length(asd.files)){
    to.read <- file(asd.files[i],"rb")
    
    seek(to.read,where=offsets[1],origin="start",rw="r") # 0,3
    file.ver[i] <- readBin(to.read,what=character(),size=info.size[1],endian = .Platform$endian)
    # File Version - as6
    
    seek(to.read,where=offsets[2],origin="start",rw="r") #3,157
    comments[i] <- readBin(to.read,what=character(),size=info.size[2],endian = .Platform$endian)
    # comment field
    
    ### Spectra time/date collection info chunk -------------------------------------------------
    seek(to.read,where=offsets[3],origin="start",rw="r") # 160,2
    spec.sec <- readBin(to.read,what=integer(),size=2,endian = .Platform$endian)
    # seconds [0,59]
    if (nchar(spec.sec)<2){
      spec.sec <- paste(0,spec.sec,sep="")
    }
    
    seek(to.read,where=offsets[3]+2,origin="start",rw="r") # 162,2
    spec.min <- readBin(to.read,what=integer(),size=2,endian = .Platform$endian)
    # minutes [0,59]
    if (nchar(spec.min)<2){
      spec.min <- paste(0,spec.min,sep="")
    }
    
    seek(to.read,where=offsets[3]+4,origin="start",rw="r") # 164,2
    spec.hour <- readBin(to.read,what=integer(),size=2,endian = .Platform$endian)
    # hour [0,23]
    if (nchar(spec.hour)<2){
      spec.hour <- paste(0,spec.hour,sep="")
    }
    
    seek(to.read,where=offsets[3]+6,origin="start",rw="r") # 166,2
    spec.day <- readBin(to.read,what=integer(),size=2,endian = .Platform$endian)
    # day of month [1,31]
    
    seek(to.read,where=offsets[3]+8,origin="start",rw="r") # 168,2
    spec.month <- (readBin(to.read,what=integer(),size=2,endian = .Platform$endian))+1
    # Month of year [0-11], thus 0=Jan, 11=Dec
    
    seek(to.read,where=offsets[3]+10,origin="start",rw="r") # 170,2
    spec.year <- (readBin(to.read,what=integer(),size=2,endian = .Platform$endian))+1900
    # years since 1900.  example: 113 --> 1900+113=2013
    
    seek(to.read,where=offsets[3]+12,origin="start",rw="r") # 172,2
    spec.dow[i] <- readBin(to.read,what=integer(),size=2,endian = .Platform$endian)
    # day of week [0,6] (Sunday = 0). 0 Sun, 1 Mon, 2 Tues, 3 Wed, 4 Thurs, 5 Fri, 6 Sat
    
    seek(to.read,where=offsets[3]+14,origin="start",rw="r") # 174,2
    spec.doy[i] <- readBin(to.read,what=integer(),size=2,endian = .Platform$endian)
    # day of year [0,365]
    
    seek(to.read,where=offsets[3]+16,origin="start",rw="r") # 176,2
    daylight.savings[i] <- readBin(to.read,what=integer(),size=2,endian = .Platform$endian)
    # daylight savings flag. 1 TRUE 0 FALSE
    
    # Convert to date/time string
    date.temp <- paste(as.Date(paste(spec.year,spec.month,spec.day,sep="-")),
                       paste(spec.hour,spec.min,spec.sec,sep=":"))
    spec.date[i] <- date.temp
    # move this down
    #if (!is.null(tz)){
    #  spec.date[i] <- as.POSIXct(date.temp,tz=tz)
    #} else {
    #  spec.date[i] <- as.POSIXct(date.temp)
    #}
    #spec.date[i] <- format(spec.date[i],tz="UTC",usetz=TRUE)
    
    rm(spec.sec,spec.min,spec.hour,spec.day,spec.month,spec.year,date.temp)
    ### ----------------------------------------------------------------------------------------
        
    seek(to.read,where=offsets[4],origin="start",rw="r") # 178,1
    program.ver[i] <- as.character(readBin(to.read,what=raw(),size=info.size[4],endian = .Platform$endian))
    # ver. of the program creating this file. major ver in upper nibble, min in lower 
    
    seek(to.read,where=offsets[5],origin="start",rw="r") # 179,1
    spec.file.ver[i] <- as.character(readBin(to.read,what=raw(),size=info.size[5],endian = .Platform$endian))
    # spectrum file format version
    
    seek(to.read,where=offsets[6],origin="start",rw="r") # 181, 1
    dc.corr[i] <- as.character(readBin(to.read,what=raw(),size=info.size[6],endian = .Platform$endian))
    if (dc.corr[i]=="01"){
      dc.corr[i] <- "yes"
    } else {
      dc.corr[i] <- "no"
    }
    # 1 if DC subtracted, 0 if not
    
    seek(to.read,where=offsets[7],origin="start",rw="r") # 182,4
    dc.time[i] <- readBin(to.read,what=integer(),size=info.size[7],endian = .Platform$endian)
    # seconds since 1/1/1970
    #dc.time <- as.Date(as.POSIXct(dc.time, origin="1970-01-01", tz = "GMT")) #just date
    #dc.time <- as.POSIXct(dc.time, origin="1970-01-01", tz = "UTC") # date and time --> moved down
    # Time of last dc, seconds since 1/1/1970
    
    seek(to.read,where=offsets[8],origin="start",rw="r") # 186,1
    data.type.temp <- readBin(to.read,what=raw(),size=info.size[8],endian = .Platform$endian)
    if (data.type.temp==0) {
      data.type[i] <- "raw_type"
    } else if (data.type.temp==1) {
      data.type[i] <- "refl_type"
    } else if (data.type.temp==2) {
      data.type[i] <- "rad_type"
    } else if (data.type.temp==3) {
      data.type[i] <- "nounits_type"
    } else if (data.type.temp==4) {
      data.type[i] <- "nounits_type"
    } else if (data.type.temp==5) {
      data.type[i] <- "qi_type"
    } else if (data.type.temp==6) {
      data.type[i] <- "trans_type"
    } else if (data.type.temp==7) {
      data.type[i] <- "unknown_type"
    } else if (data.type.temp==8) {
      data.type[i] <- "abs_type"
    }
    # Spectra data type
    rm(data.type.temp)
    
    seek(to.read,where=offsets[9],origin="start",rw="r") # 187,4
    ref.time[i] <- readBin(to.read,what=integer(),size=info.size[9],endian = .Platform$endian)
    # seconds since 1/1/1970 of last white reference
    #wref.time <- as.POSIXct(ref.time, origin="1970-01-01", tz = "UTC") # date and time --> moved down
    # Time of last white reference, seconds since 1/1/1970
    
    seek(to.read,where=offsets[10],origin="start",rw="r") # 191,4
    ch1.wavelength[i] <- readBin(to.read,what=numeric(),size=info.size[10],endian = .Platform$endian)
    # calibrated starting wavelength in nm
    
    seek(to.read,where=offsets[11],origin="start",rw="r") # 195,4
    wavelength.step[i] <- readBin(to.read,what=numeric(),size=info.size[11],endian = .Platform$endian)
    # calibrated wavelength step in nm
    
    seek(to.read,where=offsets[12],origin="start",rw="r") # 199,1
    data.format.temp <- readBin(to.read,what=raw(),size=info.size[12],endian = .Platform$endian)
    if (data.format.temp==0){
      data.format[i] <- "float"
    } else if (data.format.temp==1){
      data.format[i] <- "integer"
    } else if (data.format.temp==2){
      data.format[i] <- "double"
    } else if (data.format.temp==3){
      data.format[i] <- "unknown"
    }
    # format of spectrum.
    rm(data.format.temp)
    
    seek(to.read,where=offsets[13],origin="start",rw="r") # 200,1
    old.dc.count[i] <- as.character(readBin(to.read,what=raw(),size=info.size[13],endian = .Platform$endian))
    # Number of dark current measurements in average. old version. unused in new file format
    # use check against new version.  if old >0 then file is an older ASD file format
    
    seek(to.read,where=offsets[14],origin="start",rw="r") # 201,1
    old.ref.count[i] <- as.character(readBin(to.read,what=raw(),size=info.size[14],endian = .Platform$endian))
    # Number of white ref measurements in average. old version. unused in new file format
    # use check against new version.  if old >0 then file is an older ASD file format
    
    seek(to.read,where=offsets[15],origin="start",rw="r") # 202,1
    old.sample.count[i] <- as.character(readBin(to.read,what=raw(),size=info.size[15],endian = .Platform$endian))
    # Num of spec samples in the avg. old version. unused in new file format
    # use check against new version.  if old >0 then file is an older ASD file format
    
    #seek(to.read,where=offsets[16],origin="start",rw="r") # 203,1
    #app <- readBin(to.read,what=integer(),size=info.size[16],endian = .Platform$endian)
    # Which application created APP_DATA
    
    seek(to.read,where=offsets[17],origin="start",rw="r") # 204, 2
    channels[i] <- readBin(to.read,what=integer(),size=info.size[17],endian = .Platform$endian)
    # Num of channels in the detector.
    
    #seek(to.read,where=offsets[18],origin="start",rw="r") # 206, 128
    #app.data <- readBin(to.read,what=integer(),size=info.size[18],endian = .Platform$endian)
    # Application-specific data
    
    #seek(to.read,where=offsets[19],origin="start",rw="r") # 334, 56
    #gps.data <- readBin(to.read,what=integer(),size=info.size[19],endian = .Platform$endian)
    # GPS position, course, etc.
    
    seek(to.read,where=offsets[20],origin="start",rw="r") # 390,4
    int.time[i] <- readBin(to.read,what=integer(),size=info.size[20],endian = .Platform$endian)
    # Actual integration time in ms
    
    seek(to.read,where=offsets[21],origin="start",rw="r") # 394,2
    foreoptic.deg[i] <- readBin(to.read,what=integer(),size=info.size[21],endian = .Platform$endian)
    # The fore optic attachment's view in degrees
    
    seek(to.read,where=offsets[22],origin="start",rw="r") # 396,2
    dcc.value[i] <- readBin(to.read,what=integer(),size=info.size[22],endian = .Platform$endian)
    # The dark current correction value
    
    seek(to.read,where=offsets[23],origin="start",rw="r") # 398,2
    calib.series[i] <- readBin(to.read,what=integer(),size=info.size[23],endian = .Platform$endian)
    # calibration series
    
    seek(to.read,where=offsets[24],origin="start",rw="r") # 400,2
    inst.number[i] <- readBin(to.read,what=integer(),size=info.size[24],endian = .Platform$endian)
    # instrument number. i.e. serial number of unit
    
    seek(to.read,where=offsets[25],origin="start",rw="r") # 402,4
    ymin[i] <- readBin(to.read,what=double(),size=info.size[25],endian = .Platform$endian)
    # setting of the y axis' min value
    
    seek(to.read,where=offsets[26],origin="start",rw="r") # 406,4
    ymax[i] <- readBin(to.read,what=double(),size=info.size[26],endian = .Platform$endian)
    # setting of the y axis' min value
    
    seek(to.read,where=offsets[27],origin="start",rw="r") # 410,4
    xmin[i] <- readBin(to.read,what=double(),size=info.size[27],endian = .Platform$endian)
    # setting of the x axis' min value
    
    seek(to.read,where=offsets[28],origin="start",rw="r") # 414,4
    xmax[i] <- readBin(to.read,what=double(),size=info.size[28],endian = .Platform$endian)
    # setting of the x axis' min value
    
    seek(to.read,where=offsets[29],origin="start",rw="r") # 418,2
    dyn.range[i] <- readBin(to.read,what=integer(),size=info.size[29],endian = .Platform$endian)
    # instrument's dynamic range;  ip_numbits; e.g. 16bit
    
    seek(to.read,where=offsets[30],origin="start",rw="r") # 420,1
    xmode[i] <- as.character(readBin(to.read,what=raw(),size=info.size[30],endian = .Platform$endian))
    # x axis mode. See *_XMODE
    
    seek(to.read,where=offsets[31],origin="start",rw="r") # 421,4
    flags[i] <- readBin(to.read,what=numeric(),size=info.size[31],endian = .Platform$endian)
    
    seek(to.read,where=offsets[32],origin="start",rw="r") # 425,2
    dc.count[i] <- readBin(to.read,what=integer(),size=info.size[32],endian = .Platform$endian)
    # number of dark current measurements in average. new version
    
    seek(to.read,where=offsets[33],origin="start",rw="r") # 427,2
    ref.count[i] <- readBin(to.read,what=integer(),size=info.size[33],endian = .Platform$endian)
    # Num of WR in the average 
    
    seek(to.read,where=offsets[34],origin="start",rw="r") # 429,2
    sample.count[i] <- readBin(to.read,what=integer(),size=info.size[34],endian = .Platform$endian)
    # Num of spec samples in the avg
    
    seek(to.read,where=offsets[35],origin="start",rw="r") # 431,1
    inst.type.temp <- readBin(to.read,what=raw(),size=info.size[35],endian = .Platform$endian)
    if (inst.type.temp==0){
      inst.type[i] <- "unknown_instrument"
    } else if (inst.type.temp==1) {
      inst.type[i] <- "PSII_instrument"
    } else if (inst.type.temp==2) {
      inst.type[i] <- "LSVNIR_instrument"
    } else if (inst.type.temp==3) {
      inst.type[i] <- "FSVNIR_instrument"
    } else if (inst.type.temp==4) {
      inst.type[i] <- "FSFR_instrument"
    } else if (inst.type.temp==5) {
      inst.type[i] <- "FSNIR_instrument"
    } else if (inst.type.temp==6) {
      inst.type[i] <- "CHEM_instrument"
    } else if (inst.type.temp==7) {
      inst.type[i] <- "FSFR_unattended_instrument"
    }
    rm(inst.type.temp)
    
    seek(to.read,where=offsets[36],origin="start",rw="r") # 432,4
    bulb[i] <- readBin(to.read,what=integer(),size=info.size[36],endian = .Platform$endian)
    # The id number of the cal bulb
    
    seek(to.read,where=offsets[37],origin="start",rw="r") # 436,2
    swir1.gain[i] <- readBin(to.read,what=integer(),size=info.size[37],endian = .Platform$endian)
    # gain setting for swir 1
    
    seek(to.read,where=offsets[38],origin="start",rw="r") # 438,2
    swir2.gain[i] <- readBin(to.read,what=integer(),size=info.size[38],endian = .Platform$endian)
    # gain setting for swir 2
    
    seek(to.read,where=offsets[39],origin="start",rw="r") # 440,2
    swir1.offset[i] <- readBin(to.read,what=integer(),size=info.size[39],endian = .Platform$endian)
    # offset setting for swir 1
    
    seek(to.read,where=offsets[40],origin="start",rw="r") # 442,2
    swir2.offset[i] <- readBin(to.read,what=integer(),size=info.size[40],endian = .Platform$endian)
    # offset setting for swir 2
    
    seek(to.read,where=offsets[41],origin="start",rw="r") # 444,4
    splice1[i] <- readBin(to.read,what=numeric(),size=info.size[41],endian = .Platform$endian)
    # wavelength of VNIR and SWIR1 splice
    
    seek(to.read,where=offsets[42],origin="start",rw="r") # 448,4
    splice2[i] <- readBin(to.read,what=numeric(),size=info.size[42],endian = .Platform$endian)
    # wavelength of SWIR1 and SWIR2 splice
    
    # ---------------------------------------------------------------------------------------
    ## SMART detector chunk 
    #seek(to.read,where=offsets[43],origin="start",rw="r")
    #serial.number <- readBin(to.read,what=integer(),size=4,endian = .Platform$endian)
    # 
    #seek(to.read,where=456,origin="start",rw="r")
    #readBin(to.read,what=numeric(),size=8,endian = .Platform$endian)
    # ---------------------------------------------------------------------------------------
        
    ## Attempt to gather info from reference file header
    spectrum.data.start <- 484 #<-- from Indico file definition
    spectrum.data.end <- spectrum.data.start+(length(seq(ch1.wavelength[i],ch1.wavelength[i]+channels[i]-1,
                                                         wavelength.step[i])))
    
    seek(to.read,where=spectrum.data.end+1,origin="start",rw="r")
    ref.flag[i] <- readBin(to.read,what=logical(),size=2,endian = .Platform$endian)
    # Reference been taken. Logical: TRUE/FALSE

    close(to.read)
    
  }
  
  # Convert date/times to proper output format
  dc.time <- as.POSIXct(dc.time, origin="1970-01-01", tz = "UTC") # date and time in UTC
  dc.time <- format(dc.time,tz="UTC",usetz=TRUE)
  wref.time <- as.POSIXct(ref.time, origin="1970-01-01", tz = "UTC") # date and time
  wref.time <- format(wref.time,tz="UTC",usetz=TRUE)
  
  if (!is.null(tz)){
    spec.date <- as.POSIXct(spec.date,tz=tz)
  } else {
    spec.date <- as.POSIXct(spec.date)
  }
  spec.date <- format(spec.date,tz="UTC",usetz=TRUE)
  
  # Create output
  out.head <- c("Spectra_File_Name","File_Version","Program_Version","Spec_File_Version",
                "Instrument_Type","Instrument_Number","Calibration_Series","Inst_Dynamic_Range",
                "Cal_Bulb_Number","DC_Correction","DC_Time_UTC","WRef_Time_UTC","Spectrum_Time_UTC",
                "Spectrum_DOY","Spectrum_Data_Type","Spectrum_Data_Format","Calibrated_Starting_Wavelength",
                "Calibrated_Wavelength_Step","Detector_Channels","Integration_Time_ms","SWIR1_Gain",
                "SWIR2_Gain","SWIR1_Offset","SWIR2_Offset","VNIR_SWIR1_Splice","SWIR1_SWIR2_Splice",
                "DC_Value","Num_DC_Measurements","Num_WRef_Measurements","Num_Sample_Measurements",
                "Foreoptic_Deg","Comments")
  out.metadata <- data.frame(asd.files.names,file.ver,program.ver,spec.file.ver,inst.type,inst.number,
                             calib.series,dyn.range,bulb,dc.corr,dc.time,wref.time,spec.date,spec.doy,
                             data.type,data.format,ch1.wavelength,wavelength.step,channels,int.time,
                             swir1.gain,swir2.gain,swir1.offset,swir2.offset,splice1,splice2,dcc.value,
                             dc.count,ref.count,sample.count,foreoptic.deg,comments)
  names(out.metadata) <- out.head
  
  if (intern){
    return(out.metadata)
  } else {
    if (!file.exists(out.dir)) dir.create(out.dir, recursive=TRUE)
    write.csv(out.metadata,paste(out.dir,"/",out.file.name,".metadata",output.file.ext,sep=""),
              row.names=FALSE)
  }
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name extract.metadata.se
##' @title Extract metadata from Spectral Evolution files.  Called from extract.metadata
##' 
##' @author Shawn P. Serbin
##' 
extract.metadata.se <- function(file.dir,out.dir,spec.file.ext,output.file.ext,tz,intern){
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  # Check for custom output file extension
  if (is.null(spec.file.ext)) {
    spec.file.ext <- ".sed"
  } else {
    spec.file.ext <- spec.file.ext
  }
  
  if (intern==FALSE){
    print("------- Processing file(s) -------")
  }
  
  out.head <- c("File_Name","Instrument","Detectors","Measurement", "Date", "Time", "Temperature",
                "Battery_Voltage","Averages","Integration","Dark_Mode","Foreoptic","Radiometric_Calibration",
                "Units","Spec_Units","Wavelength_Range","Detector_Channels","Cal_Ref_Correction_File","Num_Data_Columns",
                "Latitude_DD","Longitude_DD","Altitude_m","GPS_Time_UTC","Num_Satellites")
  
  # Determine if running on single file or directory
  check <- file.info(file.dir)
  if (check$isdir) {
    se.files.names <- list.files(path=file.dir,pattern=spec.file.ext,full.names=FALSE)
    se.files.names <- unlist(strsplit(se.files.names,".sed"))
    se.files <- list.files(path=file.dir,pattern=spec.file.ext,full.names=TRUE)
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
  dm <- rep(NA,length(se.files));radcal <- rep(NA,length(se.files));foreoptic <- rep(NA,length(se.files))
  units <- rep(NA,length(se.files));wave.range <- rep(NA,length(se.files));lat <- rep(NA,length(se.files))
  long <- rep(NA,length(se.files));alt <- rep(NA,length(se.files));GPS.time <- rep(NA,length(se.files))
  satellites <- rep(NA,length(se.files));cal.ref.cor.file <- rep(NA,length(se.files))
  channels <- rep(NA,length(se.files));data.columns <- rep(NA,length(se.files))
  spec.units <- rep(NA,length(se.files))
  
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
    foreoptic[i] <- gsub(" ","",(strsplit(file.head[14],":")[[1]])[2])
    radcal[i] <- gsub(" ","",(strsplit(file.head[15],":")[[1]])[2])
    units[i] <- gsub(" ","",(strsplit(file.head[16],":")[[1]])[2])
    wave.range[i] <- gsub(" ","",(strsplit(file.head[17],":")[[1]])[2])
    lat[i] <- gsub(" ","",(strsplit(file.head[18],":")[[1]])[2])
    long[i] <- gsub(" ","",(strsplit(file.head[19],":")[[1]])[2])
    alt[i] <- gsub(" ","",(strsplit(file.head[20],":")[[1]])[2])
    GPS.time[i] <- paste(gsub(" ","",(strsplit(file.head[21],":")[[1]])[2:4]),sep="",collapse=":")
    satellites[i] <- gsub(" ","",(strsplit(file.head[22],":")[[1]])[2])
    cal.ref.cor.file[i] <- gsub(" ","",(strsplit(file.head[23],":")[[1]])[2])
    channels[i] <- gsub(" ","",(strsplit(file.head[24],":")[[1]])[2])
    data.columns[i] <- gsub("[^0-9]", "", strsplit(file.head[25],":")[[1]])
    
    temp.1 <- read.table(se.files[i],skip=data.line,nrows=1,sep="\t")
    temp.2 <- apply(temp.1, 1, function(x) pmatch("Reflect",x)) 
    temp.3 <- gsub("Reflect.", "", temp.1[temp.2])
    if (temp.3=="%"){
      spec.units[i] <- "Percent"
    } else {
      spec.units[i] <- "0-1"
    }
    rm(data.line,file.head,temp.1,temp.2,temp.3)
  }

  # Create output
  out.metadata <- data.frame(se.files.names,inst,detec,meas,date,time,temp,batt,avg,int,dm,foreoptic,radcal,
                             units,spec.units,wave.range,channels,cal.ref.cor.file,data.columns,lat,long,alt,
                             GPS.time,satellites)
  names(out.metadata) <- out.head
  
  if (intern){
    return(out.metadata)
  } else {
    if (!file.exists(out.dir)) dir.create(out.dir, recursive=TRUE)
    write.csv(out.metadata,paste(out.dir,"/",out.file.name,".metadata",output.file.ext,sep=""),
              row.names=FALSE)
  }
  
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
##' @param header logical. Do/does the file(s) have a header line?
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
##' @param header logical. Does the spectra file(s) have a header line? Default = TRUE
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