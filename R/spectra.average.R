#--------------------------------------------------------------------------------------------------#
# A location to consolidate all averaging functions for all supported spectral file formats
# and instruments
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' @name spec.avg
##' Temporary name for function.  Change to average.spec once all are consolidated into a single call
##' 
##' @title Average associated spectra files into a single spectrum. Works on a single spectra or a 
##' directory.
##'
##' @param file.dir File directory or filename of single spectra for processing
##' @param out.dir Output directory for metadata information file
##' @param spec.type Option to set what type of spectra to process. Options: Reflectance,
##' Transmittance.  Can be set with abbreviations: e.g. "Refl" or "Tran". Default is "Reflectance"
##' @param instrument What instrument was used to collect spectra.  Current options: ASD, SE, SVC
##' @param spec.file.ext [Optional] Input spectra file extension. E.g. .asd (ASD), .sed (Spectral Evolution),
##' or .sig (Spectra Vista). In not input extension is assumed based on instrument type.
##' @param start.wave Starting wavelength of spectra files. 
##' Not needed if specified in XML settings file.
##' @param end.wave Ending wavelength of spectra files. Not needed if 
##' specified in XML settings file. 
##' @param step.size Resolution of spectra files. E.g. 1 for 1nm, 5 for 5nm. 
##' Not needed if specified in XML settings file.  **Phasing this option out through the use of
##' spec file metadata and determining wavelength numbers from spec files
##'
##'
##'
##'
##'
##'
##'
spec.avg <- function(file.dir=NULL,out.dir=NULL,spec.type="Reflectance",instrument=NULL,
                     spec.file.ext=NULL,start.wave=NULL,end.wave=NULL,step.size=NULL,
                     bias.threshold=NULL,outlier.cutoff=2.0,suffix.length=NULL,
                     output.file.ext=NULL,settings.file=NULL) {
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  print(" ")
  
  #--------------------- Setup function -----------------------#
  
  # Instrument - ***This section needs to be refined*** Cleanup and make more general
  if (is.null(settings.file) && is.null(instrument) && is.null(spec.file.ext)){ 
    stop("*** No instrument defined in settings file or function call. ***")
  } else if (!is.null(instrument)){
    instrument <- instrument
  } else if (!is.null(settings.file$instrument$name)){
    inst <- c("ASD","ASD","ASD","SE","SE","SE","SE","SE","SE","SVC","SVC","SVC","SVC","SVC","SVC","SVC")
    temp <- tolower(settings.file$instrument$name)
    #index <- pmatch(temp,c("asd","fieldspec","fieldspec 3","se","spectral evolution","evolution"))
    index <- agrep(pattern=temp,c("asd","fieldspec","fieldspec 3","se","spectral evolution","spectral evolution psm-3500",
                                  "evolution","psm-3500","psm 3500","svc","spectra vista","spec vista","hr 1024i",
                                  "hr 1024","1024i","1024"),max=5,ignore.case = TRUE)
    instrument <- inst[max(index)]
  } else if (spec.file.ext==".asd") {
    instrument <- "ASD"
  } else if (spec.file.ext==".sed") {
    instrument <- "SE"
  }else if (spec.file.ext==".sig") {
    instrument <- "SVC"
  }
  
  print(paste0("Instrument: ",instrument))
  print(" ")
  
  # Input file extension
  if (is.null(settings.file$options$spec.file.ext) && is.null(spec.file.ext)){ 
    if(instrument=="ASD") (spec.file.ext=".asd")
    if(instrument=="SE") (spec.file.ext=".sed")
    if(instrument=="SVC") (spec.file.ext=".sig")
    warning("No input file extension defined in settings file or function call")
    warning(paste0("Using default: ", spec.file.ext))
  } else if (!is.null(spec.file.ext)){
    spec.file.ext <- spec.file.ext
  } else if (!is.null(settings.file$options$spec.file.ext)){
    spec.file.ext <- settings.file$options$spec.file.ext
  }
  
  ### Select optional spectra type for processing and plotting
  if (!is.null(spec.type)) {
    s.type <- c("Reflectance","Transmittance","Canopy")
    #index <- agrep(pattern=spec.type,c("reflectance","transmittance"),ignore.case = TRUE,max.distance=0.3)
    index <- pmatch(tolower(spec.type),c("reflectance","transmittance","canopy"))
    spec.type <- s.type[index]
  } else {
    spec.type <- "Reflectance"
  }
  print(paste0("Spectra Type: ",spec.type))
  print(" ")
  
  # Custom output file extension
  #if (!is.null(settings.file$options$output.file.ext)){
  #  output.file.ext <- settings.file$options$output.file.ext
  #}
  
  ### Look for a custom output extension, otherwise use default
  if (is.null(settings.file$options$output.file.ext) && is.null(output.file.ext)){
    output.file.ext <- ".csv"  # <-- Default
  } else if (!is.null(output.file.ext)){
    output.file.ext <- output.file.ext
  } else if (!is.null(settings.file$options$output.file.ext)){
    output.file.ext <- settings.file$options$output.file.ext
  } 

  ### Check for proper input directory
  if (instrument=="SE" || instrument=="SVC"){
    if (is.null(settings.file) && is.null(file.dir)){
      stop("No input file directory given in settings file or function call.")
    } else if (!is.null(file.dir)){
      file.dir <- file.dir
    } else if (is.null(file.dir) && !is.null(settings.file$output.dir)){
      file.dir <- settings.file$spec.dir
    }
  } else if (instrument=="ASD"){
    if (is.null(settings.file) && is.null(file.dir)){
      stop("ERROR: No input file directory given in settings file or function call.")
    } else if (!is.null(file.dir)){
      file.dir <- file.dir
    } else if (is.null(file.dir) && !is.null(settings.file$output.dir)){
      file.dir <- paste(settings.file$output.dir,dlm,"jc_files/",sep="") #assuming JC correction has been done
    } 
  }
  
  # Determine if input directory is valid/exists
  if (!file.exists(file.dir)){
    stop("*** EROR: Input file directory is not valid ***")
  }
  
  ### create output directory if it doesn't already exist
  if (!is.null(out.dir)) {
    out.dir <- out.dir
  } else if (!is.null(settings.file$output.dir)) {
    out.dir <- paste0(settings.file$output.dir,dlm,"averaged_files/")
  } else {
    ind <- gregexpr(dlm, file.dir)[[1]]
    out.dir <- paste0(substr(file.dir,ind[1], ind[length(ind)-1]-1),dlm,"averaged_files")
  }
  if (!file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)

  ### Create bad spectra folder. Spectra not corrected
  badspec.dir <- paste(out.dir,dlm,"Bad_Spectra",sep="")
  if (! file.exists(badspec.dir)) dir.create(badspec.dir,recursive=TRUE)
  
  if (instrument=="SE" || instrument=="SVC"){
    ### Create white reference folder.
    whiteref.dir <- paste(out.dir,dlm,"White_Reference_Spectra",sep="")
    if (! file.exists(whiteref.dir)) dir.create(whiteref.dir,recursive=TRUE)
    unlink(list.files(whiteref.dir,full.names=TRUE),recursive=TRUE,force=TRUE)
  }

  ### Remove any previous output in out.dir
  unlink(list.files(out.dir,full.names=TRUE),recursive=FALSE,force=TRUE)
  unlink(list.files(badspec.dir,full.names=TRUE),recursive=TRUE,force=TRUE)
  
  ### Define suffix length.  Needed to properly subset spectra by groups for averaging replicates
  if (is.null(settings.file$options$suffix.length) && is.null(suffix.length)){
    stop("ERROR: File suffix length not defined.  Please set in settings file or at function call before continuing")
  } else if (!is.null(suffix.length)){
    suffix.length <- suffix.length
  } else if (!is.null(settings.file$options$suffix.length)){
    suffix.length <- as.numeric(settings.file$options$suffix.length)
  } 

  ### Define bias threshold
  if (is.null(settings.file$options$bias.threshold) && is.null(bias.threshold)){
    warning("Bias threshold not set.  Defaulting to 0.06")
    bias.threshold <- 0.06
  } else if (!is.null(bias.threshold)){
    bias.threshold <- bias.threshold
  } else if (!is.null(settings.file$options$bias.threshold)){
    bias.threshold <- as.numeric(settings.file$options$bias.threshold)
  } 
  
  #--------------------- Run appropriate averaging function -----------------------# 
  
  # Run appropriate function for spectral averaging
  do.call(paste("average.spec",tolower(instrument),sep="."),
          args = list(file.dir=file.dir,out.dir=out.dir,spec.file.ext=spec.file.ext,
                      output.file.ext=output.file.ext,spec.type=spec.type,badspec.dir,
                      whiteref.dir=NULL,suffix.length=suffix.length,
                      bias.threshold=bias.threshold,start.wave=start.wave,end.wave=end.wave,
                      step.size=step.size))
  
} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##'
##'
##'
##'
##'
##'
##'
##'
average.spec.svc <- function(file.dir,out.dir,spec.file.ext,output.file.ext,spec.type,
                             badspec.dir,whiteref.dir,suffix.length,bias.threshold,
                             start.wave=start.wave,end.wave=end.wave,step.size=step.size) { 

  #print(file.dir)
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  ### Count number of files to process
  svc.files <- list.files(path=file.dir,pattern=spec.file.ext,
                         full.names=FALSE)
  num.files <- length(svc.files)
  
  ### Check whether files exist. STOP if files missing and display an error
  if (num.files<1){
    stop(paste0("No files found in directory with extension: ",spec.file.ext,sep=""))
  }
  
  ### Create file info list for output
  info <- data.frame(Spectra=rep(NA,num.files),WhiteRef=rep(NA,num.files),Threshold_Check = rep(NA,num.files),
                    Threshold_Value=rep(NA,num.files),Failed_Sdev_Check= rep(NA,num.files))
  names(info)=c("Spectra","White_Reference (Yes/No)?","Threshold Check","Bias Threshold Value",
                "Failed Outlier Test (Yes/No)?")
  
  ### Display info to console
  tmp  <- unlist(strsplit(file.dir,dlm))
  current <- tmp[length(tmp)]
  print(paste("----- Processing directory: ",current) )
  flush.console() #<--- show output in real-time
  
  j <- 1 # <--- Numeric counter for progress bar
  pb <- txtProgressBar(min = 0, max = 100, char="*",width=70,style = 3)
  
  # Grab wavelength info from files if not given _- NEEDS UPDATE TO ACCOUNT FOR SVC NUANCE!!! E.g. waves, step for uninterp data
  if (is.null(start.wave) | is.null(end.wave) | is.null(step.size)) {
    wave.range <- as.character(extract.metadata(file.dir=paste0(file.dir,"/",svc.files[1]),spec.file.ext=spec.file.ext)$Wavelength_Range)
    channels <- as.numeric(as.character(droplevels(extract.metadata(file.dir=paste0(file.dir,"/",se.files[1]),spec.file.ext=spec.file.ext)$Detector_Channels)))
    start.wave <- as.numeric((strsplit(wave.range,"-")[[1]])[1])
    end.wave <- as.numeric((strsplit(wave.range,"-")[[1]])[2])
    step.size <- ((end.wave-start.wave)+1)/channels
    lambda <- seq(start.wave,end.wave,step.size)
  } else {
    lambda <- seq(start.wave,end.wave,step.size)
  }
  
  print(lambda)
  
}  ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################