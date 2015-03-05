#--------------------------------------------------------------------------------------------------#
# A location to consolidate all averaging functions for all supported spectral file formats
# and instruments
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' @name spec.avg
##' Temporary name for function.  Change to average.spec once all are consolidated into a single call
##' @title Average associated spectra files into a single spectrum. Works on a single spectra or a 
##' directory.
##'
##' @param file.dir File directory or filename of single spectra for processing
##' @param out.dir Output directory for metadata information file
##'
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
  
  # Instrument - ***This section needs to be refined***
  if (is.null(settings.file) && is.null(instrument) && is.null(spec.file.ext)){ 
    stop("No instrument defined in settings file or function call.")
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
  if (is.null(settings.file) && is.null(file.dir)){
    stop("No input file directory given in settings file or function call.")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (is.null(file.dir) && !is.null(settings.file$output.dir)){
    file.dir <- settings.file$spec.dir
  }
  
  if (is.null(settings.file) && is.null(file.dir)){
    stop("ERROR: No input file directory given in settings file or function call.")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (is.null(file.dir) && !is.null(settings.file$output.dir)){
    file.dir <- paste(settings.file$output.dir,dlm,"jc_files/",sep="") #assuming JC correction has been done
  } 
  
  
  
  
  
  
  
  # Run appropriate function for meta-data extraction
  if (!is.null(settings.file)){
    settings.file=settings.file
  }
  do.call(paste("average.spec",tolower(instrument),sep="."),
          args = list(file.dir=file.dir,out.dir=out.dir,spec.file.ext=spec.file.ext,
                      output.file.ext=output.file.ext,settings.file=settings.file))
  
  
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
average.spec.svc <- function(file.dir,out.dir,spec.file.ext,output.file.ext,settings.file) { 
  
  ### Check for proper input directory
  if (is.null(settings.file) && is.null(file.dir)){
    stop("No input file directory given in settings file or function call.")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (is.null(file.dir) && !is.null(settings.file$output.dir)){
    file.dir <- settings.file$spec.dir
  }
  
  print(file.dir)
  
}  ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################