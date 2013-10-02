#--------------------------------------------------------------------------------------------------#
##'
##' Function to average replicate spectra within a directory of spectra files
##'
##' @name average.spec.se
##' @title average replicate spectra within a directory of spectra files
##' @param file.dir directory of spectra files to process
##' @param out.dir output directory for processed spectra files
##' 
##' @export
##'
##' @author Shawn P. Serbin
##'
average.spec.se <- function(file.dir=NULL,out.dir=NULL,spec.file.ext=NULL,start.wave=NULL,
                         end.wave=NULL,step.size=NULL,bias.threshold=NULL,suffix.length=NULL,
                         output.file.ext=NULL,settings.file=NULL) {
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  # TODO:  USE invisible to return a dataframe of spectra
  
  ### Check for proper input directory
  if (is.null(settings.file) && is.null(file.dir)){
    stop("No input file directory given in settings file or function call.")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (is.null(file.dir) && !is.null(settings.file$output.dir)){
    file.dir <- settings.file$spec.dir
  }
  
  ### Check for input spec file extension
  if (is.null(settings.file$options$spec.file.ext) && is.null(spec.file.ext)){
    warning("No input spectra file extension given. Using default .sed")
    spec.file.ext <- ".sed"
  } else if (!is.null(spec.file.ext)) {
    spec.file.ext <- spec.file.ext
  } else if (!is.null(settings.file$options$spec.file.ext)) {
    spec.file.ext <- settings.file$options$spec.file.ext
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
  
  ### Create bad spectra folder. Spectra not used in averages
  badspec.dir <- paste(out.dir,dlm,"Bad_Spectra",sep="")
  if (! file.exists(badspec.dir)) dir.create(badspec.dir,recursive=TRUE)
  
  ### Remove any previous output in out.dir
  unlink(list.files(out.dir,full.names=TRUE),recursive=FALSE,force=TRUE)
  unlink(list.files(badspec.dir,full.names=TRUE),recursive=TRUE,force=TRUE)

  ### Look for a custom output extension, otherwise use default
  if (is.null(settings.file$options$output.file.ext) && is.null(output.file.ext)){
    output.file.ext <- ".csv"  # <-- Default
  } else if (!is.null(output.file.ext)){
    output.file.ext <- output.file.ext
  } else if (!is.null(settings.file$options$output.file.ext)){
    output.file.ext <- settings.file$options$output.file.ext
  } 
  
  ### Define suffix length.  Needed to properly subset spectra by groups for averaging replicates
  if (is.null(settings.file$options$suffix.length) && is.null(suffix.length)){
    stop("File suffix length not defined.  Please set in settings file or at function call before continuing")
  } else if (!is.null(suffix.length)){
    suffix.length <- suffix.length
  } else if (!is.null(settings.file$options$suffix.length)){
    suffix.length <- as.numeric(settings.file$options$suffix.length)
  } 
  
  ### Define bias threshold
  if (is.null(settings.file$options$bias.threshold) && is.null(bias.threshold)){
    warning("Bias threshold not set.Defaulting to 0.06")
    bias.threshold <- 0.06
  } else if (!is.null(bias.threshold)){
    bias.threshold <- bias.threshold
  } else if (!is.null(settings.file$options$bias.threshold)){
    bias.threshold <- as.numeric(settings.file$options$bias.threshold)
  } 
  
  ### Count number of files to process
  se.files  = list.files(path=file.dir,pattern=spec.file.ext,
                         full.names=FALSE)
  num.files  = length(se.files)
  
  ### Check whether files exist. STOP if files missing and display an error
  if (num.files<1){
    stop(paste0("No ASCII files found in directory with extension: ",output.file.ext,sep=""))
  }
  
  ### Create file info list for output
  info = data.frame(Spectra=rep(NA,num.files),Threshold_Check = rep(NA,num.files),
                    Threshold_Value=rep(NA,num.files),Failed_Sdev_Check= rep(NA,num.files))
  names(info)=c("Spectra","Threshold Check","Bias Threshold Value",
                "Failed Outlier Test (Yes/No)?")
  
  ### Display info to console
  tmp  = unlist(strsplit(file.dir,dlm))
  current = tmp[length(tmp)]
  print(paste("----- Processing directory: ",current) )
  flush.console() #<--- show output in real-time
  
  j <- 1 # <--- Numeric counter for progress bar
  pb <- txtProgressBar(min = 0, max = 100, char="*",width=70,style = 3)
  
  ### Read in sed files for averaging
  if (is.null(start.wave) | is.null(end.wave)) {
    # get info to build internal array
    data.line <- strsplit(system(paste("grep -n","Data", se.files[1]),intern=TRUE)[2],":")[[1]]
    data.line <- as.numeric(data.line[1])
    file.head <- readLines(se.files[1],n=data.line-1)
    
    
  }

  
  
  
  in.spec <- array(0,dim=c(num.files,
                           (end.wave-start.wave)+1)) # build empty array to populate with spectra files
  
  
  
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################