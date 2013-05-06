#--------------------------------------------------------------------------------------------------#
##'
##' A wrapper function to import raw binary ASD spectra files, apply jump correction to 
##' the imported files and average replicate files into a single spectrum.  Calls read.asd(),
##' jump.correction(), and average.spec() functions.
##'
##' @name import.asd
##' @title Import raw binary ASD spectra files
##' @param asd.dir directory containing raw ASD files or sub-directories to be processed. 
##' Not needed if specified in settings file.
##' @param output.dir main ouput directory. Not needed if specified in settings file.
##' @param jump.correction option to apply a jump correction to the imported spectra files. 
##' Not needed if specified in settings file.
##' @param average option to average imported spectra files.  Will use
##' jump corrected files if exist. Not needed if specified in settings file.
##' @param start.wave starting wavelength of spectra files. Not needed if specified in settings file.
##' @param end.wave ending wavelength of spectra files. Not needed if specified in settings file.
##' @param step.size resolution of spectra files. E.g. 1 for 1nm, 5 for 5nm. Not needed if specified in settings file.
##' @param jumploc1 location of the first jump in the spectra to correct. Not needed if jump.correction=FALSE
##' or if specified in settings file.
##' @param jumploc2 location of the second jump in the spectra to correct. Not needed if jump.correction=FALSE
##' or if specified in settings file.
##' @param asd.file.ext file extension for input binary asd spectra files
##' @param output.file.ext optional setting to set file extension to output files. Defaults to .csv
##' @param settings.file settings file used for spectral processing options (OPTIONAL).  
##' Contains information related to the spectra collection instrument, output directories, 
##' and processing options such as applying a jump correction to the spectra files.  Options in the settings
##' file take precedent over options selected in the function call.
##' 
##' @export
##'
##' @author Shawn P. Serbin
import.asd <- function(asd.dir=NULL,output.dir=NULL,jump.correction=FALSE,
                       average=FALSE,start.wave=NULL,end.wave=NULL,
                       step.size=NULL,jumploc1=NULL,jumploc2=NULL,asd.file.ext=NULL,
                       output.file.ext=NULL,settings.file=NULL){
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  #------------------------- Function setup -------------------------#
  ### Check for settings file
  if (is.null(settings.file)){
    print("----- No settings file specified -----")
    
  } else{
    print("")
    print("----- Settings file specified -----")
    
    # ----- Define instrument specifics and defaults ----- #
    if (is.null(settings.file$instrument$name)){
      print(" ")
      print("----- Instrument: Unspecified")
    } else {
      print(" ")
      print(paste("----- Instrument: ",settings.file$instrument$name,sep=""))
    }
  }
  
  ### Check for instrument info
  if(is.null(settings.file$instrument$start.wave) && is.null(start.wave)){
    stop("*****ERROR: Instrument starting wavelength not specified.")
  } else if (!is.null(settings.file$instrument$start.wave)) {
    print(paste("----- Starting wavelength: ",settings.file$instrument$start.wave,sep=""))
    start.wave <- as.numeric(settings.file$instrument$start.wave)
  }
  
  if(is.null(settings.file$instrument$end.wave) && is.null(end.wave)){
    stop("*****ERROR: Instrument ending wavelength not specified.")
  } else if (!is.null(settings.file$instrument$end.wave)) {
    print(paste("----- Starting wavelength: ",settings.file$instrument$end.wave,sep=""))
    end.wave <- as.numeric(settings.file$instrument$end.wave)
  }
  
  if(is.null(settings.file$instrument$step.size) && is.null(step.size)){
    print("*****WARNING: Instrument step size not specified.  Will be calculated.")
  } else if (!is.null(settings.file$instrument$step.size)) {
    print(paste("----- Step size: ",settings.file$instrument$step.size,sep=""))
    step.size <- as.numeric(settings.file$instrument$step.size)
  }
  
  ### Jump correction settings
  if (settings.file$options$jump.correction==TRUE || jump.correction==TRUE){
    # jump 1
    if(is.null(settings.file$instrument$jumploc1) && is.null(jumploc1)){
      stop("*****ERROR: Jump correction selected but first jump location not set. Please correct.")
    } else if (!is.null(settings.file$instrument$jumploc1)) {
      print(paste("----- First jump location: ",settings.file$instrument$jumploc1,sep=""))
      jumploc1 <- as.numeric(settings.file$instrument$jumploc1)
    }
    # jump 2
    if(is.null(settings.file$instrument$jumploc2) && is.null(jumploc2)){
      stop("*****ERROR: Jump correction selected but second jump location not set. Please correct.")
    } else if (!is.null(settings.file$instrument$jumploc2)) {
      print(paste("----- Second jump location: ",settings.file$instrument$jumploc2,sep=""))
      jumploc2 <- as.numeric(settings.file$instrument$jumploc2)
    }
  }
  
  ### ASD file suffix length
  if(is.null(settings.file$options$suffix.length) && is.null(suffix.length)){
    print("*****WARNING: File suffix length not specified.  Will use 5 (i.e. *00000.asd)")
  } else if (!is.null(settings.file$options$suffix.length)) {
    suffix.length <- as.numeric(settings.file$options$suffix.length)
  } else {
    suffix.length <- as.numeric(suffix.length)
  }
  
  # set bias.threshold
  if(is.null(settings.file$options$bias.threshold) && is.null(bias.threshold)){
    print("*****WARNING: Bias threshold not specified.  Will use 0.06")
  } else if (!is.null(settings.file$options$bias.threshold)) {
    bias.threshold <- settings.file$options$bias.threshold
  } else {
    bias.threshold <- bias.threshold
  }
  
  print("")
  print("")
  
  ### Check for asd.dir
  if(is.null(settings.file$asd.dir) && is.null(asd.dir)){
    stop("*****ERROR: No ASD directory specified and did not find any settings file to load.")
    
  } else if (is.null(settings.file$asd.dir) && !is.null(asd.dir)){
    print(paste("----- Using ASD Directory: ",asd.dir,sep=""))
    asd.dir <- asd.dir
    
  } else if (!is.null(settings.file$asd.dir)) {
    print(paste("----- Using ASD Directory: ",settings.file$asd.dir,sep=""))
    asd.dir <- settings.file$asd.dir
  }
  
  ### Check for output.dir or set default
  if(is.null(settings.file$output.dir) && is.null(output.dir)){
    print(paste("**** WARNING: Main output directory not specified.  Defaulting to: ",
                settings.file$asd.dir,sep=""))
    output.dir <- paste(settings.file$asd.dir,dlm,sep="")
    
  } else if (is.null(settings.file$output.dir) && !is.null(output.dir)) {
    print(paste("----- Using Output Directory: ",output.dir,sep=""))
    output.dir <- output.dir
    
  } else if (!is.null(settings.file$output.dir)) {
    print(paste("----- Using Output Directory: ",settings.file$output.dir,sep=""))
    output.dir <- settings.file$output.dir
  }
  
  ### Check for optional asd.file.ext setting and set to custom option or default to .asd
  if (is.null(settings.file$options$asd.file.ext) && is.null(asd.file.ext)) {
    asd.file.ext <- ".asd"
  } else if (is.null(settings.file$options$asd.file.ext) && !is.null(asd.file.ext)) {
    asd.file.ext <- as.character(asd.file.ext)
  } else if (!is.null(settings.file$options$asd.file.ext)) {
    asd.file.ext <- as.character(settings.file$options$asd.file.ext)
  }
  
  ### Check for optional file.ext setting and set to custom option or default to .csv
  if (is.null(settings.file$options$output.file.ext) && is.null(output.file.ext)) {
    output.file.ext <- ".csv"
  } else if (is.null(settings.file$options$output.file.ext) && !is.null(output.file.ext)) {
    output.file.ext <- as.character(output.file.ext)
  } else if (!is.null(settings.file$options$output.file.ext)) {
    output.file.ext <- as.character(settings.file$options$output.file.ext)
  }
  
  #------------------------- Start main function -------------------------#

  ### Setup main output directory to place ascii, jc, and average sub-directories
  if (! file.exists(output.dir)) dir.create(output.dir)
  
  #------------------------- Setup output directory structure -------------------------#
  dirs <- list.dirs(path = asd.dir,full.names=FALSE)
  num.dirs <- length(dirs)
  
  dir.name.length <- rep(NA,1,num.dirs)
  for (i in 1:num.dirs){
    dir.name.length[i] <- length(unlist(strsplit(dirs[i],dlm)))
  }
  
  dir.max <- max(dir.name.length)
  dirs <- dirs[which(dir.name.length==dir.max)] 
  num.dirs <- length(dirs)  # <--- How many subdirectories are there to process?
  
  ### Setup desired output directories
  if (nchar(output.dir)>0){
    
    ### Custom output locations defined by user
    ascii.dir <- paste(output.dir,dlm,"ascii_files",sep="")
    if (! file.exists(ascii.dir)) dir.create(ascii.dir)
    
    jc.dir <- paste(output.dir,dlm,"jc_files",sep="")
    if (! file.exists(jc.dir)) dir.create(jc.dir)
    
    avg.dir <- paste(output.dir,dlm,"averaged_files",sep="")
    if (! file.exists(avg.dir)) dir.create(avg.dir)
    
    ### Setup individual output sub-directories
    for (i in 1:num.dirs){
      
      # ---- ASCII dirs ----
      ### replace ASD.dir with output ASCII dir for each subdir
      out.dir <- gsub(asd.dir,ascii.dir,dirs[i])
      if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
      rm(out.dir) 
      
      # ---- JC dirs ----
      if (settings.file$options$jump.correction==TRUE || jump.correction==TRUE){
        out.dir <- gsub(asd.dir,jc.dir,dirs[i])
        if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
        rm(out.dir)
      } ### End setting up JC dirs
      
      # ---- Avg dirs ----
      if (settings.file$options$average==TRUE || average==TRUE){
        out.dir <- gsub(asd.dir,avg.dir,dirs[i])
        if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
        rm(out.dir)
      } ### End setting up avg dirs
    } ### End for loop
    
  } else {
    
    ### Default location for output of ascii and jc files
    ind <- gregexpr(dlm, asd.dir)[[1]]
    
    ascii.dir <- paste(substr(asd.dir,ind[1], ind[length(ind)]-1),dlm,"ascii_files",sep="")
    if (! file.exists(ascii.dir)) dir.create(ascii.dir)
    
    jc.dir <- paste(substr(asd.dir,ind[1], ind[length(ind)]-1),dlm,"jc_files",sep="")
    if (! file.exists(jc.dir)) dir.create(jc.dir)
    
    avg.dir <- paste(substr(asd.dir,ind[1], ind[length(ind)]-1),dlm,"averaged_files",sep="")
    if (! file.exists(avg.dir)) dir.create(avg.dir)
    
    # Setup individual output directories
    for (i in 1:num.dirs){
      # ---- ASCII dirs ----
      # replace ASD.dir with output ASCII dir for each subdir
      out.dir <- gsub(asd.dir,ascii.dir,dirs[i])
      if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
      rm(out.dir)
      
      # ---- JC dirs ----
      if (settings.file$options$jump.correction==TRUE || jump.correction==TRUE){
        out.dir <- gsub(asd.dir,jc.dir,dirs[i])
        if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
        rm(out.dir)
      }
      
      # ---- Avg dirs ----
      if (settings.file$options$average==TRUE || average==TRUE){
        out.dir <- gsub(asd.dir,avg.dir,dirs[i])
        if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
        rm(out.dir)
      }
    } ### End for loop
  } ### End if/else. Output directories have been created

  #--------------------- Read in binary .asd files and convert to ascii text. ---------------------#
  ### Info: Import raw binary spectra files and convert to text files.
  
  print("")
  print("")
  print("------------- Importing ASD spectra files -------------")
  
  rm(num.dirs)
  ascii.dirs <- list.dirs(path = ascii.dir,full.names=FALSE)
  num.dirs <- length(ascii.dirs)
  dir.length <- rep(NA,1,num.dirs)
  for (i in 1:num.dirs){
    dir.length[i] <- length(unlist(strsplit(ascii.dirs[i],dlm)))
  }
  
  rm(num.dirs)
  dir.max = max(dir.length)
  ascii.dirs = ascii.dirs[which(dir.length==dir.max)] 
  num.dirs = length(ascii.dirs)  # <--- How many subdirectories are there to process?
  #print("Num Dirs")
  #print(num.dirs)
  
  print("")
  print("")
  
  t1 = Sys.time()
  ### Main outer loop
  system.time(for (i in 1:num.dirs){
    
    ### Get list of .asd files in current directory
    asd.files  <- list.files(path=dirs[i],pattern=asd.file.ext)
    num.files	<- length(asd.files)
    
    if (num.files<1){
      print("*********************************************************************************")
      stop(paste("******* ERROR: No ASD files found in directory with extension: ",asd.file.ext," *******",sep=""))
    }

    ### Display info to the terminal
    tmp  <- unlist(strsplit(dirs[i],dlm))
    current <- tmp[length(tmp)]
    print(paste("------- Processing directory: ",current))
    print(paste("------- Number of files: ",num.files))
    flush.console() #<--- show output in real-time
    
    ### Call read.asd function and import spectra
    read.asd(file.dir=dirs[i],out.dir=ascii.dirs[i],start.wave=start.wave,end.wave=end.wave,
                     step.size=step.size,asd.file.ext=asd.file.ext,output.file.ext=output.file.ext,
             spec.dataframe=FALSE)

  }) ### End read.asd outer loop
  
  ### Get runtime stats for import of asd files
  t2 = Sys.time()
  ellapsed = t2-t1
  print(ellapsed)
  #print(paste("---- Processing time: ", round(ellapsed,2),"s" ))
  closeAllConnections()                            # close any remaining file connections
  Sys.sleep(2)
  rm(t1,t2,ellapsed)
  
  
  #-------- Open ascii files and apply spectra jump (splice) correction. --------#
  if (settings.file$options$jump.correction==TRUE | jump.correction==TRUE) {
    
    print("")
    print("")
    print("----- Applying jump (splice) correction to spectra files -----")
    print("")
    print("")
    
    ### Define output JC dirs
    rm(num.dirs,dir.max)
    jc.dirs <- list.dirs(path = jc.dir,full.names=FALSE)
    # Remove bad spec dirs
    bad.spec.dirs <- grep(pattern="Bad_Spectra",jc.dirs)
    if (length(bad.spec.dirs)>0){
      jc.dirs <- jc.dirs[-bad.spec.dirs]
    }
    num.dirs <- length(jc.dirs)
    dir.length <- rep(NA,1,num.dirs)
    for (i in 1:num.dirs){
      dir.length[i] <- length(unlist(strsplit(jc.dirs[i],dlm)))
    }

    rm(num.dirs)
    dir.max = max(dir.length)
    jc.dirs = jc.dirs[which(dir.length==dir.max)] 
    num.dirs = length(jc.dirs)  # <--- How many subdirectories are there to process?
    #print(jc.dirs) # for debugging
    
    ### Run JC loop
    t1 = Sys.time()
    system.time(for (i in 1:num.dirs){

      ### call jump.correction() function to processes directories
      jump.correction(ascii.dirs[i],jc.dirs[i],start.wave=start.wave,end.wave=end.wave,
                      step.size=step.size,jumploc1=jumploc1,jumploc2=jumploc2,
                      output.file.ext=output.file.ext)
      
    }) ### End of for loop
    
    ### Get runtime stats for import of asd files
    t2 = Sys.time()
    ellapsed = t2-t1
    print(ellapsed)
    #print(paste("---- Processing time: ", round(ellapsed,2),"s" ))
    closeAllConnections()                            # close any remaining file connections
    Sys.sleep(2)
    rm(t1,t2,ellapsed)
    
  } ### End of jump correction
  
  #------------------------ Average spectra files. ------------------------#
  if (settings.file$options$average==TRUE | average==TRUE) {
    
    print("")
    print("")
    print("----- Averaging spectral files -----")
    print("")
    print("")
    
    ### Define output averaged dirs
    rm(num.dirs,dir.max)
    avg.dirs <- list.dirs(path = avg.dir,full.names=FALSE)
    # Remove bad spec dirs
    bad.spec.dirs <- grep(pattern="Bad_Spectra",avg.dirs)
    if (length(bad.spec.dirs)>0){
      avg.dirs <- avg.dirs[-bad.spec.dirs]
    }
    num.dirs <- length(avg.dirs)
    dir.length <- rep(NA,1,num.dirs)
    for (i in 1:num.dirs){
      dir.length[i] <- length(unlist(strsplit(avg.dirs[i],dlm)))
    }
    
    rm(num.dirs)
    dir.max = max(dir.length)
    avg.dirs = avg.dirs[which(dir.length==dir.max)] 
    num.dirs = length(avg.dirs)  # <--- How many subdirectories are there to process?
    #print(avg.dirs) # for debugging
    
    ### Run averaging loop
    t1 = Sys.time()
    system.time(for (i in 1:num.dirs){
      
      ### call jump.correction() function to processes directories
      average.spec(jc.dirs[i],avg.dirs[i],start.wave=start.wave,end.wave=end.wave,
                   step.size=step.size,bias.threshold=bias.threshold,
                   suffix.length=suffix.length,output.file.ext=output.file.ext)
      
    }) ### End of averaging for loop
    
    ### Get runtime stats for import of asd files
    t2 = Sys.time()
    ellapsed = t2-t1
    print(ellapsed)
    #print(paste("---- Processing time: ", round(ellapsed,2),"s" ))
    closeAllConnections()                            # close any remaining file connections
    Sys.sleep(2)
    rm(t1,t2,ellapsed)
    
  } ### End of averaging files
  
} ### End of function call
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################