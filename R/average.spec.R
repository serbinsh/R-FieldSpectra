#--------------------------------------------------------------------------------------------------#
##'
##' Function to average replicate spectra within a directory of spectra files
##'
##' @name average.spec
##' @title average replicate spectra within a directory of spectra files
##' 
##' @param file.dir Directory of spectra files to process
##' @param out.dir Output directory for processed spectra files
##' @param spec.type Option to set what type of spectra to average. 
##' Options: Reflectance, Transmittance.  Can be set with abbreviations: e.g. "Refl" or "Tran"
##' Default is "Reflectance"
##' @param start.wave Starting wavelength of spectra files. 
##' Not needed if specified in XML settings file.
##' @param end.wave Ending wavelength of spectra files. Not needed if 
##' specified in XML settings file. 
##' @param step.size Resolution of spectra files. E.g. 1 for 1nm, 5 for 5nm. 
##' Not needed if specified in XML settings file.
##' @param bias.threshold Reflectance/transmittance cutoff to remove spectra with anartificial 
##' bias (shift) due to improper spectral collection
##' @param outlier.cutoff [Optional] Set upper/lower standard deviation cutoff to identify statistical Refl/Trans outliers within
##' individual sample sets.  Set as outlier.cutoff*Sample Sdev, e.g. 2.0*Sdev.  Default 2.0
##' @param suffix.length Length of auto numbering attached to ASD file names.  This number of 
##' characters will be removed from the filename when averaged.
##' @param output.file.ext Optional setting to set file extension of output files. Defaults to .csv
##' @param spec.dataframe Option to return a data frame with the converted spectra files 
##' @param metadata.file Option to select custom metadata file for use in processing. If not set
##' then the information is either read from default metadata file, the settings file or at the function call. 
##' Need to set this as the full qualified path to the spectral metadata file is using a custom file/location
##' @param image Logical. Whether to produce .png images of each spectrum (TRUE) or not (FALSE).
##' Default is FALSE.  Useful for diagnosing spectral observations during processing.
##' @param settings.file Settings file used for spectral processing options (OPTIONAL).  
##' Contains information related to the spectra collection instrument, output directories, 
##' and processing options such as applying a jump correction to the spectra files.  Options in the settings
##' file take precedent over options selected in the function call.
##'
##' @examples
##' \dontrun{
##' average.spec()
##' average.spec(file.dir,out.dir, start.wave=350,end.wave=2500,step.size=1,bias.threshold=0.06,
##' suffix.length=5,output.file.ext=".csv",spec.dataframe = TRUE,)
##' }
##'  
##' @export
##'
##' @author Shawn P. Serbin
##'
average.spec <- function(file.dir=NULL,out.dir=NULL,spec.type="Reflectance",start.wave=NULL,end.wave=NULL,
                         step.size=NULL,bias.threshold=NULL,outlier.cutoff=2.0,suffix.length=NULL,
                         output.file.ext=NULL,metadata.file=NULL,image=FALSE,
                         settings.file=NULL){
  ## TODO:
  # using if(any()) here to remove bad spec with threshold cutoffs
  # use directory rather than file as input to speed up output
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  #--------------------- Setup function -----------------------#

  ### Check for proper input directory
  if (is.null(settings.file) && is.null(file.dir)){
    stop("ERROR: No input file directory given in settings file or function call.")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (is.null(file.dir) && !is.null(settings.file$output.dir)){
    file.dir <- paste(settings.file$output.dir,dlm,"jc_files/",sep="")
  } 
  
  ### create output directory if it doesn't already exist
  if (!is.null(out.dir)) {
    out.dir <- out.dir
  } else if (!is.null(settings.file$output.dir)) {
    out.dir <- paste(settings.file$output.dir,dlm,"averaged_files/",sep="")
  } else {
    ind <- gregexpr(dlm, file.dir)[[1]]
    out.dir <- paste(substr(file.dir,ind[1], ind[length(ind)-1]-1),dlm,"averaged_files",sep="")
  }
  if (!file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
  
  ### Create bad spectra folder. Spectra not corrected
  badspec.dir <- paste(out.dir,dlm,"Bad_Spectra",sep="")
  if (! file.exists(badspec.dir)) dir.create(badspec.dir,recursive=TRUE)
  
  ### Remove any previous output in out.dir
  unlink(list.files(out.dir,full.names=TRUE),recursive=FALSE,force=TRUE)
  unlink(list.files(badspec.dir,full.names=TRUE),recursive=TRUE,force=TRUE)
  
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

  ### Define wavelengths.  If set in settings or function call.  Otherwise read from file header
  if (!is.null(start.wave)){
    start.wave <- start.wave
  } else if (!is.null(settings.file$instrument$start.wave)){
    start.wave <- as.numeric(settings.file$instrument$start.wave)
  }
  if (!is.null(end.wave)){
    end.wave <- end.wave
  } else if (!is.null(settings.file$instrument$end.wave)){
    end.wave <- as.numeric(settings.file$instrument$end.wave)
  }
  if (!is.null(step.size)){
    step.size <- step.size
  } else if (!is.null(settings.file$instrument$step.size)){
    step.size <- as.numeric(settings.file$instrument$step.size)
  }

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
  
  ### Remove diagnostic info from file list to be processed
  jc.files <- list.files(path=file.dir,pattern=output.file.ext,
                            full.names=FALSE)
  jc.files <- jc.files[-grep(pattern="Diagnostic",jc.files)]
  num.files  = length(jc.files)

  ### Check whether files exist. STOP if files missing and display an error
  if (num.files<1){
    stop(paste("******* ERROR: No ASCII files found in directory with extension: ",output.file.ext," *******",sep=""))
  }
  
  ### Create file info list for output
  info <- data.frame(Spectra=rep(NA,num.files),Threshold_Check = rep(NA,num.files),
                    Threshold_Value=rep(NA,num.files),Failed_Sdev_Check= rep(NA,num.files))
  names(info) <- c("Spectra","Threshold Check","Bias Threshold Value",
                "Failed Outlier Test (Yes/No)?")
  
  #-------------------------- Start average loop --------------------------# 
  
  ### Spectra metadata.
  if (is.null(metadata.file)) {
    metadata.dir <- gsub(pattern="jc_files/","",file.dir)
    metadata.file <- list.files(path=metadata.dir,pattern="metadata",full.names=FALSE)
    print(paste("------- Using metadata file: ",metadata.file,sep=""))
    metadata <- read.csv(paste(settings.file$output.dir,dlm,metadata.file,sep=""))
  } else {
    metadata <- read.csv(metadata.file,header=T)
  }
  
  ### Setup wavelengths
  if (is.null(start.wave) | is.null(end.wave) | is.null(step.size)) {
    # Using metadata info.  Read from first observations. Assumes all files are the same.
    # Need to update function to make this work across varying wavelength ranges
    start.wave <- metadata[1,]$Calibrated_Starting_Wavelength
    step.size <- metadata[1,]$Calibrated_Wavelength_Step
    channels <- metadata[1,]$Detector_Channels
    end.wave <- start.wave+((channels-1)/step.size)
    lambda <- seq(start.wave,end.wave,step.size)
    waves <- paste("Wave_",lambda,sep="")
  } else {
    # Using settings file or function argument
    lambda <- seq(start.wave,end.wave,step.size)
    waves <- paste("Wave_",lambda,sep="")
  }
  
  ### Display info to console
  tmp  = unlist(strsplit(file.dir,dlm))
  current = tmp[length(tmp)]
  print(paste("----- Processing directory: ",current) )
  flush.console() #<--- show output in real-time
  
  j <- 1 # <--- Numeric counter for progress bar
  pb <- txtProgressBar(min = 0, max = 100, char="*",width=70,style = 3)
  ### Read in jc files for averaging
  in.spec <- array(0,dim=c(num.files,
                           (end.wave-start.wave)+1)) # build empty array to populate with spectra files
  spec.names <- unlist(strsplit(jc.files,paste("\\",output.file.ext,sep="")))
  spec.names.orig <- spec.names
  spec.names <- substr(spec.names,1,nchar(spec.names)-suffix.length)
  for (i in 1:num.files){
    spec.file <- read.csv(paste(file.dir,dlm,jc.files[i],sep=""))   
    in.spec[i,] <- t(spec.file[,2])
    rm(spec.file)
  }
  
  ### Setup spectra for averaging.
  in.spec2 <- as.data.frame(in.spec)
  in.spec3 <- data.frame(Orig.Spec.Name=spec.names.orig,Spectra=spec.names,in.spec2)
  names(in.spec3) <- c("Orig.Spec.Name","Spectra",waves)
  rm(in.spec,in.spec2)
  
  print("")
  print(paste("----- Total number of spectra files: ",
              dim(in.spec3)[1],sep=""))
  print(paste("----- Total number of spectra samples: ",
              length(unique.spec <- unique(in.spec3$Spectra)),sep=""))
  print(paste0("----- Outlier Threshold: ",outlier.cutoff))
  print("")
  
  setTxtProgressBar(pb, 20)                      # show progress bar
  flush.console()                                #<--- show output in real-time
  
  ### Do initial checks on spectra
  # spec bias threshold
  bias <- bias.threshold
  # 450nm
  bad.spec <- droplevels(in.spec3[which(in.spec3$Wave_450>bias),])
  # Create good.spec data
  good.spec <- droplevels(in.spec3[which(in.spec3$Wave_450<=bias),])
  dims <- dim(good.spec)
  
  ### Update diagnostic info
  for (i in 1:dim(in.spec3)[1]){
    info[i,1] <- as.character(droplevels(in.spec3[i,1]))
    if (as.character(droplevels(in.spec3[i,1])) %in% droplevels(bad.spec[,1])){
      info[i,2] <- "Failed"
      info[i,3] <- as.numeric(round(in.spec3[i,"Wave_450"],4))
      info[i,4] <- "No"
    } else {
      info[i,2] <- "Passed"
      info[i,3] <- as.numeric(round(in.spec3[i,"Wave_450"],4))
    }
  }
  
  setTxtProgressBar(pb, 30)                      # show progress bar
  flush.console()                                #<--- show output in real-time
  
  ##### Initial average
  ind <- factor(as.numeric(good.spec$Spectra))
  mat.data <- as.matrix(good.spec[,3:dims[2]])
  if (dim(mat.data)[1]==0){
    stop("******** ERROR: Bias threshold too strict, no remaining spectra to average. Please correct ********")
  }
  ### Get spectra averages
  spec.avg <- mApply(mat.data,ind,colMeans,simplify=TRUE)
  ### Reformat data for further processing
  if (is.null(dim(spec.avg)[1])) {
    spec.avg <- data.frame(Spectra=unique(good.spec$Spectra), t(as.vector(spec.avg)))
  } else {
    spec.avg <- data.frame(Spectra=unique(good.spec$Spectra), as.data.frame(spec.avg))
  }
  names(spec.avg) <- c("Spectra",waves)
  
  ### Get spectra sdev
  dims <- dim(good.spec)
  spec.sdev <- aggregate(.~Spectra,data=good.spec[,2:dims[2]],sd,simplify=TRUE)
  spec.sdev[is.na(spec.sdev)]=0
  spec.avg.names <- droplevels(spec.avg$Spectra)
  dims <- dim(spec.avg)
  rm(mat.data,ind)
  
  # Debug
  #plot(seq(350,2500,1),unlist(spec.sdev[9,2:dim(spec.sdev)[2]]),ylim=c(0,0.04),type="l")
  
  
  ### Setup spectral checks
  dims <- dim(spec.avg)
  n <- rle(as.numeric(good.spec$Spectra))$lengths
  spec.upper <- spec.avg[,2:dims[2]] + (spec.sdev[,2:dims[2]]*outlier.cutoff) # 2*SD outlier check
  #spec.upper <- spec.avg[,2:dims[2]] + ((spec.sdev[,2:dims[2]]/sqrt(n))*2.96)
  spec.upper <- data.frame(Spectra=spec.avg.names,spec.upper)
  spec.lower <- spec.avg[,2:dims[2]] - (spec.sdev[,2:dims[2]]*outlier.cutoff)
  #spec.lower <- spec.avg[,2:dims[2]] - ((spec.sdev[,2:dims[2]]/sqrt(n))*2.96)
  spec.lower <- data.frame(Spectra=spec.avg.names,spec.lower)
  
  ### For debugging
  #dims <- dim(spec.avg)
  #plot(seq(350,2500,1),unlist(spec.avg[17,2:dims[2]]),type="l",lwd=2,ylim=c(0,0.6))
  #lines(seq(350,2500,1),unlist(spec.upper[17,2:dims[2]]),lty=2)
  #lines(seq(350,2500,1),unlist(spec.lower[17,2:dims[2]]),lty=2)
  #dims <- dim(good.spec)
  #lines(seq(350,2500,1),unlist(good.spec[142,3:dims[2]]),lty=1)
  ###
  
  setTxtProgressBar(pb, 50)                      # show progress bar
  flush.console()                                #<--- show output in real-time
  
  # Do second sdev check of spectra
  ind <- as.numeric(good.spec$Spectra)
  good.spec2 <- as.matrix(good.spec[,-c(1,2)])
  spec.upper <- as.matrix(spec.upper[,-1])
  spec.lower <- as.matrix(spec.lower[,-1])
  for (i in 1:length(ind)){
    ind.avg <- ind[i]
    orig.spec.name <- droplevels(good.spec[i,1])
    # run check of individual spectra against the group mean
    # need to make sure indices are alligned
    # window <- 151:2051 # 500 - 2400
    # window <- 151:1851 # 500 - 2200
    # window <- 151:1751 # 500 - 2100
    if (spec.type=="Reflectance") {
      window <- 151:1851 # 500 - 2200
    } else if (spec.type=="Transmittance") {
      window <- 151:1351 # 500 - 1700
    } else if (spec.type=="Canopy") {
      #window <- c(151:1441) # 500 - 1790
      window <- c(151:1441,1671:1901)
    } 
    if (any(good.spec2[i,window]>spec.upper[ind.avg,window]) || 
      any(good.spec2[i,window] < spec.lower[ind.avg,window])) {
      bad.spec <- rbind(bad.spec,good.spec[i,])
      good.spec[i,] <- NA
      
      # update diagnostic info
      loc <- which(orig.spec.name==info$Spectra,arr.ind=TRUE)
      info[loc,4] <- "Yes"
      
    } else {
      # update diagnostic info
      loc <- which(orig.spec.name==info$Spectra,arr.ind=TRUE)
      info[loc,4] <- "No"
    }
  }
  rm(i,good.spec2,spec.lower,spec.upper,loc,ind)
  
  # Keep only remaining spectra
  good.spec <- droplevels(good.spec[!is.na(good.spec[,3]),])
  good.spec <- droplevels(good.spec[,-1])
  
  ### Create final spectral averages
  rm(spec.avg)
  #spec.avg <- aggregate(.~Spectra,data=good.spec,mean,simplify=TRUE)
  ind <- factor(as.numeric(good.spec$Spectra))
  mat.data <- as.matrix(good.spec[,2:dims[2]])
  spec.avg <- mApply(mat.data,ind,colMeans,simplify=TRUE)
  ### Reformat spectral data for output
  if (is.null(dim(spec.avg)[1])){
    spec.avg <- data.frame(Spectra=unique(good.spec$Spectra), t(as.vector(spec.avg)))
  } else {
    spec.avg <- data.frame(Spectra=unique(good.spec$Spectra), as.data.frame(spec.avg))
  }
  names(spec.avg) <- c("Spectra",waves)
  rm(mat.data,ind)

  setTxtProgressBar(pb, 80)                      # show progress bar
  flush.console()                                #<--- show output in real-time
  
  ### Output bad spectra files and figs
  dims <- dim(bad.spec)
  if (dims[1]>0){
    for (i in 1:dims[1]){
      spec.name <- droplevels(bad.spec[i,1])
      out.spec <- t(bad.spec[i,3:dims[2]])
      out.spec <- data.frame(Wavelength=seq(start.wave,end.wave,step.size),
                             Spectra=out.spec)
      names(out.spec)=c("Wavelength",paste(spec.name))
      out.filename <- paste(droplevels(bad.spec[i,1]),output.file.ext,sep="")
      write.csv(out.spec,paste(badspec.dir,dlm,out.filename,sep=""),row.names=FALSE)
      
      # Output plot of bad spectra for quick reference
      if(image=="TRUE" | settings.file$options$diagnostic.images=="TRUE"){
        rng <- range(out.spec[,2])
        if (rng[1]<0) rng[1] <- 0
        if (rng[2]>1) rng[2] <- 1
        ylimit <- c(rng[1],rng[2])
        png(file=paste(badspec.dir,dlm,spec.name,".png",sep=""),width=800,height=600,res=100)
        plot(out.spec[,1], out.spec[,2],cex=0.01,xlim=c(350,2500),ylim=ylimit,xlab="Wavelength (nm)",
           ylab="Reflectance (%)", main=out.filename,cex.axis=1.3,cex.lab=1.3)
        lines(out.spec[,1], out.spec[,2],lwd=2)
        abline(h=bias,lty=2,col="dark grey")
        box(lwd=2.2)
        dev.off()
      }
    } 
    rm(spec.name,out.spec,out.filename,dims)
  } # End of output of bad spectra

  # Output good spec
  dims <- dim(spec.avg)
  if (dims[1]>0){
    for (i in 1:dims[1]){
      spec.name <- droplevels(spec.avg[i,1])
      out.spec <- t(spec.avg[i,2:dims[2]])
      out.spec <- data.frame(Wavelength=seq(start.wave,end.wave,step.size),
                             Spectra=out.spec)
      names(out.spec)=c("Wavelength",paste(spec.name))
      out.filename <- paste(spec.name,output.file.ext,sep="")
      write.csv(out.spec,paste(out.dir,dlm,out.filename,sep=""),row.names=FALSE)
      
      ### Output plot of average spectra for quick reference
      if(image=="TRUE" | settings.file$options$diagnostic.images=="TRUE"){
        rng <- range(out.spec[,2])
        if (rng[1]<0) rng[1] <- 0
        if (rng[2]>1) rng[2] <- 1
        ylimit <- c(rng[1],rng[2])
        png(file=paste(out.dir,dlm,spec.name,".png",sep=""),width=800,height=600,res=100)
        plot(out.spec[,1], out.spec[,2],cex=0.01,xlim=c(350,2500),ylim=ylimit,xlab="Wavelength (nm)",
            ylab="Reflectance (%)", main=out.filename,cex.axis=1.3,cex.lab=1.3)
        lines(out.spec[,1], out.spec[,2],lwd=2)
        abline(h=bias,lty=2,col="dark grey")
        box(lwd=2.2)
        dev.off()
      }
    }
    rm(spec.name,out.spec,out.filename,dims)
  }

  ### Write out concatenated averaged spectra
  write.csv(spec.avg,paste(out.dir,dlm,"Averaged_Spectra",output.file.ext,sep=""),row.names=FALSE)
  
  ### Output diagnostic info
  write.csv(info,paste(out.dir,dlm,"Spectra_Diagnostic_Information",".txt",sep=""),row.names=FALSE)
  
  ### Output plot of averaged spectra for quick reference
  dims <- dim(spec.avg)
  if(image=="TRUE" | settings.file$options$diagnostic.images=="TRUE"){
    if (dims[1]>0){
      plot.data <- as.matrix(spec.avg[,2:dims[2]])
      png(file=paste(out.dir,dlm,"Averaged_Spectra",".png",sep=""),width=800,height=600,res=100)
      matplot(seq(start.wave,end.wave,step.size), t(plot.data),cex=0.01,xlim=c(350,2500),ylim=c(0,1),
              xlab="Wavelength (nm)",ylab="Reflectance (%)", main="Averaged Spectra",
              cex.axis=1.3,cex.lab=1.3, type="l",lwd=1.5)
      abline(h=bias,lty=2,col="dark grey")
      box(lwd=2.2)
      dev.off()
    }
  }
  
  setTxtProgressBar(pb, 100)  # show progress bar
  flush.console()             #<--- show output in real-time
  close(pb)
  print("")
  
  ### Return dataframe of spectra if requested
  invisible(spec.avg)
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################