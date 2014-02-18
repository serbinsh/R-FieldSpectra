#--------------------------------------------------------------------------------------------------#
##'
##' Function to average replicate spectra within a directory of spectra files
##'
##' @name average.spec.se
##' @title average replicate spectra within a directory of spectra files
##' @param file.dir directory of spectra files to process
##' @param out.dir output directory for processed spectra files
##' @param spec.type Option to set what type of spectra to average. 
##' Options: Reflectance, Transmittance.  Can be set with abbreviations: e.g. "Refl" or "Tran"
##' Default is "Reflectance"
##' @param spec.file.ext Optional to set the input file extension.  Default is ".sed"
##' @param start.wave starting wavelength of spectra files. 
##' Not needed if specified in XML settings file.
##' @param end.wave ending wavelength of spectra files. Not needed if 
##' specified in XML settings file. 
##' @param step.size resolution of spectra files. E.g. 1 for 1nm, 5 for 5nm. 
##' Not needed if specified in XML settings file.
##' @param bias.threshold reflectance/transmittance cutoff to remove spectra with anartificial 
##' bias (shift) due to improper spectral collection
##' @param suffix.length length of auto numbering attached to ASD file names.  This number of 
##' characters will be removed from the filename when averaged.
##' @param output.file.ext optional setting to set file extension of output files. Defaults to .csv
##' @param settings.file settings file used for spectral processing options (OPTIONAL).  
##' Contains information related to the spectra collection instrument, output directories, 
##' and processing options such as applying a jump correction to the spectra files.  Options in the settings
##' file take precedent over options selected in the function call.
##' 
##' @export
##'
##' @author Shawn P. Serbin
##'
average.spec.se <- function(file.dir=NULL,out.dir=NULL,spec.type=NULL,spec.file.ext=NULL,start.wave=NULL,
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
  
  ### Select optional spectra type for plotting
  if (!is.null(spec.type)) {
    s.type <- c("Reflectance","Transmittance")
    index <- agrep(pattern=temp,c("reflectance","transmittance"),ignore.case = TRUE)
    spec.type <- s.type[index]
  } else {
    spec.type <- "Reflectance"
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

  ### Create bad spectra folder. Spectra not used in averages
  badspec.dir <- paste(out.dir,dlm,"Bad_Spectra",sep="")
  if (! file.exists(badspec.dir)) dir.create(badspec.dir,recursive=TRUE)
  
  ### Create white reference folder.
  whiteref.dir <- paste(out.dir,dlm,"White_Reference_Spectra",sep="")
  if (! file.exists(whiteref.dir)) dir.create(whiteref.dir,recursive=TRUE)
  
  ### Remove any previous output in out.dir
  unlink(list.files(out.dir,full.names=TRUE),recursive=FALSE,force=TRUE)
  unlink(list.files(badspec.dir,full.names=TRUE),recursive=TRUE,force=TRUE)
  unlink(list.files(whiteref.dir,full.names=TRUE),recursive=TRUE,force=TRUE)
  
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
  se.files <- list.files(path=file.dir,pattern=spec.file.ext,
                         full.names=FALSE)
  num.files <- length(se.files)
  
  ### Check whether files exist. STOP if files missing and display an error
  if (num.files<1){
    stop(paste0("No files found in directory with extension: ",spec.file.ext,sep=""))
  }
  
  ### Create file info list for output
  info = data.frame(Spectra=rep(NA,num.files),WhiteRef=rep(NA,num.files),Threshold_Check = rep(NA,num.files),
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
  
  # Grab wavelength info from files if not given
  if (is.null(start.wave) | is.null(end.wave) | is.null(step.size)) {
    wave.range <- as.character(extract.metadata(file.dir=paste0(file.dir,"/",se.files[1]),spec.file.ext=spec.file.ext)$Wavelength_Range)
    channels <- as.numeric(as.character(droplevels(extract.metadata(file.dir=paste0(file.dir,"/",se.files[1]),spec.file.ext=spec.file.ext)$Detector_Channels)))
    start.wave <- as.numeric((strsplit(wave.range,"-")[[1]])[1])
    end.wave <- as.numeric((strsplit(wave.range,"-")[[1]])[2])
    step.size <- ((end.wave-start.wave)+1)/channels
    lambda <- seq(start.wave,end.wave,step.size)
  } else {
    lambda <- seq(start.wave,end.wave,step.size)
  }
  
  ### Setup wavelengths for output
  waves <- paste("Wave_",lambda,sep="")
  
  # create empty array for output
  in.spec <- array(0,dim=c(num.files,
                           (end.wave-start.wave)+1)) # build empty array to populate with spectra files  
  spec.names <- unlist(strsplit(se.files,paste("\\",spec.file.ext,sep="")))
  spec.names.orig <- spec.names
  spec.names <- substr(spec.names,1,nchar(spec.names)-suffix.length)
  
  ### Read in sed files for averaging
  data.columns <- as.numeric(as.character(droplevels(extract.metadata(file.dir=paste0(file.dir,"/",se.files[1]),
                                                                      spec.file.ext=spec.file.ext)$Num_Data_Columns)))
  data.line <- extract.metadata(file.dir=paste0(file.dir,"/",se.files[1]),spec.file.ext=spec.file.ext)$Data_Line
  for (i in 1:num.files){ 
    spec.units <- as.character(extract.metadata(file.dir=paste0(file.dir,"/",se.files[i]),spec.file.ext=spec.file.ext)$Spec_Units)
    spec.file <- read.table(paste(file.dir,dlm,se.files[i],sep=""),header=F,skip=data.line+1)
    if (spec.units=="Percent"){
      in.spec[i,] <- (spec.file[,data.columns])*.01
    } else {
      in.spec[i,] <- spec.file[,data.columns]
    }
    rm(spec.units,spec.file)
  }
  
  ### Setup spectra for averaging.
  in.spec2 <- as.data.frame(in.spec)
  in.spec3 <- data.frame(Orig.Spec.Name=spec.names.orig,Spectra=spec.names,in.spec2)
  names(in.spec3) <- c("Orig.Spec.Name","Spectra",waves)

  print("")
  print(paste("----- Total number of spectral files: ",
              dim(in.spec3)[1],sep=""))
  print(paste("----- Total number of spectral samples: ",
              length(unique.spec <- unique(in.spec3$Spectra)),sep=""))
  print("")
  
  ## Update progress bar
  setTxtProgressBar(pb, 20)                      # show progress bar
  flush.console()                                #<--- show output in real-time
  
  ### Do initial checks on spectra
  # find white refs
  wht.ref <- which(rowMeans(in.spec2)>0.97) # should change this to an option.  I.e. allow user to select WR criteria
  rm(in.spec,in.spec2)
  wht.ref.spec <- droplevels(in.spec3[wht.ref,])
  
  # spec bias threshold
  bias <- bias.threshold
  # 450nm
  bad.spec.temp <- which(in.spec3$Wave_450>bias)
  bad.spec.temp <- bad.spec.temp[-which(bad.spec.temp %in% wht.ref)] # get rid of wr rows
  bad.spec <- droplevels(in.spec3[bad.spec.temp,])
  
  # Create good.spec data
  good.spec.temp <- which(in.spec3$Wave_450<=bias)
  good.spec.temp2 <- good.spec.temp[-which(good.spec.temp %in% wht.ref)] # get rid of wr rows
  #good.spec <- droplevels(in.spec3[which(in.spec3$Wave_450<=bias),])
  if (length(good.spec.temp2)>0){
    good.spec <- droplevels(in.spec3[good.spec.temp2,])  
    dims <- dim(good.spec)
    check <- length(wht.ref)+dim(good.spec)[1]+dim(bad.spec)[1]==dim(in.spec3)[1]
  } else {
    #good.spec <- droplevels(in.spec3[good.spec.temp,])
    good.spec <- wht.ref.spec
    dims <- dim(good.spec)
    check <- length(wht.ref)+dim(bad.spec)[1]==dim(in.spec3)[1]
  }              
  #dims <- dim(good.spec)
  #check <- length(wht.ref)+dim(good.spec)[1]+dim(bad.spec)[1]==dim(in.spec3)[1]
  
  # temporary debugging flag
  if (check==FALSE) {
    stop("Error in checking spectra. White Ref + Good Spec + Bad Spec != Total spec")
  }
  
  ### Update diagnostic info
  for (i in 1:dim(in.spec3)[1]){
    info[i,1] <- as.character(droplevels(in.spec3[i,1]))
    if (as.character(droplevels(in.spec3[i,1])) %in% droplevels(in.spec3[wht.ref,1])) {
      info[i,2] <- "Yes"
      info[i,3] <- NA
    } else {
      info[i,2] <- "No"
      info[i,3] <- NA
    }
  }
  for (i in 1:dim(in.spec3)[1]){ 
    if (as.character(droplevels(in.spec3[i,1])) %in% droplevels(bad.spec[,1])){
      info[i,3] <- "Failed"
      info[i,4] <- as.numeric(round(in.spec3[i,"Wave_450"],4))
      info[i,5] <- "No"
    } else {
      info[i,3] <- "Passed"
      info[i,4] <- as.numeric(round(in.spec3[i,"Wave_450"],4))
    }
  }
  
  setTxtProgressBar(pb, 30)                      # show progress bar
  flush.console()                                #<--- show output in real-time
  
  ##### Initial average
  ind <- factor(as.numeric(good.spec$Spectra))
  mat.data <- as.matrix(good.spec[,3:dims[2]])
  if (dim(mat.data)[1]==0){
    stop("Bias threshold too strict, no remaining spectra to average. Please update with less strict value")
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
  
  ### Get spectra sdev  --- THIS IS A SLOW POINT. NEED TO WORK ON SPEEDING UP
  dims <- dim(good.spec)
  spec.sdev <- aggregate(.~Spectra,data=good.spec[,2:dims[2]],sd,simplify=TRUE)
  spec.sdev[is.na(spec.sdev)]=0
  spec.avg.names <- droplevels(spec.avg$Spectra)
  dims <- dim(spec.avg)
  rm(mat.data,ind)
  
  ### Setup spectral checks
  dims <- dim(spec.avg)
  n <- rle(as.numeric(good.spec$Spectra))$lengths
  spec.upper <- spec.avg[,2:dims[2]] + (spec.sdev[,2:dims[2]]*2) # 2*SD outlier check
  #spec.upper <- spec.avg[,2:dims[2]] + ((spec.sdev[,2:dims[2]]/sqrt(n))*2.96)
  spec.upper <- data.frame(Spectra=spec.avg.names,spec.upper)
  spec.lower <- spec.avg[,2:dims[2]] - (spec.sdev[,2:dims[2]]*2)
  #spec.lower <- spec.avg[,2:dims[2]] - ((spec.sdev[,2:dims[2]]/sqrt(n))*2.96)
  spec.lower <- data.frame(Spectra=spec.avg.names,spec.lower)
  
  ### For debugging
  #dims <- dim(spec.avg)
  #plot(unlist(spec.avg[1,2:dims[2]]),type="l",lwd=2,ylim=c(0,0.8))
  #lines(unlist(spec.upper[1,2:dims[2]]),lty=2)
  #lines(unlist(spec.lower[1,2:dims[2]]),lty=2)
  #dims <- dim(good.spec)
  #lines(unlist(good.spec[7,3:dims[2]]),lty=1)
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
    window <- 151:1751 # 500 - 2100
    if (any(good.spec2[i,window]>spec.upper[ind.avg,window]) || 
          any(good.spec2[i,window] < spec.lower[ind.avg,window])) {
      bad.spec <- rbind(bad.spec,good.spec[i,])
      good.spec[i,] <- NA
      
      # update diagnostic info
      loc <- which(orig.spec.name==info$Spectra,arr.ind=TRUE)
      info[loc,5] <- "Yes"
      
    } else {
      # update diagnostic info
      loc <- which(orig.spec.name==info$Spectra,arr.ind=TRUE)
      info[loc,5] <- "No"
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
      spec.name <- gsub("-","_",droplevels(bad.spec[i,1]))
      out.spec <- t(bad.spec[i,3:dims[2]])
      out.spec <- data.frame(Wavelength=seq(start.wave,end.wave,step.size),
                             Spectra=out.spec)
      names(out.spec)=c("Wavelength",paste(spec.name))
      out.filename <- paste(spec.name,output.file.ext,sep="")
      write.csv(out.spec,paste(badspec.dir,dlm,out.filename,sep=""),row.names=FALSE)
      
      # Output plot of bad spectra for quick reference
      rng <- range(out.spec[,2])
      if (rng[1]<0) rng[1] <- 0
      if (rng[2]>1) rng[2] <- 1
      ylimit <- c(rng[1],rng[2])
      png(file=paste(badspec.dir,dlm,spec.name,".png",sep=""),width=800,height=600,res=100)
      plot(out.spec[,1], out.spec[,2],cex=0.01,xlim=c(350,2500),ylim=ylimit,xlab="Wavelength (nm)",
           ylab=paste(spec.type,"(%)",sep=" "), main=out.filename,cex.axis=1.3,cex.lab=1.3)
      lines(out.spec[,1], out.spec[,2],lwd=2)
      abline(h=bias,lty=2,col="dark grey")
      box(lwd=2.2)
      dev.off()
    } 
    rm(spec.name,out.spec,out.filename,dims)
  } # End of output of bad spectra
  
  # Output White Reference Spectra
  dims <- dim(wht.ref.spec)
  if (dims[1]>0){
    for (i in 1:dims[1]){
      spec.name <- gsub("-","_",droplevels(wht.ref.spec[i,1]))
      out.spec <- t(wht.ref.spec[i,3:dims[2]])
      out.spec <- data.frame(Wavelength=seq(start.wave,end.wave,step.size),
                             Spectra=out.spec)
      names(out.spec)=c("Wavelength",paste(spec.name))
      out.filename <- paste(spec.name,output.file.ext,sep="")
      write.csv(out.spec,paste(whiteref.dir,dlm,out.filename,sep=""),row.names=FALSE)
      
      # Output plot of white ref spectra for quick reference
      rng <- range(out.spec[,2])
      ylimit <- c(rng[1],rng[2])
      png(file=paste(whiteref.dir,dlm,spec.name,".png",sep=""),width=800,height=600,res=100)
      plot(out.spec[,1], out.spec[,2],cex=0.01,xlim=c(350,2500),ylim=ylimit,xlab="Wavelength (nm)",
           ylab=paste(spec.type,"(%)",sep=" "), main=out.filename,cex.axis=1.3,cex.lab=1.3)
      lines(out.spec[,1], out.spec[,2],lwd=2)
      abline(h=bias,lty=2,col="dark grey")
      box(lwd=2.2)
      dev.off()
    } 
    rm(spec.name,out.spec,out.filename,dims)
  } # End of output of bad spectra

  # Output individual averaged spec files
  dims <- dim(spec.avg)
  if (dims[1]>0){
    for (i in 1:dims[1]){
      spec.name <- gsub("-","_",droplevels(spec.avg[i,1]))
      out.spec <- t(spec.avg[i,2:dims[2]])
      out.spec <- data.frame(Wavelength=seq(start.wave,end.wave,step.size),
                             Spectra=out.spec)
      names(out.spec)=c("Wavelength",paste(spec.name))
      out.filename <- paste(spec.name,output.file.ext,sep="")
      write.csv(out.spec,paste(out.dir,dlm,out.filename,sep=""),row.names=FALSE)
      
      ### Output plot of average spectra for quick reference
      rng <- range(out.spec[,2])
      if (rng[1]<0) rng[1] <- 0
      if (rng[2]>1) rng[2] <- 1
      ylimit <- c(rng[1],rng[2])
      png(file=paste(out.dir,dlm,spec.name,".png",sep=""),width=800,height=600,res=100)
      plot(out.spec[,1], out.spec[,2],cex=0.01,xlim=c(350,2500),ylim=ylimit,xlab="Wavelength (nm)",
           ylab=paste(spec.type,"(%)",sep=" "), main=out.filename,cex.axis=1.3,cex.lab=1.3)
      lines(out.spec[,1], out.spec[,2],lwd=2)
      abline(h=bias,lty=2,col="dark grey")
      box(lwd=2.2)
      dev.off()
    }
    rm(spec.name,out.spec,out.filename,dims)
  }
  
  ### Write out concatenated averaged spectra
  spec.avg[,1] <- gsub("-","_",spec.avg[,1])
  write.csv(spec.avg,paste(out.dir,dlm,"Averaged_Spectra",output.file.ext,sep=""),row.names=FALSE)
  
  ### Output diagnostic info
  write.csv(info,paste(out.dir,dlm,"Spectra_Diagnostic_Information",".txt",sep=""),row.names=FALSE)
  
  ### Output plot of averaged spectra for quick reference
  dims <- dim(spec.avg)
  if (dims[1]>0){
    plot.data <- as.matrix(spec.avg[,2:dims[2]])
    png(file=paste(out.dir,dlm,"Averaged_Spectra",".png",sep=""),width=800,height=600,res=100)
    matplot(seq(start.wave,end.wave,step.size), t(plot.data),cex=0.01,xlim=c(350,2500),
            xlab="Wavelength (nm)",ylab=paste(spec.type,"(%)",sep=" "), main="Averaged Spectra",
            cex.axis=1.3,cex.lab=1.3, type="l",lwd=1.5)
    abline(h=bias,lty=2,col="dark grey")
    box(lwd=2.2)
    dev.off()
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