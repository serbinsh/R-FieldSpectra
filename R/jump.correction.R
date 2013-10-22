#--------------------------------------------------------------------------------------------------#
##'
##' A function to apply a jump (splice) correction to imported spectra files
##'
##' @name jump.correction
##' @title apply a jump (splice) correction to imported ASD spectra files. This splice or jump
##' occurs at the boundaries between detectors
##' 
##' @param file.dir directory of spectra files to process
##' @param out.dir output directory for processed spectra files
##' @param spec.type [Optional] Option to set the type of spectra being processed.  
##' Options: "Reflectance" or "Transmittance"  Defaults to "Reflectance"
##' @param start.wave starting wavelength of spectra files. Not needed if specified in XML settings file.
##' @param end.wave ending wavelength of spectra files. Not needed if specified in XML settings file. 
##' @param step.size resolution of spectra files. E.g. 1 for 1nm, 5 for 5nm. Not needed if specified in settings file.
##' @param jumploc1 Wavelength location of the first jump in the spectra to correct. Not needed if 
##' specified in XML settings file.
##' @param jumploc2 Wavelength location of the second jump in the spectra to correct. Not needed if
##' specified in XML settings file.
##' @param firstJumpMax maximum jump threshold for the first jump location. Determines whether spectra
##' will be corrected or flaged as bad. (Optional.  Default is 0.02)
##' @param secondJumpMax maximum jump threshold for the second jump location. Determines whether spectra
##' will be corrected or flaged as bad. (Optional.  Default is 0.02)
##' @param output.file.ext option to set file extension of the output files. Defaults to .csv
##' @param metadata.file Option to select custom metadata file for use in processing. If not set
##' then the information is either read from default metadata file, the settings file or at the function call. 
##' Need to set this as the full qualified path to the spectral metadata file is using a custom file/location
##' @param image Logical. Whether to produce .png images of each spectrum (TRUE) or not (FALSE).
##' Default is FALSE.  Useful for diagnosing spectral observations during processing.
##' @param settings.file settings file used for spectral processing options (OPTIONAL).  
##' Contains information related to the spectra collection instrument, output directories, 
##' and processing options such as applying a jump correction to the spectra files.  
##'
##' @return output list containing processed spectra and associated diagnostic information
##'
##' @examples
##' \dontrun{
##' jump.correction(file.dir,out.dir='~', start.wave=350,end.wave=2500,step.size=1,jumploc1=651,jumploc2=1451,
##' output.file.ext=".csv",settings.file=NULL)
##' }
##'
##' @export
##'
##' @author Shawn P. Serbin
##'
jump.correction <- function(file.dir=NULL,out.dir=NULL,spec.type=NULL,start.wave=NULL,end.wave=NULL,step.size=NULL,
                           jumploc1=NULL,jumploc2=NULL,firstJumpMax=NULL,secondJumpMax=NULL,
                           output.file.ext=NULL,metadata.file=NULL,image=FALSE,settings.file=NULL){
  
  # TODO: implement the use of a settings file in this function. Allow adjustment of 
  # JC thresholds (below)
  # Allow running of function on single file
  # Allow for no written output, just returning of dataframe
  # Clean up function, improve generality
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?

  ### Check for proper input directory
  if (is.null(settings.file$output.dir) && is.null(file.dir)){
    stop("ERROR: No input file directory given in settings file or function call")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (!is.null(settings.file$output.dir)){
    file.dir <- paste(settings.file$output.dir,dlm,"ascii_files/",sep="")
  } 
  
  ### create output directory if it doesn't already exist
  if (!is.null(out.dir)) {
    out.dir <- out.dir
  } else if (!is.null(settings.file$output.dir)) {
    out.dir <- paste(settings.file$output.dir,dlm,"jc_files/",sep="")
  } else {
    ind <- gregexpr(dlm, file.dir)[[1]]
    out.dir <- paste(substr(file.dir,ind[1], ind[length(ind)-1]-1),dlm,"jc_files",sep="")
  }
  if (!is.null(out.dir)) {
    if (!file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
  }
  
  ### Create bad spectra folder. Spectra not corrected
  if (!is.null(out.dir)) {
    badspec.dir <- paste(out.dir,dlm,"Bad_Spectra",sep="")
    if (! file.exists(badspec.dir)) dir.create(badspec.dir,recursive=TRUE)
  }
  
  ### Remove any previous or old output in out.dir
  if (!is.null(out.dir)) {
    unlink(list.files(out.dir,full.names=TRUE),recursive=FALSE,force=TRUE)
    unlink(list.files(badspec.dir,full.names=TRUE),recursive=TRUE,force=TRUE)
  }
  
  ### Select optional spectra type for plotting
  if (!is.null(spec.type)) {
    s.type <- c("Reflectance","Transmittance")
    index <- agrep(pattern=temp,c("reflectance","transmittance"),ignore.case = TRUE)
    spec.type <- s.type[index]
  } else {
    spec.type <- "Reflectance"
  }
  
  ### Look for a custom output extension, otherwise use default
  if (is.null(settings.file$options$output.file.ext) && is.null(output.file.ext)){
    output.file.ext <- ".csv"  # <-- Default
  } else if (!is.null(output.file.ext)){
    output.file.ext <- output.file.ext
  } else if (!is.null(settings.file$options$output.file.ext)){
    output.file.ext <- settings.file$options$output.file.ext
  } 
  
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

  ### Find files to process
  ascii.files <- list.files(path=file.dir,pattern=output.file.ext,full.names=FALSE)
  num.files  <- length(ascii.files)
  #asd.files.full <- list.files(path=settings$spec.dir, pattern=spec.file.ext, full.names=TRUE)
  
  ### Check whether files exist. STOP if files missing and display an error
  if (num.files<1){
    stop(paste("No spec files found in directory with extension: ",output.file.ext,sep=""))
  }
  
  ### Spectra metadata.
  if (is.null(metadata.file)) {
    metadata.dir <- gsub(pattern="jc_files/","",out.dir)
    metadata.file <- list.files(path=metadata.dir,pattern="metadata",full.names=FALSE)
    print(paste("------- Using metadata file: ",metadata.file,sep=""))
    metadata <- read.csv(paste(settings.file$output.dir,dlm,metadata.file,sep=""))
  } else {
    metadata <- read.csv(metadata.file,header=T)
  }

  ### Display info to the terminal
  tmp  <- unlist(strsplit(file.dir,dlm))
  current <- tmp[length(tmp)]
  print(paste("------- Processing directory: ",current))
  print(paste("------- Number of files: ",num.files))
  flush.console() #<--- show output in real-time
  
  #--------------------- Setup function -----------------------#
  # Create file info list for putput
  info <- data.frame(Spectra=rep(NA,num.files),Jump1_Size = rep(NA,num.files),
                    Jump2_Size = rep(NA,num.files),Corrected = rep(NA,num.files))
  names(info) <- c("Spectra","Jump1_Size","Jump2_Size","Corrected?")

  #*****************************************************************************************************
  ### Thresholds for correction
  firstJumpMin <- 0.0; # default
  if (is.null(settings.file$options$firstJumpMax) && is.null(firstJumpMax)){
    firstJumpMax <- 0.02; # set default
  } else if (!is.null(firstJumpMax)) {
    firstJumpMax <- firstJumpMax; # else use option set in function call
  } else if (!is.null(settings.file$options$firstJumpMax)) {
    firstJumpMax <- settings.file$options$firstJumpMax; # use settings file
  }
  secondJumpMin <- 0.0; # default
  if (is.null(settings.file$options$secondJumpMax) && is.null(secondJumpMax)){
    secondJumpMax <- 0.02; # set default
  } else if (!is.null(secondJumpMax)) {
    secondJumpMax <- secondJumpMax; # else use option set in function call
  } else if (!is.null(settings.file$options$secondJumpMax)) {
    secondJumpMax <- settings.file$options$secondJumpMax; # use settings file
  }
  
  ### Jump locations.  If set in function arg or settings file
  # Jump 1
  if (!is.null(jumploc1)) {
    jumploc1 <- jumploc1 # use option set in function call
  } else if (!is.null(settings.file$instrument$jumploc1)) {
    jumploc1 <- as.numeric(settings.file$instrument$jumploc1) # use settings file
    jumploc1 <- (jumploc1-350)+1 # Change to index
  }

  # Jump 2
  if (!is.null(jumploc2)) {
    jumploc2 <- jumploc2 # use option set in function call
  } else if (!is.null(settings.file$instrument$jumploc2)) {
    jumploc2 <- as.numeric(settings.file$instrument$jumploc2) # use settings file
    jumploc2 <- (jumploc2-350)+1 # Change to index
  }
  #*****************************************************************************************************
  
  
  #-------------------------- Start JC loop --------------------------#
  j <- 1 # <--- Numeric counter for progress bar
  pb <- txtProgressBar(min = 0, max = num.files, char="*",width=70,style = 3)
  for (i in 1:num.files){
    
    # Spec name
    tmp <- unlist(strsplit(ascii.files[i],paste("\\",output.file.ext,sep="")))  # <--- remove file extension from file name
    
    #---------------- Setup Wavelengths --------------------------------#
    if (is.null(start.wave) | is.null(end.wave) | is.null(step.size)) {
      # Using metadata info
      start.wave <- metadata[which(metadata$Spectra_File_Name==tmp),]$Calibrated_Starting_Wavelength
      step.size <- metadata[which(metadata$Spectra_File_Name==tmp),]$Calibrated_Wavelength_Step
      channels <- metadata[which(metadata$Spectra_File_Name==tmp),]$Detector_Channels
      end.wave <- start.wave+((channels-1)/step.size)
      lambda <- seq(start.wave,end.wave,step.size)
    } else {
      # Using settings file or function argument
      lambda <- seq(start.wave,end.wave,step.size)
    }
    
    #---------------- Setup Jump Location -----------------------------#
    if (is.null(jumploc1) | is.null(jumploc2)) {
      jumploc1 <- metadata[which(metadata$Spectra_File_Name==tmp),]$VNIR_SWIR1_Splice
      jumploc1 <- (jumploc1-350)+1 # Change to index
      jumploc2 <- metadata[which(metadata$Spectra_File_Name==tmp),]$SWIR1_SWIR2_Splice
      jumploc2 <- (jumploc2-350)+1 # Change to index
    } else {
      # Keep previously set values
      jumploc1 <- jumploc1
      jumploc2 <- jumploc2
    }
    
    ### Define wavelengths for correction of first jump
    jmp1Loc1 <- jumploc1-1
    jmp1Loc2 <- jumploc1
    jmp1Loc3 <- jumploc1+1
    jmp1Loc4 <- jumploc1+2
    
    ### Define wavelengths for correction of second jump
    jmp2Loc1 <- jumploc2-1
    jmp2Loc2 <- jumploc2
    jmp2Loc3 <- jumploc2+1
    jmp2Loc4 <- jumploc2+2
    #-----------------------------------------------------------------#
    
    ### Read in spec file
    out.spec <- array(0,(end.wave-start.wave)+1)       # refl or trans
    spec.file <- read.csv(paste(file.dir,dlm,ascii.files[i],sep=""))
    spectra <- spec.file[,2]
    zero.chk <- sum(spectra)

    #---------------- Apply jump correction to spectra ----------------#
    # Check size of jumps against thresholds
    jmp1Chk.min <- abs(spectra[jmp1Loc3]-spectra[jmp1Loc2])>firstJumpMin
    jmp1Chk.max <- round(abs(spectra[jmp1Loc3]-spectra[jmp1Loc2]),3)>firstJumpMax
    jmp1sz <- abs(spectra[jmp1Loc3]-spectra[jmp1Loc2])
    
    jmp2Chk.min <- abs(spectra[jmp2Loc3]-spectra[jmp2Loc2])>secondJumpMin
    jmp2Chk.max <- round(abs(spectra[jmp2Loc3]-spectra[jmp2Loc2]),3)>secondJumpMax
    jmp2sz <- abs(spectra[jmp2Loc3]-spectra[jmp2Loc2])
    
    # Set flags.  Check if jump exceeds threshold
    jmp1flag <- jmp1Chk.max
    jmp2flag <- jmp2Chk.max
    
    if (jmp1flag==TRUE | jmp2flag==TRUE | zero.chk==0){
      
      ### sum(spectra)==0 for spec that are all 0
      
      # Don't apply correction.  Output original spectra.
      jc <- spectra
      c1 <- 1
      c2 <- 1
      
    } else{
      
      # ------------------- Apply jump corrections ------------------- #
      # jump 1
      hold1   <- array(0,(end.wave-start.wave)+1)
      hold2   <- array(0,(end.wave-start.wave)+1)
      jc      <- array(0,(end.wave-start.wave)+1)
      temp1   <- spectra[jmp1Loc2]-spectra[jmp1Loc1]
      temp2   <- spectra[jmp1Loc4]-spectra[jmp1Loc3]
      Avg     <- (temp1+temp2)/2
      Prime   <- Avg + spectra[jmp1Loc2]
      c1      <- Prime/spectra[jmp1Loc3]
      
      hold1[jmp1Loc3:length(hold2)] <- spectra[jmp1Loc3:length(hold2)]*c1
      hold2[1:jmp1Loc2]             <- spectra[1:jmp1Loc2]
      hold2[jmp1Loc3:length(hold2)] <- hold1[jmp1Loc3:length(hold2)]
      spectra2                      <- hold2
      
      # remove temporary variables
      rm(temp1,temp2,Avg,Prime,hold1,hold2)
      
      # jump 2
      hold1 <- array(0,(end.wave-start.wave)+1)
      hold2 <- array(0,(end.wave-start.wave)+1)
      temp1 <- spectra2[jmp2Loc2]-spectra2[jmp2Loc1]
      temp2 <- spectra2[jmp2Loc4]-spectra2[jmp2Loc3]
      Avg   <- (temp1+temp2)/2
      Prime <- Avg + spectra2[jmp2Loc2]
      c2    <- Prime/spectra2[jmp2Loc3]
      
      hold1[jmp2Loc3:length(hold2)] <- spectra2[jmp2Loc3:length(hold2)]*c2
      hold2[1:jmp2Loc2]             <- spectra2[1:jmp2Loc2]
      hold2[jmp2Loc3:length(hold2)] <- hold1[jmp2Loc3:length(hold2)]
      spectra3                      <- hold2
      
      # Recombine into corrected spectra
      jc[1:jmp2Loc2]                <- spectra2[1:jmp2Loc2]
      jc[jmp2Loc3:length(jc)]       <- spectra3[jmp2Loc3:length(jc)]
      
      # remove temporary variables
      rm(spectra,spectra2,spectra3,temp1,temp2,Avg,Prime,hold1,hold2)
      #------------------------------------------------------------------#

    } ### End if/else
    
    out.spec <- data.frame(Wavelength=lambda,Spectra=jc)
    names(out.spec) <- c("Wavelength",paste(tmp[1],sep=""))
    out.filename <- paste(tmp[1],output.file.ext,sep="")
      
    # -------------------- Finish up for spectra file i --------------------#
    # Examine output from jump correction.  Determine if spectra was corrected.
    
    ### Not corrected spectra
    if (jmp1flag=="TRUE" | jmp2flag=="TRUE" | zero.chk==0){
      info[j,1] <- tmp[1]
      info[j,2:3] <- data.frame(jmp1sz,jmp2sz)
      info[j,4] <- "No" # <--- Not corrected
        
      # Output uncorrected spectra
      write.csv(out.spec,paste(badspec.dir,dlm,out.filename,sep=""),row.names=FALSE)
        
      # Output plot of uncorrected spectra for quick reference
      # Create output images if requested
      if(image=="TRUE" | settings.file$options$diagnostic.images=="TRUE"){
        rng <- range(out.spec[,2])
        if (rng[1]<0) rng[1] <- 0
        if (rng[2]>1) rng[2] <- 1
        ylimit <- c(rng[1],rng[2])
        png(file=paste(badspec.dir,dlm,tmp[1],".png",sep=""),width=800,height=600,res=100)
        plot(out.spec[,1], out.spec[,2],cex=0.01,xlim=c(350,2500),ylim=ylimit,xlab="Wavelength (nm)",
              ylab="Reflectance (%)", main=out.filename,cex.axis=1.3,cex.lab=1.3)
        lines(out.spec[,1], out.spec[,2],lwd=2)
        box(lwd=2.2)
        dev.off()
        rm(rng)
      }
      
      ### Corrected spectra
    } else {
      info[j,1] <- tmp[1]
      info[j,2:3] <- data.frame(jmp1sz,jmp2sz)
      info[j,4] <- "Yes" # <--- Corrected
        
      # Output corrected spectra
      write.csv(out.spec,paste(out.dir,dlm,out.filename,sep=""),row.names=FALSE)
        
      # Output plot of spectra for quick reference
      # Create output images if requested
      if(image=="TRUE" | settings.file$options$diagnostic.images=="TRUE"){
        rng <- range(out.spec[,2])
        if (rng[1]<0) rng[1] <- 0
        if (rng[2]>1) rng[2] <- 1
        ylimit <- c(rng[1],rng[2])
        png(file=paste(out.dir,dlm,tmp[1],".png",sep=""),width=800,height=600,res=100)
        plot(out.spec[,1], out.spec[,2],cex=0.01,xlim=c(350,2500),ylim=ylimit,xlab="Wavelength (nm)",
              ylab="Reflectance (%)", main=out.filename,cex.axis=1.3,cex.lab=1.3)
        lines(out.spec[,1], out.spec[,2],lwd=2)
        box(lwd=2.2)
        dev.off()
        rm(rng)
      }
    }  ### End if/else
    
      setTxtProgressBar(pb, j)                      # show progress bar
      j=j+1                                         # <--- increase counter by 1
      flush.console()                               #<--- show output in real-time
    
    ### Remove temp vars
    rm(zero.chk,out.spec,tmp,spec.file,jmp1flag,jmp2flag,jc)
        
  } ### End jc loop
  close(pb)
  
  ### Output diagnostic info to jc directory
  write.csv(info,paste(out.dir,dlm,"Spectra_Diagnostics.csv",sep=""),row.names=FALSE)
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################