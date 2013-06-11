#--------------------------------------------------------------------------------------------------#
##'
##' A function to import raw ASD binary files to ascii flat file format
##'
##' @name read.asd
##' @title A function to import raw ASD binary files
##' @param file.dir A single ASD binary file or directory of ASD files to import.  Currently
##' only supports single directory processing (i.e. no nested dir structures)
##' @param out.dir main output directory for processed spectra files.
##' @param start.wave starting wavelength of ASD binary spectra files.  Depends on instrument.
##' @param end.wave ending wavelength of ASD binary spectra files.  Depends on instrument.
##' @param step.size wavelength step size for ASD files. E.g. 1nm, 5nm, 10nm
##' @param asd.file.ext file extension of ASD files.  Usually ".asd" (Default)
##' @param output.file.ext optional setting to set file extension to output files. Defaults to .csv
##' @param settings.file settings file used for spectral processing options (OPTIONAL).  
##' Contains information related to the spectra collection instrument, output directories, 
##' and processing options such as applying a jump correction to the spectra files.  Options in the settings
##' file take precedent over options selected in the function call.
##'
##' @export
##' 
##' @return output for a single file returns a list with wavelengths and measured reflectance or transmittance 
##' values.  For a directory, outputs individual ascii text files with wavelength and spectra values for each input
##' ASD file.
##' 
##' @author Shawn P. Serbin
##'
read.asd = function(file.dir=NULL, out.dir=NULL, start.wave=NULL, end.wave=NULL, step.size=NULL, asd.file.ext=".asd",
                    output.file.ext=".csv", settings.file=NULL){

  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  ### Check for proper input
  if (is.null(settings.file) && is.null(file.dir)){
    stop("No input file directory given in settings file or function call.")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (!is.null(settings.file$asd.dir)){
    file.dir <- settings.file$asd.dir
  } 
    
  ### Define wavelengths
  if (is.null(settings.file) && is.null(start.wave)){
    stop(paste0("No starting wavelength set in settings file or in function call. Starting Wavelength is: ", start.wave))
  } else if (!is.null(settings.file$instrument$start.wave)){
    start.wave <- as.numeric(settings.file$instrument$start.wave)
  } else if (!is.null(start.wave)){
    start.wave <- start.wave
  }
  
  if (is.null(settings.file) && is.null(end.wave)){
    stop(paste0("No ending wavelength set in settings file or in function call. Ending Wavelength is: ", end.wave))
  } else if (!is.null(settings.file$instrument$end.wave)){
    end.wave <- as.numeric(settings.file$instrument$end.wave)
  } else if (!is.null(end.wave)){
    end.wave <- end.wave
  }
  
  if (is.null(settings.file) && is.null(step.size)){
      warning("No wavelength step size give in settings file or in function call. Setting to 1nm by default")
  } else if (!is.null(settings.file$instrument$step.size)){
    step.size <- as.numeric(settings.file$instrument$step.size)
  } else if (!is.null(step.size)){
    step.size <- step.size
  }
  
  ### Define wavelength numbers for processing/output based on defined start/end wavelengths and step size
  lambda <- seq(start.wave, end.wave, step.size)
  
  ### create output directory if it doesn't already exist
  if (!is.null(out.dir)) {
    out.dir <- out.dir
  } else if (!is.null(settings.file$output.dir)) {
    out.dir <- paste0(settings.file$output.dir, dlm, "ascii_files/")
  } else {
    ind <- gregexpr(dlm, file.dir)[[1]]
    out.dir <- paste0(substr(file.dir, ind[1], ind[length(ind)-1]-1), dlm, "ascii_files")
    if (!file.exists(out.dir)) dir.create(out.dir, recursive=TRUE)
  }
  if (!file.exists(out.dir)) dir.create(out.dir, recursive=TRUE)

  ### Remove any previous or old output in out.dir
  unlink(list.files(out.dir, full.names=TRUE), recursive=FALSE, force=TRUE)
  
  #--------------------- Begin function -----------------------#
  
  ### Check whether passed a single file or a directory of files
  check <- file.info(file.dir)
  
  ### Run file import loop or single import.  First check if given a directory or a single file
  if (check$isdir){

    ### Setup wavelengths for output
    waves <- paste0("Wave_", lambda)
      
    ### Get file list
    asd.files <- list.files(path=file.dir, pattern=asd.file.ext, full.names=FALSE)
    asd.files.full <- list.files(path=file.dir, pattern=asd.file.ext, full.names=TRUE)
    
    ### Setup progress bar
    j <- 1 # <--- Numeric counter for progress bar
    num.files <- length(asd.files)
    pb <- txtProgressBar(min = 0, max = num.files, char="*", width=70, style=3)
    
    ### Import data. Setup output arrays
    temp <- array(data=NA, dim=c(num.files, length(lambda) + 1))
    output.data.frame <- data.frame(temp)
    names(output.data.frame) <-c("Spectra", waves)
    output.spectra <- data.frame(array(data=NA, dim=c(length(lambda), 2)))
    
    for (i in 1:length(asd.files)){
      tmp <- unlist(strsplit(asd.files[i], paste0("\\", asd.file.ext)))   # <--- remove file extension from file name
      out.spec <- array(0, (end.wave-start.wave) + 1)   
      
      ### Open binary file for reading
      to.read = file(asd.files.full[i], "rb")

      ### Get measured radiance/reflectance data
      seek(to.read, where=484, origin="start", rw="r")
      meas <- rep(0, length(lambda))
      
      ### Check whether file is an older ASD file.  Should update this.
      meas = readBin(to.read, what=double(), n=length(lambda), size=4, endian = .Platform$endian)
      if (range(meas, na.rm=TRUE)[2] < 2){ # Less than 2 for WHITE refl.
        close(to.read)
        out.spec = array(meas)      	### Output spectra array.  This is the resulting spectra
        
      } else {
        
        ### Get measured radiance data
        seek(to.read, where=484, origin="start", rw="r")
        meas <- rep(0, length(lambda))
        meas <- readBin(to.read, what=double(), n=length(lambda), size=8, endian = .Platform$endian)
        
        ### Determine length of comment in the file, if present.      
        ### Used to skip this number of characters before reading
        ### reference radiance values
        comment <- "a"    # Allocate variable
        seek(to.read, where=0, origin="start", rw="r")
        comment <- readBin(to.read, character())
        if (nchar(comment) > 3){
          #skip=nchar(comment)
          skip <- nchar(comment) - 3  # Factory name is 3 characters long
        } else {
          skip <- 0
        }
        
        ### Get reference radiance
        seek(to.read, where=17712 + skip, origin="start", rw="r")
        ref <- rep(0, length(lambda))

        ref <- readBin(to.read, what=double(), n=length(lambda), size=8, endian = .Platform$endian)
        
        ### Close connection to binary file i
        close(to.read)
        
        ### Calculate refl or trans and setup output
        out.spec <- array(meas/ref)    		# Output spectra array
        out.spec <- as.vector(out.spec)
        
      } # End of internal if/else
      
      ### Build output dataframe of spectra files
      output.data.frame[i, 1] <- tmp
      output.data.frame[i, 2:(length(out.spec) + 1)] <- out.spec
      
      ### Output individual spectra file as ASCII text .csv file in output directory
      names(output.spectra) <- c("Wavelength", paste0(tmp[1]))
      output.spectra[, 1] <- lambda
      output.spectra[, 2] <- out.spec
      out.ascii <- paste0(tmp[1], output.file.ext)     # setup output file name
      
      ### Below for single output directory
      write.csv(output.spectra, paste0(out.dir, dlm, out.ascii), row.names=FALSE)
      
      ### Output plot of spectra for quick reference
      # First setup plot bounds
      rng <- range(output.spectra[, 2])
      if (rng[1]<0) rng[1] <- 0
      if (rng[2]>1) rng[2] <- 1
      ylimit <- c(rng[1], rng[2])
      png(file=paste0(out.dir, dlm, tmp[1], ".png"), width=800, height=600, res=100)
      plot(output.spectra[, 1], output.spectra[, 2], cex=0.01, xlim=c(350, 2500), ylim=ylimit, xlab="Wavelength (nm)",
           ylab="Reflectance (%)", main=out.ascii, cex.axis=1.3, cex.lab=1.3)
      lines(output.spectra[, 1], output.spectra[, 2], lwd=2)
      box(lwd=2.2)
      dev.off()
      
      ### Display progress to console
      setTxtProgressBar(pb, j)                      # show progress bar
      j=j+1                                         # <--- increase counter by 1
      flush.console()                               #<--- show output in real-time
      
    } # End for loop
    
    ### Close progress bar
    close(pb)
    
    ### Return spectra data object if requested i.e. spec.dataframe=TRUE
    
  } else {
    to.read <- file(file.dir, "rb")
    
    ### Get measured radiance/reflectance data
    seek(to.read, where=484, origin="start", rw="r")
    meas <- rep(0, length(lambda))
    
    ### Check whether file is an older ASD file
    meas <- readBin(to.read, what=double(), n=length(lambda), size=4, endian = .Platform$endian)
    if (range(meas, na.rm=TRUE)[2] < 2){ # Less than 2 for WHITE refl.
      close(to.read)
      out.spec = array(meas)    		# Output spectra array
      
    } else {
      
      ### Get measured radiance data
      seek(to.read, where=484, origin="start", rw="r")
      meas <- rep(0, length(lambda))
      meas <- readBin(to.read, what=double(), n=length(lambda), size=8, endian = .Platform$endian)
      
      ### Determine length of comment in the file, if present.      
      ### Used to skip this number of characters before reading
      ### reference radiance values
      comment <- "a"    # Allocate variable
      seek(to.read, where=0, origin="start", rw="r")
      comment <- readBin(to.read, character())
      if (nchar(comment) > 3){
        #skip=nchar(comment)
        skip <- nchar(comment) - 3  # Factory name is 3 characters long
      }else{
        skip <- 0
      }
      
      # Get reference radiance
      seek(to.read, where=17712+skip, origin="start", rw="r")
      ref <- rep(0, length(lambda))

      ref <- readBin(to.read, what=double(), n=length(lambda), size=8, endian = .Platform$endian)
      close(to.read)
      
      # Calculate refl or trans and setup output
      out.spec <- array(meas/ref)  			# Output spectra array
      
    } # End if/else
    
    output.data.frame <- list(Wavelength=lambda, Spectra=out.spec)  # Spectral data
  } # End of if/else loop for files
  invisible(output.data.frame)
} # End of function call
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################