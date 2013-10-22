#--------------------------------------------------------------------------------------------------#
##'
##' A function to import raw ASD binary files to ascii flat file format
##'
##' @name read.asd
##' @title A function to import raw ASD binary files
##' @param file.dir A single ASD binary file or directory of ASD files to import.  Currently
##' only supports single directory processing (i.e. no nested dir structures)
##' @param out.dir Main output directory for processed spectra files.  If not set then no output ASCII files 
##' are provided. If set then output ASCII files for each spectra are written to out.dir/ascii_files
##' @param spec.type Optional. Option to set the type of spectra being processed.  
##' Options: "Reflectance" or "Transmittance"  Defaults to "Reflectance"
##' @param start.wave Optional. Selected starting wavelength of ASD binary spectra files.  Depends on instrument.
##' If not set then read from file header
##' @param end.wave Optional. Selected ending wavelength of ASD binary spectra files.  Depends on instrument.  
##' If not set then read from file header
##' @param step.size Optional. Wavelength step size for ASD files. E.g. 1nm, 5nm, 10nm
##' If not set then read from file header.  If selected for larger size than raw data, spectrum is interpolated
##' (not yet availible)
##' @param image Logical. Whether to produce .png images of each spectrum (TRUE) or not (FALSE).
##' Default is FALSE.  Useful for diagnosing spectral observations during processing.
##' @param spec.file.ext file extension of ASD files.  Usually ".asd" (Default)
##' @param output.file.ext optional setting to set file extension to output files. Defaults to .csv
##' @param get.metadata Logical. Run extract.metadata when importing .asd files and place information
##' in a metadata file? TRUE/FALSE.  Default is TRUE
##' @param settings.file settings file used for spectral processing options (OPTIONAL).  
##' Contains information related to the spectra collection instrument, output directories, 
##' and processing options such as applying a jump correction to the spectra files.  Options in the settings
##' file take precedent over options selected in the function call.
##'
##'@examples
##' # Set input file
##' file.dir <- system.file("extdata/PM01_TIAM_B_LC_REFL00005.asd",package="FieldSpec")
##' spec <- read.asd(file.dir,out.dir='~',start.wave=350,end.wave=2500,step.size=1)
##' 
##' # Get info from file header
##' spec <- read.asd(file.dir,out.dir='~')
##' 
##' # Plot output
##' plot(spec$Wavelength,spec$Spectra,type="l",lwd=2,xlab="Wavelength (nm)", ylab="Reflectance (%)")
##' 
##' 
##' @export
##' 
##' @return output for a single file returns a list with wavelengths and measured reflectance or transmittance 
##' values.  For a directory, outputs individual ascii text files with wavelength and spectra values for each input
##' ASD file.
##' 
##' @author Shawn P. Serbin
##'
read.asd <- function(file.dir=NULL,out.dir=NULL,spec.type=NULL,start.wave=NULL,end.wave=NULL,
                     step.size=NULL,image=FALSE,spec.file.ext=".asd",output.file.ext=".csv",
                     get.metadata=TRUE,settings.file=NULL){

  ## TODO: Reformat function for speed.  Read in all data to array then generate output spec and images.
  # Rathter than reading/writing for each file in serial
  ## Allow output for single spec files
  
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  ### Check for proper input
  if (is.null(settings.file) && is.null(file.dir)){
    stop("No input file directory given in settings file or function call.  Please correct.")
  } else if (!is.null(file.dir)){
    file.dir <- file.dir
  } else if (!is.null(settings.file$spec.dir)){
    file.dir <- settings.file$spec.dir
  } 
  
  ### create output directory if it doesn't already exist and is set
  if (!is.null(out.dir)) {
    out.dir <- paste0(out.dir, dlm, "ascii_files/")
  } else if (!is.null(settings.file$output.dir)) {
    out.dir <- paste0(settings.file$output.dir, dlm, "ascii_files/")
  }
  if (!is.null(out.dir)) {
    if (!file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
  }
  
  ### Remove any previous or old output in out.dir
  if (!is.null(out.dir)) {
    unlink(list.files(out.dir, full.names=TRUE), recursive=FALSE, force=TRUE)
  }
  
  ### Select optional spectra type for plotting
  if (!is.null(spec.type)) {
    s.type <- c("Reflectance","Transmittance")
    index <- agrep(pattern=temp,c("reflectance","transmittance"),ignore.case = TRUE)
    spec.type <- s.type[index]
  } else {
    spec.type <- "Reflectance"
  }
  
  ### Check for custom input spec file extension
  if (is.null(settings.file$options$spec.file.ext) && is.null(spec.file.ext)){
    warning("No input spectra file extension given. Using default .asd")
    spec.file.ext <- ".asd"
  } else if (!is.null(spec.file.ext)) {
    spec.file.ext <- spec.file.ext
  } else if (!is.null(settings.file$options$spec.file.ext)) {
    spec.file.ext <- settings.file$options$spec.file.ext
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
  
  #--------------------- Begin function -----------------------#
  
  ### Run extract.metadata
  #print(file.dir)
  extract.metadata(file.dir=file.dir,out.dir=gsub(pattern="ascii_files/","",out.dir),instrument="ASD",
                   spec.file.ext=spec.file.ext,output.file.ext=output.file.ext)
  
  ### Check whether passed a single file or a directory of files
  check <- file.info(file.dir)
  
  ### Run file import loop or single import.  First check if given a directory or a single file
  if (check$isdir){
    
    ### Get file list
    asd.files <- list.files(path=file.dir, pattern=spec.file.ext, full.names=FALSE)
    asd.files.full <- list.files(path=file.dir, pattern=spec.file.ext, full.names=TRUE)
    
    ### Define wavelengths using file header, if not defined already
    if (is.null(start.wave) | is.null(end.wave) | is.null(step.size)) {
      start.wave <- extract.metadata(file.dir=asd.files.full[1],instrument="ASD",spec.file.ext=spec.file.ext)$Calibrated_Starting_Wavelength
      step.size <- extract.metadata(file.dir=asd.files.full[1],instrument="ASD",spec.file.ext=spec.file.ext)$Calibrated_Wavelength_Step
      channels <- extract.metadata(file.dir=asd.files.full[1],instrument="ASD",spec.file.ext=spec.file.ext)$Detector_Channels
      end.wave <- start.wave+((channels-1)/step.size)
      lambda <- seq(start.wave,end.wave,step.size)
    } else {
      lambda <- seq(start.wave,end.wave,step.size)
    }
    
    ### Setup wavelengths for output
    waves <- paste0("Wave_", lambda)
      
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
      tmp <- unlist(strsplit(asd.files[i], paste0("\\", spec.file.ext)))   # <--- remove file extension from file name
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
      
      ### Output individual spectra files as ASCII text .csv file in output directory
      if (!is.null(out.dir)) {
        names(output.spectra) <- c("Wavelength", paste0(tmp[1]))
        output.spectra[, 1] <- lambda
        output.spectra[, 2] <- out.spec
        out.ascii <- paste0(tmp[1], output.file.ext)     # setup output file name
        
        ### Below for single output directory
        write.csv(output.spectra, paste0(out.dir, dlm, out.ascii), row.names=FALSE)
      }

      ### Output plot of spectra for quick reference
      # Create images if requested
      if(image=="TRUE" | settings.file$options$diagnostic.images=="TRUE"){
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
      }
      
      ### Display progress to console
      setTxtProgressBar(pb, j)                      # show progress bar
      j=j+1                                         # <--- increase counter by 1
      flush.console()                               #<--- show output in real-time
      
    } # End for loop
    
    ### Close progress bar
    close(pb)
        
  } else {
    asd.files <- list.files(path=file.dir, pattern=spec.file.ext, full.names=FALSE)
    to.read <- file(file.dir, "rb")
    
    ### Define wavelengths using file header, if not defined already
    if (is.null(start.wave) | is.null(end.wave) | is.null(step.size)) {
      start.wave <- extract.metadata(file.dir=file.dir[1],instrument="ASD",spec.file.ext=spec.file.ext)$Calibrated_Starting_Wavelength
      step.size <- extract.metadata(file.dir=file.dir[1],instrument="ASD",spec.file.ext=spec.file.ext)$Calibrated_Wavelength_Step
      channels <- extract.metadata(file.dir=file.dir[1],instrument="ASD",spec.file.ext=spec.file.ext)$Detector_Channels
      end.wave <- start.wave+((channels-1)/step.size)
      lambda <- seq(start.wave,end.wave,step.size)
    } else {
      lambda <- seq(start.wave,end.wave,step.size)
    }
    
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