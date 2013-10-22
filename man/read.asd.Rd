\name{read.asd}
\alias{read.asd}
\title{A function to import raw ASD binary files}
\usage{
  read.asd(file.dir = NULL, out.dir = NULL,
    spec.type = NULL, start.wave = NULL, end.wave = NULL,
    step.size = NULL, image = FALSE,
    spec.file.ext = ".asd", output.file.ext = ".csv",
    get.metadata = TRUE, settings.file = NULL)
}
\arguments{
  \item{file.dir}{A single ASD binary file or directory of
  ASD files to import.  Currently only supports single
  directory processing (i.e. no nested dir structures)}

  \item{out.dir}{Main output directory for processed
  spectra files.  If not set then no output ASCII files are
  provided. If set then output ASCII files for each spectra
  are written to out.dir/ascii_files}

  \item{spec.type}{Optional. Option to set the type of
  spectra being processed. Options: "Reflectance" or
  "Transmittance" Defaults to "Reflectance"}

  \item{start.wave}{Optional. Selected starting wavelength
  of ASD binary spectra files.  Depends on instrument. If
  not set then read from file header}

  \item{end.wave}{Optional. Selected ending wavelength of
  ASD binary spectra files.  Depends on instrument. If not
  set then read from file header}

  \item{step.size}{Optional. Wavelength step size for ASD
  files. E.g. 1nm, 5nm, 10nm If not set then read from file
  header.  If selected for larger size than raw data,
  spectrum is interpolated (not yet availible)}

  \item{image}{Logical. Whether to produce .png images of
  each spectrum (TRUE) or not (FALSE). Default is FALSE.
  Useful for diagnosing spectral observations during
  processing.}

  \item{spec.file.ext}{file extension of ASD files.
  Usually ".asd" (Default)}

  \item{output.file.ext}{optional setting to set file
  extension to output files. Defaults to .csv}

  \item{get.metadata}{Logical. Run extract.metadata when
  importing .asd files and place information in a metadata
  file? TRUE/FALSE.  Default is TRUE}

  \item{settings.file}{settings file used for spectral
  processing options (OPTIONAL). Contains information
  related to the spectra collection instrument, output
  directories, and processing options such as applying a
  jump correction to the spectra files.  Options in the
  settings file take precedent over options selected in the
  function call.}
}
\value{
  output for a single file returns a list with wavelengths
  and measured reflectance or transmittance values.  For a
  directory, outputs individual ascii text files with
  wavelength and spectra values for each input ASD file.
}
\description{
  A function to import raw ASD binary files to ascii flat
  file format
}
\examples{
# Set input file
file.dir <- system.file("extdata/PM01_TIAM_B_LC_REFL00005.asd",package="FieldSpec")
spec <- read.asd(file.dir,out.dir='~',start.wave=350,end.wave=2500,step.size=1)

# Get info from file header
spec <- read.asd(file.dir,out.dir='~')

# Plot output
plot(spec$Wavelength,spec$Spectra,type="l",lwd=2,xlab="Wavelength (nm)", ylab="Reflectance (\%)")
}
\author{
  Shawn P. Serbin
}

