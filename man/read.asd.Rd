\name{read.asd}
\alias{read.asd}
\title{A function to import raw ASD binary files}
\usage{
  read.asd(file.dir = NULL, out.dir = NULL,
    start.wave = NULL, end.wave = NULL, step.size = NULL,
    image = FALSE, asd.file.ext = ".asd",
    output.file.ext = ".csv", settings.file = NULL)
}
\arguments{
  \item{file.dir}{A single ASD binary file or directory of
  ASD files to import.  Currently only supports single
  directory processing (i.e. no nested dir structures)}

  \item{out.dir}{main output directory for processed
  spectra files.}

  \item{start.wave}{starting wavelength of ASD binary
  spectra files.  Depends on instrument.}

  \item{end.wave}{ending wavelength of ASD binary spectra
  files.  Depends on instrument.}

  \item{step.size}{wavelength step size for ASD files. E.g.
  1nm, 5nm, 10nm}

  \item{image}{logical. Whether to produce png images of
  each spectrum}

  \item{asd.file.ext}{file extension of ASD files.  Usually
  ".asd" (Default)}

  \item{output.file.ext}{optional setting to set file
  extension to output files. Defaults to .csv}

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
file.dir <- system.file("extdata/PM01_TIAM_B_LC_REFL00005.asd",package="FieldSpec")
spec <- read.asd(file.dir,out.dir='$HOME',start.wave=350,end.wave=2500,step.size=1)

# Plot output
plot(spec$Wavelength,spec$Spectra,type="l",lwd=2,xlab="Wavelength (nm)", ylab="Reflectance (\%)")
}
\author{
  Shawn P. Serbin
}

