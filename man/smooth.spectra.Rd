\name{smooth.spectra}
\alias{smooth.spectra}
\title{Smooth spectra file with Savitsky-Golay smoothing filter.  Works with either a
single spectrum or array of spectral observations in row-major format.}
\usage{
  smooth.spectra(file.dir = NULL, input.file = NULL,
    out.dir = NULL, out.filename = NULL, header = TRUE,
    p = NULL, n = NULL, length = "full", file.ext = ".csv")
}
\arguments{
  \item{file.dir}{Directory of spectra files to apply
  sgolay smoothing filter.}

  \item{input.file}{Spectra file to apply sgolay smoothing
  filter. If not set then all files in file.dir are
  processed.}

  \item{out.dir}{Output directory for smoothed spectra
  files. If not set then processed spectra will be output
  to file.dir}

  \item{out.filename}{Output filename for processed spectra
  file.  If not set then the original filename(s) will be
  modified with the .sg suffix.}

  \item{header}{logical. Does the spectra file(s) have a
  header line? Default = TRUE}

  \item{p}{SG filter order.  Default 1.}

  \item{n}{SG filter length. Needs to be an odd value.
  Default 21}

  \item{length}{Apply sgolay smoothing filter to the entire
  (default=full) or subset of the spectrum.  Define subset
  limits as (wavelength1,wavelength2), e.g. (2000,2500) in
  nanometers [NOT YET IMPLEMENTED]}

  \item{file.ext}{File extension for spectra files.
  Default '.csv'}
}
\description{
  Smooth spectra file with Savitsky-Golay smoothing filter.
  Works with either a single spectrum or array of spectral
  observations in row-major format.
}
\examples{
\dontrun{
smooth.spectra()
smooth.spectra(file.dir=file.dir,input.file=input.file,out.dir=out.dir,out.filename=out.filename,
                header=TRUE,p=1,n=21,length=full)
}
}
\author{
  Shawn P. Serbin
}

