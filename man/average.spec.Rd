\name{average.spec}
\alias{average.spec}
\title{average replicate spectra within a directory of spectra files}
\usage{
  average.spec(file.dir = NULL, out.dir = NULL,
    start.wave = NULL, end.wave = NULL, step.size = NULL,
    bias.threshold = NULL, suffix.length = NULL,
    output.file.ext = NULL, spec.dataframe = FALSE,
    settings.file = NULL)
}
\arguments{
  \item{file.dir}{directory of spectra files to process}

  \item{out.dir}{output directory for processed spectra
  files}

  \item{start.wave}{starting wavelength of spectra files.
  Not needed if specified in XML settings file.}

  \item{end.wave}{ending wavelength of spectra files. Not
  needed if specified in XML settings file.}

  \item{step.size}{resolution of spectra files. E.g. 1 for
  1nm, 5 for 5nm. Not needed if specified in XML settings
  file.}

  \item{bias.threshold}{reflectance/transmittance cutoff to
  remove spectra with anartificial bias (shift) due to
  improper spectral collection}

  \item{suffix.length}{length of auto numbering attached to
  ASD file names.  This number of characters will be
  removed from the filename when averaged.}

  \item{output.file.ext}{optional setting to set file
  extension of output files. Defaults to .csv}

  \item{spec.dataframe}{option to return a data frame with
  the converted spectra files}

  \item{settings.file}{settings file used for spectral
  processing options (OPTIONAL). Contains information
  related to the spectra collection instrument, output
  directories, and processing options such as applying a
  jump correction to the spectra files.  Options in the
  settings file take precedent over options selected in the
  function call.}
}
\description{
  Function to average replicate spectra within a directory
  of spectra files
}
\examples{
\dontrun{
average.spec()
average.spec(file.dir,out.dir, start.wave=350,end.wave=2500,step.size=1,bias.threshold=0.06,
suffix.length=5,output.file.ext=".csv",spec.dataframe = TRUE,)
}
}
\author{
  Shawn P. Serbin
}

