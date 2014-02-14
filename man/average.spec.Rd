\name{average.spec}
\alias{average.spec}
\title{average replicate spectra within a directory of spectra files}
\usage{
average.spec(file.dir = NULL, out.dir = NULL, start.wave = NULL,
  end.wave = NULL, step.size = NULL, bias.threshold = NULL,
  suffix.length = NULL, output.file.ext = NULL, metadata.file = NULL,
  image = FALSE, settings.file = NULL)
}
\arguments{
  \item{file.dir}{Directory of spectra files to process}

  \item{out.dir}{Output directory for processed spectra
  files}

  \item{start.wave}{Starting wavelength of spectra files.
  Not needed if specified in XML settings file.}

  \item{end.wave}{Ending wavelength of spectra files. Not
  needed if specified in XML settings file.}

  \item{step.size}{Resolution of spectra files. E.g. 1 for
  1nm, 5 for 5nm. Not needed if specified in XML settings
  file.}

  \item{bias.threshold}{Reflectance/transmittance cutoff to
  remove spectra with anartificial bias (shift) due to
  improper spectral collection}

  \item{suffix.length}{Length of auto numbering attached to
  ASD file names.  This number of characters will be
  removed from the filename when averaged.}

  \item{output.file.ext}{Optional setting to set file
  extension of output files. Defaults to .csv}

  \item{spec.dataframe}{Option to return a data frame with
  the converted spectra files}

  \item{metadata.file}{Option to select custom metadata
  file for use in processing. If not set then the
  information is either read from default metadata file,
  the settings file or at the function call. Need to set
  this as the full qualified path to the spectral metadata
  file is using a custom file/location}

  \item{image}{Logical. Whether to produce .png images of
  each spectrum (TRUE) or not (FALSE). Default is FALSE.
  Useful for diagnosing spectral observations during
  processing.}

  \item{settings.file}{Settings file used for spectral
  processing options (OPTIONAL). Contains information
  related to the spectra collection instrument, output
  directories, and processing options such as applying a
  jump correction to the spectra files.  Options in the
  settings file take precedent over options selected in the
  function call.}
}
\description{
Function to average replicate spectra within a directory of
spectra files
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

