\name{average.spec.se}
\alias{average.spec.se}
\title{average replicate spectra within a directory of spectra files}
\usage{
average.spec.se(file.dir = NULL, out.dir = NULL, spec.type = NULL,
  spec.file.ext = NULL, start.wave = NULL, end.wave = NULL,
  step.size = NULL, bias.threshold = NULL, suffix.length = NULL,
  output.file.ext = NULL, settings.file = NULL)
}
\arguments{
  \item{file.dir}{directory of spectra files to process}

  \item{out.dir}{output directory for processed spectra
  files}

  \item{spec.type}{Option to set what type of spectra to
  average. Options: Reflectance, Transmittance.  Can be set
  with abbreviations: e.g. "Refl" or "Tran" Default is
  "Reflectance"}

  \item{spec.file.ext}{Optional to set the input file
  extension.  Default is ".sed"}

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

  \item{settings.file}{settings file used for spectral
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
\author{
Shawn P. Serbin
}

