\name{import.asd}
\alias{import.asd}
\title{Import raw binary ASD spectra files}
\usage{
import.asd(asd.dir = NULL, output.dir = NULL, jump.correction = FALSE,
  average = FALSE, start.wave = NULL, end.wave = NULL, step.size = NULL,
  jumploc1 = NULL, jumploc2 = NULL, asd.file.ext = NULL,
  output.file.ext = NULL, settings.file = NULL)
}
\arguments{
  \item{asd.dir}{directory containing raw ASD files or
  sub-directories to be processed. Not needed if specified
  in settings file.}

  \item{output.dir}{main ouput directory. Not needed if
  specified in settings file.}

  \item{jump.correction}{option to apply a jump correction
  to the imported spectra files. Not needed if specified in
  settings file.}

  \item{average}{option to average imported spectra files.
  Will use jump corrected files if exist. Not needed if
  specified in settings file.}

  \item{start.wave}{starting wavelength of spectra files.
  Not needed if specified in settings file.}

  \item{end.wave}{ending wavelength of spectra files. Not
  needed if specified in settings file.}

  \item{step.size}{resolution of spectra files. E.g. 1 for
  1nm, 5 for 5nm. Not needed if specified in settings
  file.}

  \item{jumploc1}{location of the first jump in the spectra
  to correct. Not needed if jump.correction=FALSE or if
  specified in settings file.}

  \item{jumploc2}{location of the second jump in the
  spectra to correct. Not needed if jump.correction=FALSE
  or if specified in settings file.}

  \item{asd.file.ext}{file extension for input binary asd
  spectra files}

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
\description{
A wrapper function to import raw binary ASD spectra files,
apply jump correction to the imported files and average
replicate files into a single spectrum.  Calls read.asd(),
jump.correction(), and average.spec() functions.
}
\author{
Shawn P. Serbin
}

