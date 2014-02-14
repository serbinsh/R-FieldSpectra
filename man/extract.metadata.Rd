\name{extract.metadata}
\alias{extract.metadata}
\title{Extract metadata for each spectra sample.  Works on a single spectra or a directory.}
\usage{
extract.metadata(file.dir = NULL, out.dir = NULL, instrument = NULL,
  spec.file.ext = NULL, output.file.ext = ".csv", tz = NULL,
  settings.file = NULL)
}
\arguments{
  \item{file.dir}{File directory or filename of single
  spectra for processing}

  \item{out.dir}{Output directory for metadata information
  file}

  \item{instrument}{What instrument was used to collect
  spectra.  Current options: ASD, SE}

  \item{spec.file.ext}{[Optional] Input spectra file
  extension. E.g. .asd (ASD) or .sed (Spectral Evolution).
  Default for ASD instruments is .asd.  Default for
  Spectral Evolution instruments is .sed}

  \item{output.file.ext}{[Optional] Output file extension
  of metadata information file. Default .csv}

  \item{tz}{[Optional] Set the timezone of the spectra file
  collection.  Used to covert spectra collection time to
  UTC.  If unused it is assumed that the correct timezone
  is the current system timezone.}

  \item{settings.file}{[Optional] Spectral settings file}
}
\value{
output Returns a dataframe of spectral metadata information
}
\description{

}
\examples{
# ASD
file <- system.file("extdata/PM01_TIAM_B_LC_REFL00005.asd",package="FieldSpectra")
output <- extract.metadata(file,instrument="ASD")

# Spectral Evolution
file <- system.file("extdata/cvars_grape_leaf1_lc_rg_01236.sed",package="FieldSpectra")
output <- extract.metadata(file,instrument="SE")
}
\author{
Shawn P. Serbin
}

