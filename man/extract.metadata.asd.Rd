\name{extract.metadata.asd}
\alias{extract.metadata.asd}
\title{Extract metadata from raw binary ASD files}
\usage{
  extract.metadata.asd(file.dir, out.dir, spec.file.ext,
    output.file.ext, tz, intern)
}
\arguments{
  \item{file.dir}{File directory or filename of single
  spectra for processing}

  \item{out.dir}{Output directory for meta-data information
  file}

  \item{spec.file.ext}{[Optional] Input spectra file
  extension. E.g. .asd (ASD) or .sed (Spectral Evolution).
  Default for ASD instruments is .asd.  Default for
  Spectral Evolution instruments is .sed}

  \item{output.file.ext}{[Optional] Output file extension
  of meta-data information file. Default .csv}

  \item{tz}{[Optional] Set the timezone of the spectra file
  collection.  Used to covert spectra collection time to
  UTC.  If unused it is assumed that the correct timezone
  is the current system timezone.}

  \item{intern}{logical. [Optional] Keep meta-data output
  as an internal object (TRUE) or write to file (FALSE)}
}
\description{
  Extract metadata from raw binary ASD files
}
\author{
  Shawn P. Serbin
}

