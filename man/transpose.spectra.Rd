\name{transpose.spectra}
\alias{transpose.spectra}
\title{Transpose spectra file(s) from/to row or column major format.  Works with either a
single spectrum or directory of spectral observations.}
\usage{
transpose.spectra(file.dir = NULL, input.file = NULL, out.dir = NULL,
  out.filename = NULL, in.file.ext = ".csv", out.file.ext = ".csv",
  header = FALSE)
}
\arguments{
  \item{file.dir}{directory of spectra files to process}

  \item{input.file}{name of input spectra file. If ommitted
  then the function transposes all files in the input
  file.dir}

  \item{out.dir}{output directory for transposed spectra
  file(s)}

  \item{out.filename}{output filename of transposed spectra
  file(s). If using a directory then the original filenames
  are appended with the .t suffix}

  \item{in.file.ext}{file extension for individual spectra
  files. Defaults to ".csv"}

  \item{out.file.ext}{option to set the output extension.
  Defaults to ".csv"}

  \item{header}{logical. Do/does the file(s) have a header
  line?}
}
\description{

}
\author{
Shawn P. Serbin
}

