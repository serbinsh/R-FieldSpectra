\name{concat.spectra}
\alias{concat.spectra}
\title{Concatenate a directory of spectra files into a single .csv file. Works on a single
directory or a series of directories}
\usage{
concat.spectra(file.dir = NULL, out.dir = NULL, out.filename = NULL,
  in.file.ext = ".csv", out.file.ext = ".csv", transpose = FALSE,
  spec.dataframe = FALSE)
}
\arguments{
  \item{file.dir}{directory of spectra files to process.
  Currently works on spectra files formatted where each row
  is a wavelength and associated spectral observation.}

  \item{out.dir}{output directory for concatenated spectra
  files. If not set then the output directory defaults to
  file.dir (input directory)}

  \item{out.filename}{filename for concatenated spectra
  files}

  \item{in.file.ext}{file extension for individual spectra
  files. Defaults to ".csv"}

  \item{out.file.ext}{option to set the output extension.
  Defaults to ".csv"}

  \item{transpose}{option to transpose the spectra to/from
  row/column major output. This will be applied to both the
  returned dataframe (optional) and output file.
  TRUE/FALSE. Default FALSE}

  \item{spec.dataframe}{option toreturn a R dataframe
  containing the concatenated spectral data.  TRUE/FALSE}
}
\description{

}
\examples{
\dontrun{
concat.spectra()
spectra <- concat.spectra(file.dir=file.dir,out.dir=out.dir,out.filename="concat.spectra",
out.file.ext=".csv",transpose=FALSE,spec.dataframe=FALSE)
}
}
\author{
Shawn P. Serbin
}

