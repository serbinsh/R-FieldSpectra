\name{settings}
\alias{settings}
\title{parse settings file used for spectra file import and processing}
\usage{
  settings(input.file = NULL)
}
\arguments{
  \item{input.file}{settings file containing information
  needed for spectra processing}
}
\description{
  Read settings file for spectra import and processing
}
\examples{
\dontrun{
settings <- settings()
settings <- settings('/home/$USER/settings.xml')
}
}
\author{
  Shawn P. Serbin
}

