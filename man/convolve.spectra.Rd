\name{convolve.spectra}
\alias{convolve.spectra}
\title{Convolve field spectral observations to airborne/satellite spectral bands/channels.}
\usage{
convolve.spectra(field.spectra = NULL, start.wave = NULL, end.wave = NULL,
  sensor = NULL, year = NULL, spc = NULL)
}
\arguments{
  \item{field.spectra}{Input field spectra. If a single
  spectra then the format can be either column or row major
  with the wavelengths either a column of data or the
  column headers.  If multiple (i.e. a matrix of data) the
  input should be in row major format with the column
  headers as wavelength numbers.  See examples below.}

  \item{start.wave}{Starting wavelength of field spectra}

  \item{end.wave}{ending wavelength of field spectra}

  \item{sensor}{The sensor (e.g. "AVIRIS", "Landsat 5",
  "Landsat 7", "MODIS").  Current options are: "AVIRIS"
  More to come in the future}

  \item{year}{[Optional] E.g. year=2011.  If the AVIRIS
  sensor is selected this allows the user to select a
  specific spectral calibration based on the year of data
  aquisition. Defaults to 2011.}

  \item{spc}{[Optional] Option to use custom spectral
  calibration information.  Format: band number, center
  wavelength, FWHM.  NOT YET IMPLEMENTED}

  \item{output.dir}{[Optional] Directory to output the
  convolved spectral data.  If not set the results are only
  pasesd back to the working environment}
}
\value{
output.spectra Returns a matrix of convolved spectra
}
\description{

}
\author{
Shawn P. Serbin
}

