\name{jump.correction}
\alias{jump.correction}
\title{apply a jump (splice) correction to imported ASD spectra files. This splice or jump
occurs at the boundaries between detectors}
\usage{
  jump.correction(file.dir = NULL, out.dir = NULL,
    start.wave = NULL, end.wave = NULL, step.size = NULL,
    jumploc1 = NULL, jumploc2 = NULL, firstJumpMax = NULL,
    secondJumpMax = NULL, output.file.ext = NULL,
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
  1nm, 5 for 5nm. Not needed if specified in settings
  file.}

  \item{jumploc1}{location of the first jump in the spectra
  to correct. Not needed if jump.correction=FALSE or if
  specified in XML settings file.}

  \item{jumploc2}{location of the second jump in the
  spectra to correct. Not needed if jump.correction=FALSE
  or if specified in XML settings file.}

  \item{firstJumpMax}{maximum jump threshold for the first
  jump location. Determines whether spectra will be
  corrected or flaged as bad. (Optional.  Default is 0.02)}

  \item{secondJumpMax}{maximum jump threshold for the
  second jump location. Determines whether spectra will be
  corrected or flaged as bad. (Optional.  Default is 0.02)}

  \item{output.file.ext}{option to set file extension of
  the output files. Defaults to .csv}

  \item{settings.file}{settings file used for spectral
  processing options (OPTIONAL). Contains information
  related to the spectra collection instrument, output
  directories, and processing options such as applying a
  jump correction to the spectra files.}
}
\value{
  output list containing processed spectra and associated
  diagnostic information
}
\description{
  A function to apply a jump (splice) correction to
  imported spectra files
}
\examples{
\dontrun{
jump.correction()
jump.correction(file.dir,out.dir, start.wave=350,end.wave=2500,step.size=1,jumploc1=651,jumploc2=1451,
output.file.ext=".csv",settings.file=NULL)
}
}
\author{
  Shawn P. Serbin
}

