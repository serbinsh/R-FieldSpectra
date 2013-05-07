####################################################################################################
#
#  	--- Last updated:  5.07.2013 BY Shawn Serbin <serbin@wisc.edu>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load required library
library(FieldSpec)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load XML settings file (if using). Need to put in full path to xml file.
settings <- settings('/Users/serbin/Data/Dropbox/SoyFACE/R_Spectra_Training/SoyFACE.settings.xml')
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# Run each function separately

### Import ASD binary files to ASCII format
read.asd(settings.file=settings,spec.dataframe = FALSE)   # Using settings file
spectra <- read.asd(settings.file=settings,spec.dataframe = TRUE) # output a dataframe with the spectra

# Look at spectra
plot(seq(350,2500,1),spectra[2,2:dim(spectra)[2]],type="l")

### Apply jump (i.e. splice) correction
jump.correction(settings.file=settings)

### Average spectra
average.spec(settings.file=settings)
spectra <- average.spec(settings.file=settings,spec.dataframe = TRUE) # output a dataframe with the spectra

# Look at spectra
plot(seq(350,2500,1),spectra[2,2:dim(spectra)[2]],type="l")
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Process a series of spectra directories

### Run all on a series of directories
import.asd(settings.file=settings)
#--------------------------------------------------------------------------------------------------#

