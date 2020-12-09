####################################################################################################
#
#  	--- Last updated:  02.18.2014 BY Shawn Serbin <serbin@wisc.edu>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load required library
library(FieldSpectra)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load XML settings file (if using). Need to put in full path to xml file.
settings <- settings('/Users/serbin/Data/Dropbox/SoyFACE/R_Spectra_Training/SoyFACE.settings.xml')
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# Run each function separately

### Get metadata
metadata <- extract.metadata(settings.file=settings)

### Import ASD binary files to ASCII format
read.asd(settings.file=settings)   # Using settings file
spectra <- read.asd(settings.file=settings) # output a dataframe with the spectra

# Look at spectra
plot(seq(350,2500,1),spectra[2,2:dim(spectra)[2]],type="l")

### Apply jump (i.e. splice) correction
jump.correction(settings.file=settings)

### Average spectra
average.spec(settings.file=settings, spec.type="Reflectance")
spectra <- average.spec(settings.file=settings) # output a dataframe with the spectra

# Look at spectra
plot(seq(350,2500,1),spectra[2,2:dim(spectra)[2]],type="l")
#--------------------------------------------------------------------------------------------------#
### EOF