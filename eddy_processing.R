#Process eddy covariance data with REddyProc.
#Info: https://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWebRPackage
#If you don't already have the REddyProc package:
#install.packages("REddyProc", repos=c("http://R-Forge.R-project.org","@CRAN@"))

library(REddyProc)
help('REddyProc-package')

sEddyProc.example()
