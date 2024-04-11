# libraries needed to run FBIT output

# ICES libraries
  #remotes::install_github("ices-tools-prod/icesSharePoint")
  #library(icesSharePoint)
  
  # options(repos = c(
  # icestoolsprod = 'https://ices-tools-prod.r-universe.dev',
  # CRAN = 'https://cloud.r-project.org'))
  # install.packages('icesVMS'), only needed to download VMS data from ICES data centre (data product will be on sharepoint)

# R libraries
  #library(rgdal) ## not available for R version 4.3.2 (and newer)
  library(dplyr)
  library(rje)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  library(gridExtra)