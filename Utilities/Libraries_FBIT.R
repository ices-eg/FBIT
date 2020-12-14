# libraries needed to run FBIT output

# ICES libraries
  #remotes::install_github("ices-tools-prod/icesSharePoint")
  #library(icesSharePoint)
  
  #devtools::install_github("ices-tools-prod/icesVMS")
  #library(icesVMS), only needed to download VMS data from ICES data centre (data product will be on sharepoint)

# R libraries
  library(rgdal)
  library(dplyr)
  library(rje)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  library(gridExtra)