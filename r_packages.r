#Zibusiso Masuku zibusiso.masuku@outlook.com
#List of R Packages that I use for my day to day.

#Clear the workspace
rm(list = ls())

#Install the Pacman Package
install.packages("pacman")

#List of the packages 
packages_soox = c("readxl","actuar","xlsx","reshape","lubridate","ChainLadder","plyr","stringr","tidyverse","data.table","pbapply","openxlsx","janitor","ggplot2","dplyr","tidyverse", "broom", "coefplot", "cowplot","gapminder", "GGally", "ggrepel", "ggridges", "gridExtra","here", "interplot", "margins", "maps", "mapproj","mapdata", "MASS", "quantreg", "rlang", "scales","survey", "srvyr", "viridis", "viridisLite", "devtools","readxl","readr","dplyr","plyr","tictoc")

#Load all other packages
library(pacman)
lapply(packages_soox, p_load, character.only = TRUE)

#Clear Workspace
rm(list = ls())

#End of Code