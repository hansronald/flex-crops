library(tidyverse)
library(janitor)
library(here)

theme_set(theme_classic())

#base_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/"
#image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

# Functions
scaleFUN <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )
  paste0(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
         c("","K","M","B","T")[div] )}