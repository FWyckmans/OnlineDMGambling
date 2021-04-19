#################################### Initialization ###############################################
remove(list = ls())

##### Data path
Datapath = "RawData/"
Output_path = "Output/"

##### Cran packages
## Install
packages <- c("dplyr", "tidyr",
              "ggplot2", "gridExtra", "cowplot", "corrplot",
              "nlme", "lmerTest", "BayesFactor", "stats",
              "car", "readxl", "readr", "Hmisc", "rms", "ISLR", "e1071", "stringr", "writexl",
              "hBayesDM", "FactoMineR", "factoextra",
              "devtools")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Data
library(dplyr)
library(tidyr)

## Graphics
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)

## Stat and ML
library(nlme)
library(lmerTest)
library(BayesFactor)
library(stats)
library(FactoMineR)
library(factoextra)

## Misc (or don't remember and imported anyway)
library(car)
library(readxl)
library(readr)
library(Hmisc)
library(rms)
library(ISLR)
library(e1071)
library(stringr)
library(writexl)

## Specific
library(devtools)
# install_github("andrewheiss/limer")
# library(limer)

##### Functions
for (Fun in dir("Functions/")) {
  source(paste0("Functions/", Fun))
}

##### List of badly spelled email by participant
MailToChange <- list("marouan.lahlafi@ulb.ac.be" = "marouan.lahlafi@ulb.be",
                     "Imoan@live.be" = "imoan@live.be",
                     "robin.gautier13@gmail.com " = "robin.gautier13@gmail.com",
                     "Laetitia.Mbiada.Fapom@ulb.be" = "laetitiambiada@gmail.com",
                     "oumayma.achahboune@ulb.be" = "oumayma.acha@outlook.com")

##### List with column names by type of variable for further references
