#################################### Initialization ###############################################
remove(list = ls())

##### Data path
Datapath = "RawData/"
Output_path = "Output/"

WithAnonyme = TRUE
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
library(ggpubr)

## Stat and ML
library(nlme)
library(lmerTest)
library(BayesFactor)
library(stats)
library(FactoMineR)
library(factoextra)
library(interactions)
library(rstatix)

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
                     "oumayma.achahboune@ulb.be" = "oumayma.acha@outlook.com",
                     "Ludovic.chardin@cegetel.net" = "ludovic.chardin@cegetel.net",
                     "Damien_lecat@hotmail.com" = "damien_lecat@hotmail.com",
                     "norakra123@gmail.com" = "norakrami123@gmail.com",
                     "norakrami123" = "norakrami123@gmail.com",
                     "julievetesse@icloud.com" = "julievetesse@icolud.com",
                     "azizniang@icloud.com" = "Azizniang@icloud.com",
                     "teamkebabs@icloud.com" = "teamkebabs@outlook.com",
                     "litalynda@yahoo.com" = "litalynda@yahoo.com ")


##### List of participant who gave two different ages for LS1 and LS2
FU = c("pauline.annaert@hotmail.com", "r.1991@hotmail.fr")
