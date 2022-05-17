# Hyperscript df creation
source("DataPreparation/1.1.ImportLS1.R")
source("DataPreparation/1.2.ImportLS2.R")
source("DataPreparation/1.3.ImportLS3.R")
source("DataPreparation/1.4.MergeLSdf.R")

source("DataPreparation/2.1.DataD1.R")
source("DataPreparation/2.2.DataD2.R")

source("DataPreparation/3.MergeAll.R")

source("AdditionalInfo/MailList/MailCheck.R")

source("DataPreparation/4.Anonymization.R")

# Launch from here if you do not have access to raw data and want to do the computations by yourselves
source("DataPreparation/5.Computations.R")

# Launch from here if you do not want to redo the computations and do not have all the raw data
source("DataPreparation/6.AdditComp.R")

source("DataPreparation/7.PrepForRegLog.R")
source("DataPreparation/8.DataPrep.R")
