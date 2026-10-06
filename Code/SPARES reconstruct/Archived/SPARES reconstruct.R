library(readxl)
BMR_Covid_2022_admin <- read_excel("Datasets/SPARES/BMR_Covid_2022_admin.xlsx")


BMR_Covid_souches_2022 <- read_delim("Datasets/SPARES/BMR_Covid_souches_2022.csv", 
                                        delim = "|", escape_double = FALSE, trim_ws = TRUE)



ATB2022 <- read_excel("Datasets/SPARES/ATB2022.xlsx", sheet = "ATB2022")
ATB_participants2022 <- read_excel("Datasets/SPARES/ATB2022.xlsx",  sheet = "Participants ES_2022")
ATB_admindata2022 <- read_excel("Datasets/SPARES/ATB2022.xlsx", 
                                 sheet = "admin_2022")
head(ATB_admindata2022)
head(ATB_participants2022)
head(ATB2022)                                                                                                                                     
head(BMR_Covid_souches_2022)
head(BMR_Covid_2022_admin)

str(ATB2022)                                                                                                                                     
str(BMR_Covid_souches_2022)
str(BMR_Covid_2022_admin)
str(ATB_admindata2022)
str(ATB_participants2022)