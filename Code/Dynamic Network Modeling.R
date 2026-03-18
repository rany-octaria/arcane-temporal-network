# Modeling Code
# Tinkering with the Modeling of the network over many days
# Rany Octaria
# Le CNAM

#The facility is grouped by finessGeo

# FIRST: MAKE SURE YOU OPEN THIS CODE FROM THE ARCANE-TEMPORAL-NETWORK PROJECT
# SECOND: make sure you download  the transfer datasets here #https://ehespfr-my.sharepoint.com/personal/shrichand_bhuria_ehesp_fr/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fshrichand%5Fbhuria%5Fehesp%5Ffr%2FDocuments%2FHBN%5F2024%5Fdata%5Fsharing&ga=1 

# Download it into the Datasets folder. I can't sync it into GitHub because the files were too large

library(here)
library(purrr)     # map_dfr
library(igraph)
library(patchwork)   # side-by-side layout
library(dplyr)
library(igraph)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(visNetwork)
library(ggiraph)
library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(tibble)
library(scales)
library(purrr)
library(readr)
# install.packages(c("rnaturalearth", "rnaturalearthdata", "remotes"))
# remotes::install_github("ropensci/rnaturalearthhires")

options(scipen = 999)
# Import the CSV datasets
# Importing from WINDOWS
monthly = read_csv(here("Datasets", "hbn_direct_transfer_2024","HBN_monthly_sliding_edgelist_2024.csv" ))
weekly = read_csv(here("Datasets", "hbn_direct_transfer_2024","HBN_weekly_sliding_edgelist_2024.csv" ))

#Importing facility databaseIUM_hospital_info_2024 <- read_delim("Datasets/IUM_hospital_info_2024.csv", 
IUM_hospital_info_2024 <- read_delim(here("Datasets","IUM_hospital_info_2024.csv"),
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
head(IUM_hospital_info_2024)

#Importing admission data

daily_admission<- read_delim(here("Datasets" , "hbn_direct_transfer_2024", "NO_ADMISSION_DAILY_2024.csv"), 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
head(daily_admission)

#Importing Data No beds
beds_data = read.csv(here("Datasets", "hbn_direct_transfer_2024",
                          "Data_No_Beds_2024_FinessGeo.csv"))
head(beds_data)

#geographical coord
coords = read.csv(here("Datasets", "finessGeo_data.csv"))
head(coords)


#Many of the finessGeo characteristics data is still multiple lines. Lets make sure they are aggregated
