# Code for the CRE Transmission Network
library(deSolve)
library(tidyverse)
library(stats)
library(plyr)
library(labelled)

## Setting the Working Directory
setwd("C:/Users/DC49P19/Dropbox/01 PhD Epidemiology/Dissertation/R CODE/Aim 3 Code")
folder <- "C:/Users/DC49P19/Dropbox/01 PhD Epidemiology/Dissertation/R CODE/Aim 3 Code/"
aim2fol <- "C:/Users/DC49P19/Dropbox/01 PhD Epidemiology/Dissertation/DATASETS/AIM 2 NETWORK DATA/"
options(stringsAsFactors = FALSE)

## Sourcing the SIS Code
source("SIS_functions.R")
################################################################################
####################  LOADING HRR DATA ###################################
################################################################################

hrr_dem <- read.delim("HRR_TA1_demographics.txt", skip = 2)
hrr_zip <- read.delim("ZipHsaHrr14.txt")

################################################################################
################  MERGING HOSPITAL DATA TO CROSSWALK HRR #######################
################################################################################
#Make sure that I have have this loaded when JVPN is already connected
load(paste0(aim2fol, "aim2_sel.Rdata"))
id_xwalk <- aim2_sel %>%
  dplyr::rename(
    id = hospital_id_2019,
    zipcode14 = zipcode
  ) %>%
  mutate(zipcode14 = ifelse(zipcode14 == 37583, 38583, zipcode14))

hrr_id_xwalk <- right_join(hrr_zip, id_xwalk, by = "zipcode14") %>%
  mutate(provider = id) # changing the variable name ID into provider


xwalkid= dplyr::select(hrr_id_xwalk, id, hrrnum)
################################################################################
####################  LOADING STAYS DATA ###################################
################################################################################

# Loading Stays Data for HDDS hospitals.
load(paste0(aim2fol, "aim2_sel.Rdata"))
stays <- aim2_sel %>%
  mutate(
    set = "hdds",
    freq=as.numeric(n_hosp_2018),
    IPdays = as.numeric(patient_days_2018),
    avg_los=as.numeric(avg_los_2018),
    id=as.integer(hospital_id_2019)
  ) %>% 
  dplyr::select(
    id, freq,IPdays, avg_los, id, facility_type
  ) 
#Joining stays with HRR Crosswalks
stays = left_join(stays, xwalkid, by="id")

#analysis.type <- c("TN toy", "TN")[2] # change to 1 if your want TN Toy

## TN Toy : Testing out a 4-facility model that follows the TN distribution.
# No need, stick to TN ANALYSIS
# Just to seeif the model is working
################################################################################
####################  LOADING transfers DATA ###################################
################################################################################

#load("transfers_cms.Rdata") #loading CMS transfers data - NO NEED FOR NOW
#load("transfers_hdds_2018.Rdata") # Loading a file called transfers

#THE NEXT STEPS WERE COMMENTED TO CREATE A  A FINAL DATASET THAT WE LOADED. 
#load("transfers_surrogates_2018.Rdata") #Loading a file called transfers2
# surfol= 'H:/NHSN/CRE and CRA/RANY/Network Analysis/Datasets/Surrogate Edgelist/'
# transfers_num= read.csv(paste0(surfol,"SURROGATE_EDGELIST_V3_C_2018_4_dir.csv"))
# transfers_30d= read.csv(paste0(surfol,"SURROGATE_EDGELIST_V3_C_2018_4_30d.csv"))
# transfers_56d= read.csv(paste0(surfol,"SURROGATE_EDGELIST_V3_C_2018_4_56d.csv"))
# transfers_365d= read.csv(paste0(surfol,"SURROGATE_EDGELIST_V3_C_2018_4_365d.csv"))
# 
# transdata = list(transfers_num, transfers_30d, transfers_56d, transfers_365d)
# transfers_sur = join_all(transdata, by
#                          =c("HOSPITAL_ID1", "HOSPITAL_ID2"))
# colnames(transfers_sur) = tolower(colnames(transfers_sur))
# save(transfers_sur, file="transfers_surrogates_2018.Rdata")
# write.csv (transfers_sur, file = paste0(surfol, "transfers_surrogates_2018.csv"))
# write.csv (transfers_sur, file = paste0(folder, "transfers_surrogates_2018.csv"))
#load("transfers_surrogates_2018.Rdata")

# transfers2 =transfers1%>% 
#   dplyr::rename(
#     id_a = hospital_id1,
#     id_b = hospital_id2,
#     transfers_num = transfers_365d
#   ) %>% 
#   mutate( transfers_num = ifelse (transfers_num==1, 0, transfers_num)
#   )

transfers1 = read.csv ("H:/NHSN/CRE and CRA/RANY/Dissertation/DATASETS/HDDS EDGELIST/2018/HDDS_EDGELIST_2018_INP_DIRECT.csv")
colnames(transfers1) = c("id_a", "id_b","transfers_num", "conn") 
transfers2=    dplyr::select(transfers1,-conn) %>% 
  mutate(id_a=as.numeric(id_a), id_b=as.numeric(id_b), transfers_num=as.numeric(transfers_num))

# subsetting GEOID  if it is the same is the hrr num included in
# Crosswalk dataset for TB
hrr_dem <- subset(hrr_dem, GEOID %in% hrr_id_xwalk$hrrnum)[, 1:4] %>% 
  mutate(
    total= as.numeric(gsub(",", "", Total..Medicare.beneficiaries.age.65.99))) %>% 
  dplyr::rename(id=GEOID)


#CREATING AGGREGATE DATASET THAT WOULD AGGRGEATE THE NUMBER OF TRANSFERS
#TO ANY OTHER HOSPITAL WITHIN THE NETWORK TO #THE COMMUNITY

transfers_in_hrr = aggregate(x=transfers2$transfers_num,
                             by = list(transfers2$id_a), FUN=sum) 
colnames(transfers_in_hrr) = c("id", "transfers_in")

transfers_out_hrr = aggregate(x=transfers2$transfers_num,
                              by = list(transfers2$id_b), FUN=sum) 
colnames(transfers_out_hrr) = c("id", "transfers_out")

transfers_in_hrr1 = inner_join(transfers_in_hrr, stays, by="id") %>% 
  mutate(transfers_num = as.numeric(ifelse(freq-transfers_in>=0, 
                                           freq-transfers_in, 0))) %>% 
  dplyr::select(id,  hrrnum, transfers_num) %>% 
  dplyr::rename(id_a=id, id_b=hrrnum)  

transfers_out_hrr1 = inner_join(transfers_out_hrr, stays, by="id") %>% 
  mutate(transfers_num = as.numeric(ifelse(freq-transfers_out>=0, 
                                           freq-transfers_out, 0))) %>% 
  dplyr::select(id,  hrrnum, transfers_num) %>% 
  dplyr::rename(id_a=hrrnum, id_b=id)  

transfers_inout_hrr = bind_rows(transfers_in_hrr1, 
                                transfers_out_hrr1)

#Accumulating the number of aggregated transfers from community to 
#Hospitals and HRS

hosps = unique(transfers_in_hrr1$id_a)
hrr = unique(transfers_in_hrr1$id_b)
#Appending the combo together
hosps_hrr = data.frame(expand.grid(hosps,hrr)) %>% 
  dplyr::rename(id_a=Var1, id_b=Var2)
hrr_hosps = data.frame(expand.grid(hrr, hosps)) %>% 
  dplyr::rename(id_a=Var1, id_b=Var2)
hosps_hrr1 = bind_rows(hosps_hrr, hrr_hosps)

#adding the pairs of IDs between hospitals and HRRs they are not located in 
transfers_hrr1 = full_join(transfers_inout_hrr, hosps_hrr1, 
                           by =c("id_a", "id_b")) %>% 
  mutate(transfers_num =ifelse(is.na(transfers_num),0, transfers_num))


#MEGING THIS WITH THE STAYS DATA TO TO GET THE NUMBER OF TOTAL HOSPITALIZATIONS
#PER HOSPITAL, AND SUBTRACTING THAT FROM THE TRANSFERS TO GET THE 
#AGGREGATE OF TRANSFERS TO THE COMMUNITY.
#create an empry edgelist wtth all the hospitals and its transfers 
#to all HRRs
#ADDING THE TRANSFERS TO HRR TO THE ORIGINAL HOSPITAL-TO-HOSPITAL TRANSFERS
transfers3 = bind_rows(transfers2, transfers_hrr1)

#TOTAL DAYS OF STAYS IN THE COMMUNITY IS ONE YEAR PER PATIENT. 
# hrrstays = aggregate(x=transfers_in_hrr1$transfers_num, 
#                      by=list(transfers_in_hrr$id_b),
#                      FUN=sum) %>% 
#          mutate(id=Group.1,
#          facility_type= "Other",
#          freq=x) %>% 
#   dplyr::select(id, freq, facility_type)


#Getting total IP days per hospitals 
hrrIPdays= aggregate(x=stays$IPdays, 
                     by=list(stays$hrrnum),
                     FUN=sum) %>% 
  mutate(id=Group.1,
         hospdays=x) %>% 
  dplyr::select(id, hospdays)

hrrdata = list(hrr_dem, hrrIPdays)
hrrstays1= join_all(hrrdata, "id" ) %>% 
  mutate(hrr_los = sum(stays$IPdays)/ total,
         avg_los = 365.25 - sum(stays$IPdays)/total,
         freq = total,
         facility_type="Other",
         IPdays = avg_los*total) %>% 
  dplyr::select(id, avg_los, freq, IPdays, facility_type)

stays1 = bind_rows(stays, hrrstays1) %>% 
  mutate(factype=factor(ifelse(facility_type=="LTAC",3,
                               ifelse(facility_type=="Other",2,1)),
                        levels = c(1, 2, 3),
                        labels = c("STH", "Other", "LTH")))

# For HDDS, we first need to merge the transfers data with the stays data
transfers_a <- inner_join(transfers3, stays1, by = c("id_a" = "id")) %>%
  dplyr::select(id_a, id_b, transfers_num, factype) %>% 
  dplyr::rename(fac_type_a= factype)


#THE FACILITY TYPE FOR HRR IS OTHER.
transfers <- inner_join(transfers_a, stays1, by = c("id_b" = "id")) %>%
  dplyr::rename(fac_type_b = factype) %>% 
  dplyr::select(id_a, id_b, transfers_num, fac_type_a, fac_type_b)

#GETTING JUST THE VECTORS OF FACILITY TYPES
fac_type <- stays1$factype
fac_type_a <- transfers$fac_type_a
fac_type_b <- transfers$fac_type_b

nat_stays <- xtabs(stays1$freq ~ fac_type)
nat_los1 <- xtabs(stays1$freq * stays1$avg_los ~ fac_type) / nat_stays
nat_stays <- data.frame(
  fac_type = names(nat_stays),
  freq = c(nat_stays),
  avg_los = c(nat_los1)
)
nat_transfers <- data.frame(xtabs(transfers$transfers_num ~
                                    fac_type_a + fac_type_b))

#getting the transfers from hospitals to HRR

#getting HRR of the facility in stays data

###############################################################################
###############################################################################
# For Tennessee Data, we don't need to subset the facilities into "TN"
# because the dataset is already using a TN data.

#Stays included only 
stays <- stays1 %>% 
  dplyr::select(id, freq, IPdays, avg_los ) %>% 
  mutate(id = as.character(id)) %>% #change id into char vars
  filter(complete.cases(.))   #Remove lines with NAs


#Creating a matrix based on the number of facility in stays dataset
trans.mat <- array(0, rep(nrow(stays), 2), 
                   dimnames = list(stays$id, stays$id))

#FILLING THE MATRIX WITH TRANSFERS
for (i in 1:nrow(transfers)) {
  trans.mat[as.character(transfers[i, "id_a"]), as.character(transfers[i, "id_b"])] <-
    trans.mat[as.character(transfers[i, "id_a"]), as.character(transfers[i, "id_b"])] +
    transfers[i, "transfers_num"]
}
in.fractions <- prop.table(trans.mat, 2)
in.fractions[is.nan(in.fractions)] <- 0
occupancy <- stays$IPdays / 365.25

names(occupancy) <- dimnames(in.fractions)[[1]]
#Beta for HRR (Community) is really low, 1/500, 
#and then we set certain betas based on length of Stay
beta <- ifelse(dimnames(in.fractions)[[1]] %in% hrr_dem$id, 
               1 / 500, # Arbitrary low beta in community
               ifelse(stays$avg_los >= 15, 0.04192535, 0.1044544) #using the value from prabasaj's 
)             
#ifelse(stays$avg_los >=15,   0.0646782 , 0.04851752 )  #Rany's values from regression
# NHSN estimates (2016-11-01)
#ifelse(stays$avg_los>10,0.076,0.058)) 
# Utah modeling paper; los>10 -> LTACH? - TN it is 9

########################################################
# HDDS Data Define parameters for DE solver
pars <- list(
  beta = beta,
  tau0 = stays$avg_los, # Length of stay at hospital/community
  gamma = 1 / 387, # Clearance rate (1/days): Utah modeling paper
  in.fractions = in.fractions
)
################################################################################
################################################################################
## NHSN TN CRE stuff copied from ../fac_fig.R
# CRE vs LOS: NHSN data, 2015
# May need to deduplicate
# this is a CRE 2015 within NHSN
wdbkup <- getwd()
options(stringsAsFactors = FALSE)


################################################################################
################################################################################
# Getting the CRE Case counts as LABID events for CRE in TN
# labidevents = subset(read.delim("labidevents_CRE_2015.txt"),outpatient=="N")
labidevents <- aim2_sel %>%
  dplyr::select(
    hospital_id_2019, count_cre_all,
    patient_days_2019, n_hosp_2019
  ) %>%
  dplyr::rename(
    numPatDays = patient_days_2019,
    numAdms = n_hosp_2019,
    cre_count = count_cre_all,
    id = hospital_id_2019
  ) %>% 
  #filter(cre_count>0) %>% 
  mutate(id=factor(id))


## One positive lab results per line, inpatient only. Subset for the CRE ne
# denom=read.delim("Mdro_denom_cre_valid_id.txt")
# We already have denom data within labidevents
# For denominator, we will use the patient-days or number of hospitalizations

## What should we put here? Need advice #count of
# patient days month by month for the lab id events
# all facilities, all stay, month by month

# facility = read.delim("facility.txt")[,c("id","county","zip","id")]

################################################################################
################################################################################
#### List of TN Facilities already on HRR ID XWALK dataset ####
facility = hrr_id_xwalk %>%
  dplyr::select(
    id, countyname, zipcode14,
    hsanum, hsacity, hsastate, hrrnum, hrrcity, hrrstate
  )  %>% 
  mutate(id=factor(id))

los_data = dplyr::select(stays,IPdays, freq, avg_los, id ) %>% 
  mutate(id=factor(id))

# used to calculated the average LOS, and
# LOS calculation (avg). by facilities, 
# and aggregating over all months of the year.

################################################################################
################################################################################

cre_count <- labidevents %>%
  dplyr::select(cre_count, id) %>% 
  left_join(los_data, by = "id")

cre_count_hrr <- left_join(cre_count, facility, by="id") %>% 
  dplyr::select(cre_count, id, hrrnum, avg_los, IPdays, freq ) %>% 
  mutate(rate= ifelse(cre_count>0, cre_count/freq, NA),
         rateday= ifelse(cre_count >0, cre_count/IPdays, NA)) 

sth = dplyr::filter(cre_count_hrr, avg_los<15) 
###############################################################################
###### MODEL FOR SHORT TERM HOSPITAL #########################################
mylnlmhrr <- lm(I(log(rate)) ~
                  avg_los + as.factor(hrrnum),
                data= subset(cre_count_hrr, avg_los<15))
#this regression automatically removed the counts=0.
summary(mylnlmhrr)

# cutoff of length of stay, 15 is eyeballing from the gap
# of LOS between STH and LTH


tau_hat <- mean(subset(cre_count_hrr, avg_los<15)$avg_los)
tau_hat  #The mean average Length of Stay is what we are going to use to 
#the transmissibility 5.12

#Estimated beta (Transmissibility)
# hrr_beta_est <- coefficients(mylnlmhrr)[2] /
#    (1 + coefficients(mylnlmhrr)[2] * tau_hat) + 1 / 387 # 0.04851752
#hrr_beta_est
#If using the data from Prabasaj's

hrr_beta_est =0.1044544 #Uses Prabasaj's Paper


#Using the coefficient of average LOS dividing by 
#1 + coefficient for avg_los  + clearance rate 1/387

#Confidence interval for the Trasmissibility 
# CI_hrr_beta_est <- confint(mylnlmhrr)[2, ] /
#    (1 + confint(mylnlmhrr)[2, ] * tau_hat) + 1 / 387 # (0.07138965, 0.12461344)
# CI_hrr_beta_est
CI_hrr_beta_est = c(0.07138965, 0.12461344) #Prabasaj's Paper
#
# mylm <- lm(I(cre_count/ freq) ~ avg_los, cre_count)
# mylnlm1 <- lm(I(log(cre_count/ freq)) ~ avg_los, subset(cre_count, avg_los < 15))
# # But log~log better fit than log~lin
# mylnlm2 <- lm(I(log(cre_count / freq)) ~ avg_los, subset(cre_count, avg_los >= 15))

#LInear model without the transformation. and predictor is 1/avg_los
# ltlm <- lm(I(cre_count/freq) ~ I(1 / avg_los), 
#            subset(cre_count_hrr, avg_los >= 15))
# 
# summary(ltlm)# v_inf = 1-gamma/beta-1/(beta*los)
# coefficients(ltlm)

# LTACH beta estimate:
 # ltbeta <- coefficients(ltlm)[1] / 
 #  (-coefficients(ltlm)[2]) + pars$gamma # 0.04192535 = 1/23.85192  !
 # ltbeta 
ltbeta =0.04192535

#temp <- c(1 / coefficients(ltlm)[2], -coefficients(ltlm)[1] / coefficients(ltlm)[2]^2)
#temp

######################################################################################
#FRom Prabasaj's Paper
CI_ltbeta = c(.036, .048)
#Approximate SD from SE-ltbeta )
sd_ltbeta = ((ltbeta - CI_ltbeta[1])/1.96)^2
sd_ltbeta
n=8 #N Long Term Hospitals in Prabasaj's Study
sd_ltbeta= sd_ltbeta * sqrt(n)

#sd_ltbeta <- c(sqrt(temp %*% vcov(ltlm) %*% temp))
#Fraction deteted is the intercept of the model (baseline rate)/
#1-(recovery rate/transmission rate in LTACH)
#frac_detect <- coefficients(ltlm)[1] / (1 - pars$gamma / ltbeta) 
 #Fraction detected 0.1371169 (1 in 7) in Prabasaj's Paper
#frac_detect # 0.02496826 1 in 40 (much lower)
frac_detect = 0.1371169  
#######################################################################################
### NOTE OUR FRACTRION OF DETECTED CASES WERE TOO LOW, SO 
## LET'S JUST USE THE FRACTION FROM PRABASAJ'S PAPER
## SINCE THEY COME FROM MORE FACILITIES

### SHEA 2017 abstract stuff: use  parametrization
# but with almost zero community transmission:
ncezid.colors <- rgb(
  red = c(217, 139, 141, 0, 120),
  green = c(83, 49, 139, 106, 29),
  blue = c(30, 2, 0, 113, 126),
  alpha = rep(180, 5),
  names = c("zorange", "zbrown", "zolive", "zteal", "zpurple"),
  maxColorValue = 255
)
pars.shea <- pars  #Parameters for SHEA is the same with the parameter we set

#Looking at the transmissibility in the community during an endemic state,
#We use the results from the regression above to set the beta 
#for each short term and long-term hospital.

pars.shea$beta = ifelse(dimnames(pars.shea$in.fractions)[[1]] %in% hrr_dem$id,0,
                        # Low beta in community
                        ifelse(pars.shea$tau0>=15,ltbeta,hrr_beta_est))
pars.shea$beta
# NHSN estimates (symbolic values 2017-08-15)
# ifelse(pars.shea$tau0>=15,0.076,0.058)) # Utah values
#pars.shea$beta

# NHSN estimates (symbolic values 2017-08-15)
# 	ifelse(pars.shea$tau0>=15,0.076,0.058)) # Utah values
pars.shea$v0 <- pars.shea$beta * 0 + 0.0001 # Create zero vector and add small prevalence
pars.shea$v0

prev.endemic <- SISmultifacSteadyState(pars.shea)
###COmmenting this because not used in our setting
#Getting the prevalence of CRE in endemic by running the SIS model for multifac using
#the Parameters we set up there

adm.prev <- c(prev.endemic %*% pars.shea$in.fractions)
adm.prev
prev.cases <- prev.endemic * occupancy
prev.cases
num <- xtabs(prev.endemic * occupancy ~ cut(pars.shea$tau0, c(-Inf, 15, 180, Inf)))
num
den <- xtabs(occupancy ~ cut(pars.shea$tau0, c(-Inf, 15, 180, Inf)))
den
# num[2]/(num[1]+num[2]) # disease burden fraction
den[2]/(den[1]+den[2]) # occupancy fraction

# Updated so that 0 degree nodes do not give Ein or Eout of 1
tm <- trans.mat
tm  #this is the matrix of transfers
diag(tm) <- 0 # eliminate self-transfers
outdegree <- rowSums(tm > 0) #Calculate Out-Degree
Woutdegree <- rowSums(tm) #Weighted Out-Degree
Eoutdegree <- exp(-rowSums(tm / rowSums(tm) * log(tm / rowSums(tm)), na.rm = TRUE)) -
  (rowSums(tm > 0) == 0) #Eigen?
tm <- t(tm)
tm
indegree <- rowSums(tm > 0) #In-degree
Windegree <- rowSums(tm) #Weighted in-degree
Eindegree <- exp(-rowSums(tm / rowSums(tm) * log(tm / rowSums(tm)),
                          na.rm = TRUE)) - (rowSums(tm > 0) == 0)

p <- occupancy * prev.endemic
p <- p / sum(p)
p
Eendemic <- sum(-p * log(p), na.rm = TRUE)
Eendemic
## Set up graphics stuff:
fac.los.type <- cut(pars.shea$tau0, c(-Inf, 15, 180, Inf), labels = FALSE)
adjmat <- (trans.mat >= 10)
if (!exists("setupGraphics")) setupGraphics <- TRUE # Default for backward compatibility
if (setupGraphics) {
  try(library(statnet))
  myg <- network(adjmat, matrix.type = "adjacency")
  set.seed(0.42)
  mylayout <- network.layout.fruchtermanreingold(myg, layout.par = NULL) # Get coordinates for plot
}
# End graphics set-up

# Interventions incorporated as beta-multipliers; intervention remains in place until time.stop
#Setting the SIS Multifacility Model 
SISmultifac.int <- function(Time, State, Pars) {
  with(as.list(Pars), {
    beta.mult <- ifelse(seq(beta) %in% subset(
      interventions,
      time <= Time & time.stop > Time
    )$fac, int_mult, 1)
    dState <- beta.mult * beta * State * (1 - State) - State * (gamma + 1 / tau0) + c(State %*% in.fractions) / tau0
    return(list(dState))
  })
}

# Maximum eigenvalue with intervention
maxEV.int <- function(Time, int.fac.time, int_mult) {
  pars.int <- pars.shea
  pars.int$beta <- ifelse(seq(beta) %in% subset(int.fac.time, time <= Time & time.stop > Time)$fac, int_mult, 1) * pars.int$beta
  maxEV(pars.int)
}

