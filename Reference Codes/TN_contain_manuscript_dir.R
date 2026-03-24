
setwd("C:/Users/DC49P19/Dropbox/01 PhD Epidemiology/Dissertation/R CODE/Aim 3 Code")
source("TN_contain_setup_hdds_dir.R")
#source("TN_contain_setup_hdds.R")
#source("TN_contain_setup_hdds_sur.R")
library(statnet)
library(deSolve)

options(scipen=999)
### SETTING THE FOLDER, FILE NAME AND FILES FOR THE SIMULATION RUNS #############
folder = "C:/Users/DC49P19/Dropbox/01 PhD Epidemiology/Dissertation/R CODE/Aim 3 Code/"
savedfold= paste0(folder, "savedruns_dir/")
savedruns1 = paste0(folder, "savedruns_dir/manuscript_results_uniform")
savedruns2 = paste0(folder, "savedruns_dir/manuscript_results")
multifile = paste0(savedfold, "Multiple Effectiveness/manuscript_results")

###################################################################################
#### SETTING THE FOLDER TO SAVE GRAPHS ############################################
graffold = "C:/Users/DC49P19/Dropbox/01 PhD Epidemiology/Dissertation/R CODE/Aim 3 Code/graphs_dir/"

# Outbreak tracking stuff
# Steps:
#  Pick index facility
#  Set trigger criterion
#  Locate trigger event (day and facility)
#  Determine upstream and downstream facilities of interest
#  Institute infection control measures at facilities of interest (multiplier for beta)
#  Run from trigger event
#  Repeat with next trigger event

#####################################################
# Following two paragraphs copied from TN_contain to ensure consistency in run parameters and settings
# Trigger and run parameters
inc.wt = 1; adm.wt = 1 # Weights to set detection threshold
int_delay = 30 # Days from trigger event to intervention
go_up_down = TRUE # TRUE if up and downtream facilities intervened at
up_by_case_prob = TRUE # TRUE if upstream by max prob of admitting case; FALSE if upstream by admission fraction
int_mult = 0.80 # Multiplier for beta at intervened in facilities
pps.time = 14 # Number of days between point prevalence surveys
pps.neg = 2 # Intervention stops after pps.neg consecutive surveys with no positives
pps.window = rep(1, pps.neg) # Convolution window

max_days = 1800 # Overall time frame for runs
min_days = 500 # Time out from each intervention for sub-runs
index_fac = which.max(Woutdegree[1:144]) # Happens to be the largest hospital, too
#INTRODUCE TO INDEX FACILITY VUMC
index_fac
#########################################################

summary(occupancy[145:152])  #Occupancy of HRR (8 in TN)
summary(occupancy[1:144]) #Occupancy of facilities)
summary(pars$tau0[1:144]) #LOS of facilities

summary(occupancy[1:152]) #All occupancy
sum(occupancy[145:152])

#TOTAL TRANSFERS BETWEEN HOSPITALS
sum(trans.mat[1:144,1:144])
#total transfers  from hospitals to HRR
sum(trans.mat[1:144,145:152])
#Total hospital occupancy
sum(occupancy[1:144])

dim(cre_count_hrr)
sum(cre_count_hrr$freq)

# Daily transmission estimate in TN
xtabs(cre_count_hrr$freq~substr(cre_count_hrr$id,1,2))/(frac_detect*365.25)

# Occupancy related items:
# occ_temp = merge(cost_report[,c("prvdr_num","Bedsize", "Tot_MDCR_IPDays",
#                                 "Tot_IPDays", "Tot_MDCR_Dschg", "TOT_Dschg")],
#   occupancy,by.x="prvdr_num",by.y=0)
# summary(lm(Tot_MDCR_IPDays~Tot_IPDays+0, occ_temp)) # 28.6% of occupancy MDCR
# summary(lm(y~I(Tot_MDCR_IPDays/365.25)+0, occ_temp)) # coeff = 1.143


# Set up transfer numbers, excluding community and self
temp.mat = in.fractions  # Setting up transfers fom the trans.mat (as fractions)
#temp.mat[fac.los.type==3,] = 0
#temp.mat[,l==3] = 0
diag(temp.mat) = 0


# BEGIN postprocessing function definition
day.summary = function(day) {
  #Cumulative cases by day
  cum.case.day = t(matrix(colSums(inc.cases[1:day,]),nrow=2))
  #Prevalent cases at that day
  prev.day = t(matrix(prev.cases[day,],nrow=2))
  #Cumulative Cases of that day
  cum.case.day = cbind(cum.case.day,
                       round(100*(cum.case.day[,1]-cum.case.day[,2])/cum.case.day[,1],1))
  
  prev.day = cbind(prev.day, 
                   round(100*(prev.day[,1]-prev.day[,2])/prev.day[,1],1))
  res = data.frame(cbind(res.pars, cum.case.day,prev.day))
  names(res) = c("run","index_fac","int_mult","pps_time","transmission",
                 "transmission_int","transmission_dec","prevalent","prevalent_int","prevalent_dec")
  res[,-(1:3)] = round(res[,-(1:3)])
  res
}
hosp_bed_counts = function(day, interventions) {
  temp = subset(interventions,time<=day) # Historical interventions
  temp$occ = occupancy[temp$fac]
  hosp.count = length(unique(temp$fac))
  hosp.day.count = sum(pmin(day,temp$time.stop)-temp$time)
  bed.day.count = sum(temp$occ * (pmin(day,temp$time.stop)-temp$time))
  c(hosp.count, hosp.day.count,bed.day.count)
}

outbreak.summary = function(ob.num, time_pts = 0:3*365) {
  ob.pars = res.pars[ob.num,]
  trig = data.frame(time = triggers[[3*ob.num-2]], 
                    fac = triggers[[3*ob.num-1]], 
                    type = triggers[[3*ob.num]])
  intv = data.frame(fac =  int.list[[3*ob.num-2]], 
                    time = int.list[[3*ob.num-1]], time.stop = int.list[[3*ob.num]])
  ob.inc = inc.cases[, (-1):0 + 2*ob.num]
  ob.prev = prev.cases[, (-1):0 + 2*ob.num]
  trig.one = min(trig$time)
  int_duration = ob.pars[3]
  tot.trans = apply(ob.inc,2,cumsum)[trig.one + time_pts,] 
  # Cumulative transmissions at first trigger date + time_pts
  prevs = ob.prev[trig.one + time_pts,] # Prevalent cases at first trigger date + time_pts
  
  res = data.frame(rbind(time_pts+trig.one, t(tot.trans), 
                         100*(tot.trans[,1]-tot.trans[,2])/tot.trans[,1],
                         t(prevs), 100*(prevs[,1]-prevs[,2])/prevs[,1], 
                         sapply(trig.one + time_pts, hosp_bed_counts, intv)))
  res = round(res)
  names(res) = paste("OB.day.",time_pts,sep="")
  row.names(res) = c("Days since importation", 
                     "Transmissions",
                     "Transmissions with intervention",
                     "Decrease in transmission (%)",
                     "Prevalent cases",
                     "Prevalent cases with intervention", 
                     "Decrease in prevalence (%)", 
                     "Intervention hospital count", 
                     "Intervention hospital-day count", 
                     "Intervention bed-day count")
  res
}

# END postprocessing function definition

##############################################################################
####   simulation begins here ################################################
##############################################################################
## NOTE : SIMULATIONS IS COMMENTED HERE TO SHOW WHICH ONES HAS BEEN RUN 

# # "Clean" run with uniform betas:
source("TN_contain_mainloop.R")

time.stamp = strftime(Sys.time(),format="%Y%m%d%H%M")
save(index_fac, int_mult, out, go_up_down, up_by_case_prob,
     pps.time, pps.neg, out.noint, trigger_event, interventions,
     new.cases, new.cases.noint,
     file=paste(savedruns1,index_fac,100*int_mult,time.stamp,sep="_"))


## Introduce random heterogeneity in betas
# Assume that beta estimates and CI are median and
# quantiles of a lognormal (to ensure positive betas)
# and estimate mu and sigma

#RANY'S CORRECTION FOR CI BETA IF USING TN SURV DATA
#(NEGATIVE PRODUCED NaNs in log scale, )
# CI_hrr_beta_est[1] =.00001

#Printing the beta parameters
print(c(hrr_beta_est, CI_hrr_beta_est))
print(c(ltbeta, sd_ltbeta))

st.mu.sigma = c(log(hrr_beta_est), diff(log(CI_hrr_beta_est))/
                  (2*qnorm(0.975)))
exp(st.mu.sigma)
lt.mu.sigma = c(log(ltbeta/sqrt(1+(sd_ltbeta/ltbeta)^2)), 
                sqrt(log(1+(sd_ltbeta/ltbeta)^2)))
lt.mu.sigma
exp(lt.mu.sigma)
factype = cut(pars.shea$tau0,c(-Inf,15,180,Inf), 
              labels=FALSE, include.lowest=TRUE) # 1=STH, 2=LTACH, 3=HRR

rand_seed_set = 41 +1:34
rand_seed_set

for (rand_seed in rand_seed_set) {
 set.seed(rand_seed)
 beta_noise = rep(1, length(occupancy))
 beta_noise[factype==1] = exp(rnorm(sum(factype==1), mean=st.mu.sigma[1], sd=st.mu.sigma[2]))/hrr_beta_est
 beta_noise[factype==2] = exp(rnorm(sum(factype==2), mean=lt.mu.sigma[1], sd=lt.mu.sigma[2]))/ltbeta
 beta_bakup = pars.shea$beta
 pars.shea$beta = pars.shea$beta * beta_noise
 source("TN_contain_mainloop.R")

 time.stamp = strftime(Sys.time(),format="%Y%m%d%H%M")
 save(index_fac, int_mult, out, go_up_down, up_by_case_prob,
      pps.time, pps.neg, out.noint, trigger_event, interventions,
      new.cases, new.cases.noint,
      file=paste(savedruns2,rand_seed,index_fac,100*int_mult,time.stamp,sep="_"))
 pars.shea$beta = beta_bakup
}

###############################################################################
#   STARTING HERE, IT'S POST PTOCESSING #
###############################################################################
#Starting at tn intervention with 80% effectiveness.

my_int_mult = 80
noint.list=list()
int.list = list()
intervention.list = list()
cases1095 = c()

myruns = list.files(savedfold, 
                    pattern="^manuscript[_]results[_][[:print:]]+[_][[:digit:]]{12}$")
myruns
run.pars = data.frame(t(sapply(myruns,
                               function(str) 
                                 as.numeric(strsplit(str,"_")[[1]][3:6])))) # Warnings OK
names(run.pars) = c("RAND_SEED","IND_FAC","INT_MULT","TIME_STAMP")

for (myrun in subset(myruns,
                     run.pars$INT_MULT==my_int_mult & 
                     !is.na(run.pars$RAND_SEED))) {
  load(paste(savedfold,myrun,sep=""))
  this.noint = data.frame(time=out.noint[-1,1],
                          inc.cases=rowSums(new.cases.noint))
  this.int = data.frame(time=out[-1,1],
                        inc.cases=rowSums(new.cases))
  noint.list = c(noint.list, this.noint)
  int.list = c(int.list, this.int)
  intervention.list = c(intervention.list, interventions)
  cases1095 = rbind(cases1095, 
    c(sum(occupancy*out.noint[findInterval(1095,out.noint[,"time"]),-1]),
    sum(occupancy*out[findInterval(1095,out[,"time"]),-1])))
}

# Mean Cases for Intervention 
mean.int = data.frame(time=round(unlist(int.list[2 * 1:(length(int.list)/2) - 1])),
                      inc.cases=unlist(int.list[2 * 1:(length(int.list)/2)]))
mean.int = tapply(mean.int$inc.case, mean.int$time, mean)
mean.int = data.frame(time= as.numeric(names(mean.int)), 
                      inc.cases=mean.int)

#Mean for No INtervention
mean.noint = data.frame(time=round(unlist(noint.list[2 * 1:(length(noint.list)/2) - 1])), 
                        inc.cases=unlist(noint.list[2 * 1:(length(noint.list)/2)]))
mean.noint = tapply(mean.noint$inc.case, mean.noint$time, mean)
mean.noint = data.frame(time= as.numeric(names(mean.noint)), inc.cases=mean.noint)

# Load "uniform" beta data:
unifrun = subset(myruns, run.pars$INT_MULT==my_int_mult & 
                   is.na(run.pars$RAND_SEED))
load(paste0(savedfold,unifrun))
unif.cases1095 = c(sum(occupancy*out.noint[findInterval(1095,out.noint[,"time"]),-1]),
    sum(occupancy*out[findInterval(1095,out[,"time"]),-1]))

# Mean reduction in case count at 1095 days
reduc1095 = 1-exp(median(log(cases1095[,2]/cases1095[,1])))
reduc1095
# Median and IQR
quantile(100 * (1- cases1095[,2]/cases1095[,1]), 1:3/4)


###############################################################################
### EPIDEMIC COURSE ###########################################################
tiff(paste0(graffold,"epicourse_v2_",my_int_mult,".tiff"),
     height=1300, width=2000, pointsize=40)
#!# BEGIN: FIG 2A
  
# lwd changed from 1 to 3 for uniform runs (2018-04-10)
yrange = range(0, sapply(2 * 1:(length(noint.list)/2), 
                         function(nn) max(noint.list[[nn]]) )) # Vertical range for plot
# Make empty plot;max_days * 0.9 for cosmetic reasons (remove edge effects)
plot(c(0,max_days * 0.9), yrange, type="n", 
     xlab = "Days since importation", 
     ylab="Transmissions/day") # xlab = "Time (days)" replaced 2019-02-19
# Plot lines
null.out = sapply(2 * 1:(length(noint.list)/2), 
                  function(nn) lines(noint.list[[nn-1]], noint.list[[nn]], 
                                     col="pink")) 
null.out = sapply(2 * 1:(length(int.list)/2), 
                  function(nn) lines(int.list[[nn-1]], int.list[[nn]], 
                                     col="lightblue"))
# Plot lines from "uniform" run
lines(out.noint[-1,1], rowSums(new.cases.noint), col="red", lwd=3, lty=2)
lines(out[-1,1], rowSums(new.cases), 
      col="blue", 
      lwd=3, lty=2)
# Plot means
lines(mean.int, lwd=3, col="blue")
lines(mean.noint, lwd=3, col="red")
legend("topleft",c("Without intervention","With intervention",
                   "Mean","Uniform transmission parameters"), 
       bty="n", 
  col=c("pink","lightblue","black","black"), 
  lwd = c(3,3,1,1),
  lty=c(1,1,1,2)) # lwd=6,6,3,3, not 1, for visibility?
#!# END: FIG 2A
dev.off()

###############################################################################
################ FIGURE 2 Median REDUCTION ###################################
###############################################################################

tiff(paste0(graffold,"cases1095_",my_int_mult,"_aspect1.tiff"),
     height=1300, width=1300, pointsize=36)
# Make empty plot
plot(rbind(c(0,0),cases1095,unif.cases1095)/1000, 
     xlab="Predicted cases (1000s) at 3 years without intervention",
  ylab="Predicted cases (1000s) at 3 years with intervention",
  type="n",xlim=c(0,max(cases1095[,1])/1000),
  ylim=c(0,max(cases1095[,1])/1000))
points(cases1095[,1]/1000,cases1095[,2]/1000,
       pch=19)
points(unif.cases1095[1]/1000,unif.cases1095[2]/1000,
       pch="X")
abline(a=0, b=1-reduc1095,col="grey25")

legend("topleft", 
       c(paste0("Median ",round(reduc1095*100),"% reduction"),
         "Uniform transmission parameters"), 
  lwd=c(1,NA), pch=c(NA,"X"), 
  col=c("grey25","black"),bty="n") 
dev.off()


################################################################################
################ NUMBER OF CASES WITH AND WITHOUT INTERVENTION #################
tiff(paste0(graffold, "cases1095_",my_int_mult,".tiff"),
     height=1300, width=1300, pointsize=40)
#!# BEGIN: FIG 2B
# Make empty plot
plot(rbind(c(0,0),cases1095,unif.cases1095)/1000,
     xlab="Predicted cases (1000s) at 3 years without intervention",
  ylab="Predicted cases (1000s) at 3 years with intervention", type="n")
points(cases1095[,1]/1000,cases1095[,2]/1000,pch=19)
points(unif.cases1095[1]/1000,unif.cases1095[2]/1000,pch="X")
abline(a=0, b=1-reduc1095,col="grey25",lwd=2)
legend("topleft",
       c(paste0("Median ",round(reduc1095*100),"% reduction"),
         "Uniform transmission parameters"), 
  lwd=c(1,NA), pch=c(NA,"X"), col=c("grey25","black"),bty="n") 
#!# END: FIG 2B
dev.off()

#############################################################################

# Third year intervention summary
day1095.hosp.counts = t(sapply(1:(length(intervention.list)/3)*3, 
  function(n3) hosp_bed_counts(1095,data.frame(intervention.list[n3-2:0]))))

day730.hosp.counts = t(sapply(1:(length(intervention.list)/3)*3, 
  function(n3) hosp_bed_counts(730,data.frame(intervention.list[n3-2:0]))))

# 	Number of hospitals enlisted in three years
summary(day1095.hosp.counts[,1])
#	Fraction of hospital patient days intervened in over third year
summary((day1095.hosp.counts-day730.hosp.counts)[,3]/
          sum(365*occupancy[1:144]))

####################################################################################
# Run with int_mult 0.95, 0.20
#"Clean" run with uniform betas:
int_mult = 0.95 ## If the intervention is only 20% effective
# source("TN_contain_mainloop.R")
# 
# time.stamp = strftime(Sys.time(),format="%Y%m%d%H%M")
# save(index_fac, int_mult, out, go_up_down, up_by_case_prob,
#   pps.time, pps.neg, out.noint, trigger_event, interventions, new.cases, new.cases.noint,
#   file=paste(savedruns1,index_fac,100*int_mult,time.stamp,sep="_"))
# 
# 
# ## Introduce random heterogeneity in betas
# # Assume that beta estimates and CI are median and quantiles of a lognormal (to ensure positive betas)
# # and estimate mu and sigma
# st.mu.sigma = c(log(hrr_beta_est), diff(log(CI_hrr_beta_est))/(2*qnorm(0.975)))
# lt.mu.sigma = c(log(ltbeta/sqrt(1+(sd_ltbeta/ltbeta)^2)), sqrt(log(1+(sd_ltbeta/ltbeta)^2)))
# factype = cut(pars.shea$tau0,c(-Inf,15,180,Inf), labels=FALSE, include.lowest=TRUE) # 1=STH, 2=LTACH, 3=HRR
# rand_seed_set = 41 + 1:34
# for (rand_seed in rand_seed_set) {
#  set.seed(rand_seed)
#  beta_noise = rep(1, length(occupancy))
#  beta_noise[factype==1] = exp(rnorm(sum(factype==1), mean=st.mu.sigma[1], sd=st.mu.sigma[2]))/hrr_beta_est
#  beta_noise[factype==2] = exp(rnorm(sum(factype==2), mean=lt.mu.sigma[1], sd=lt.mu.sigma[2]))/ltbeta
#  beta_bakup = pars.shea$beta
#  pars.shea$beta = pars.shea$beta * beta_noise
#  source("TN_contain_mainloop.R")
# 
#  time.stamp = strftime(Sys.time(),format="%Y%m%d%H%M")
#  save(index_fac, int_mult, out, go_up_down, up_by_case_prob,
#   pps.time, pps.neg, out.noint, trigger_event, interventions, new.cases, new.cases.noint, rand_seed,
#   file=paste(savedruns2,rand_seed,index_fac,100*int_mult,time.stamp,sep="_"))
# 
#  pars.shea$beta = beta_bakup
# }

####################################################################################
## Run with random int_mult (2018-01-03)

## Introduce random heterogeneity in betas
# Assume that beta estimates and CI are median and quantiles of a lognormal (to ensure positive betas)
# and estimate mu and sigma

# 
# ##Adding a SUBFOLDER TO SAVE THE RESULTS OF THE SIMS WITH RANDOM EFFECTIVENESS

# multifile
# 
# st.mu.sigma = c(log(hrr_beta_est), 
#                 diff(log(CI_hrr_beta_est))/(2*qnorm(0.975)))
# lt.mu.sigma = c(log(ltbeta/sqrt(1+(sd_ltbeta/ltbeta)^2)), 
#                 sqrt(log(1+(sd_ltbeta/ltbeta)^2)))
# factype = cut(pars.shea$tau0,c(-Inf,15,180,Inf),
#               labels=FALSE, include.lowest=TRUE) # 1=STH, 2=LTACH, 3=HRR
# 
# rand_seed_set = 1:50
# for (rand_seed in rand_seed_set) {
#  set.seed(rand_seed)
#  beta_noise = rep(1, length(occupancy))
#  beta_noise[factype==1] = exp(rnorm(sum(factype==1),
#                                     mean=st.mu.sigma[1],
#                                     sd=st.mu.sigma[2]))/hrr_beta_est
#  beta_noise[factype==2] = exp(rnorm(sum(factype==2), 
#                                     mean=lt.mu.sigma[1], 
#                                     sd=lt.mu.sigma[2]))/ltbeta
#  beta_bakup = pars.shea$beta
#  pars.shea$beta = pars.shea$beta * beta_noise
# 
#  logit.int_mult=rnorm(1,mean=log(3)); 
#  int_mult=round(exp(logit.int_mult)/
#                   (1+exp(logit.int_mult)),2)
# 
#  source("TN_contain_mainloop.R")
# 
#  time.stamp = strftime(Sys.time(),format="%Y%m%d%H%M")
#  save(index_fac, int_mult, out, go_up_down, up_by_case_prob, 
#   pps.time, pps.neg, out.noint, trigger_event, interventions, new.cases, new.cases.noint, rand_seed,
#   file=paste(multifile,rand_seed,index_fac,100*int_mult,time.stamp,sep="_")) 
# 
#  pars.shea$beta = beta_bakup
# }

###################################################
## Post-processing with different values of int_mult
# Option 1: 34 runs at each of 4 int_mult values

run.dir = savedfold
noint.list=list()
int.list = list()
intervention.list = list()
cases1095 = c()
myruns = list.files(run.dir, pattern="^manuscript[_]results[_][[:print:]]+[_][[:digit:]]{12}$")
myruns
run.pars = data.frame(t(sapply(myruns,function(str) as.numeric(strsplit(str,"_")[[1]][3:6])))) # Warnings OK
names(run.pars) = c("RAND_SEED","IND_FAC","INT_MULT","TIME_STAMP")
for (myrun in myruns) {
  load(paste(run.dir,myrun,sep=""))
  this.noint = data.frame(time=out.noint[-1,1],inc.cases=rowSums(new.cases.noint))
  this.int = data.frame(time=out[-1,1],inc.cases=rowSums(new.cases))
  noint.list = c(noint.list, this.noint)
  int.list = c(int.list, this.int)
  intervention.list = c(intervention.list, interventions)
  cases1095 = rbind(cases1095,
    c(sum(occupancy*out.noint[findInterval(1095,out.noint[,"time"]),-1]),
    sum(occupancy*out[findInterval(1095,out[,"time"]),-1])))
}


reduc1095 = 1-exp(tapply(log(cases1095[,2]/cases1095[,1]), 
                         run.pars$INT_MULT, mean))
reduc1095

###############################################################################
################### FIGURE: REDUCTION 3 YEARS AFTER INTERVENTION WITH 80 VS 20% 
### INTERVENTION EFFECTIVENESS #################################################
colnames(cases1095) = c("noint", "int")

tiff(paste0(graffold,"reduc1095_vs_reducbeta_opt1.tiff"),
     height=1500, width=1500, pointsize=40)
plot(jitter(100-run.pars$INT_MULT),
     100*(1-cases1095[,2]/cases1095[,1]),
     pch=19, cex=0.8, col=run.pars$INT_MULT,
     ylim=c(0,100),
  xlab="Reduction in transmission (%)",
  ylab="Reduction in case count 3 years after importation (%)",
  main= "Reduction in case count from reduction in intra-facility/ ",
  subtitle="Number of transmission due to infection control measures")

lines(100-as.numeric(names(reduc1095)),
      100*reduc1095,col="forestgreen",lwd=3)
dev.off()

##########
# Option 2: 50 runs at random int_mult values
run.dir = paste0(savedfold,"Multiple Effectiveness/")
noint.list=list()
int.list = list()
intervention.list = list()
cases1095 = c()
myruns = list.files(run.dir, pattern="^manuscript[_]results[_][[:print:]]+[_][[:digit:]]{12}$")
run.pars = data.frame(t(sapply(myruns,function(str) as.numeric(strsplit(str,"_")[[1]][3:6])))) # Warnings OK
names(run.pars) = c("RAND_SEED","IND_FAC","INT_MULT","TIME_STAMP")
for (myrun in myruns) {
  load(paste(run.dir,myrun,sep=""))
  this.noint = data.frame(time=out.noint[-1,1],inc.cases=rowSums(new.cases.noint))
  this.int = data.frame(time=out[-1,1],inc.cases=rowSums(new.cases))
  noint.list = c(noint.list, this.noint)
  int.list = c(int.list, this.int)
  intervention.list = c(intervention.list, interventions)
  cases1095 = rbind(cases1095, 
    c(sum(occupancy*out.noint[findInterval(1095,out.noint[,"time"]),-1]),
    sum(occupancy*out[findInterval(1095,out[,"time"]),-1])))
}



######################################################################

fit_0=loess(I(100*(1-cases1095[,2]/cases1095[,1]))~I(100-run.pars$INT_MULT))
tiff(paste0(graffold,"reduc1095_vs_reducbeta_opt2.tiff"),
            height=1000, width=1200, pointsize=30)
#!# BEGIN: FIG 4A
plot(100-run.pars$INT_MULT, 100*(1-cases1095[,2]/cases1095[,1]), pch=19, cex=0.8, ylim=c(0,100),
  xlab="% reduction in transmission parameter",
  ylab="% reduction in cases three years into outbreak") # 3 changed to three 2019-04-08
lines(0:100,predict(fit_0,0:100),col="blue", lwd=2)
abline(v=50, col="grey30", lty=2)
#!# END: FIG 4A
dev.off()

# Intervention IP days (3rd year), data & grap (2018-05-09)
long.stay.facs = which(pars$tau0>=15 & pars$tau0<180) 
# Lower cut-off moved to 15 from 12 to be consistent with NHSN analysis (2019-02-27)
fac.int.days = c()
for (myrun in myruns) {
  load(paste(run.dir,myrun,sep=""))
  int3 = subset(interventions, time<=1095 & time.stop>=730)
  int3$time = pmax(int3$time,730)
  int3$time.stop = pmin(int3$time.stop,1095)
  fac.int.days = rbind(fac.int.days, data.frame(
    total=sum(int3$time.stop-int3$time),
    total.ipdays = sum(occupancy[int3$fac] * (int3$time.stop-int3$time)),
    long=with(subset(int3, fac %in% long.stay.facs), sum(time.stop-time)),
    long.ipdays = sum(occupancy[int3$fac] * (int3$time.stop-int3$time)*(int3$fac %in% long.stay.facs)) )) # long.ipdays added 2019-02-19
}

sys.ipdays = sum(occupancy[pars$tau0<180])*(1095-730) # Total IP days at all hospitals; added 2018-04-10
sys.ipdays
fit_1=loess(I(100*fac.int.days$total.ipdays/sys.ipdays)~I(100-run.pars$INT_MULT))
tiff(paste0(graffold,"intervention_share_ipdays.tiff"), 
     height=800, width=800, #pointsize=60
     )
#par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,0,-1))
#!# BEGIN: FIG 4B
plot(100-run.pars$INT_MULT,
     100*fac.int.days$total/sys.ipdays,
     pch=19, col="black",
     cex=0.8, ylim=c(0,100),
  xlab="Percent reduction in transmission parameter",
  ylab="Percent of inpatient days under intervention/n(third year of outbreak)")
lines(0:100,predict(fit_1,0:100),col="blue", lwd=2)
#!# END: FIG 4B
dev.off()

#####
## SHEA 2018 numbers
# Use the 4 fixed int_mult runs
reduc = tapply(100*(1-cases1095[,2]/cases1095[,1]),100-run.pars$INT_MULT,mean)
reduc_IQR = tapply(100*(1-cases1095[,2]/cases1095[,1]),100-run.pars$INT_MULT,quantile, c(0.25,0.75))


# Third year intervention summary
day1095.hosp.counts = t(sapply(1:(length(intervention.list)/3)*3, 
  function(n3) hosp_bed_counts(1095,data.frame(intervention.list[n3-2:0]))))
day730.hosp.counts = t(sapply(1:(length(intervention.list)/3)*3, 
  function(n3) hosp_bed_counts(730,data.frame(intervention.list[n3-2:0]))))
# 	Number of hospitals enlisted in three years
tapply(day1095.hosp.counts[,1],100-run.pars$INT_MULT,summary)
#	Fraction of hospital patient days intervened in over third year
tapply((day1095.hosp.counts-day730.hosp.counts)[,3]/sum(365*occupancy[1:160]),100-run.pars$INT_MULT,summary)

###############

n_fac_type = nchar(dimnames(pars$in.fractions)[[1]]) # hospitals 6, community 3
run.dir = savedfold
noint.list=list()
int.list = list()
intervention.list = list()
cases1095 = c()
myruns = list.files(run.dir, pattern="^manuscript[_]results[_][[:print:]]+[_][[:digit:]]{12}$")
run.pars = data.frame(t(sapply(myruns,function(str) as.numeric(strsplit(str,"_")[[1]][3:6])))) # Warnings OK
names(run.pars) = c("RAND_SEED","IND_FAC","INT_MULT","TIME_STAMP")
for (myrun in myruns) {
  load(paste(run.dir,myrun,sep=""))
  this.noint = data.frame(time=out.noint[-1,1],inc.cases=rowSums(new.cases.noint))
  this.int = data.frame(time=out[-1,1],inc.cases=rowSums(new.cases))
  noint.list = c(noint.list, this.noint)
  int.list = c(int.list, this.int)
  intervention.list = c(intervention.list, interventions)
  cases1095 = rbind(cases1095, 
    c(sum(occupancy*out.noint[findInterval(1095,out.noint[,"time"]),-1]),
    sum(occupancy*out[findInterval(1095,out[,"time"]),-1]),
    sum((n_fac_type<5)*occupancy*out.noint[findInterval(1095,out.noint[,"time"]),-1]),
    sum((n_fac_type<5)*occupancy*out[findInterval(1095,out[,"time"]),-1])
))
}
cases1095 = data.frame(cases1095)
names(cases1095) = c("NOINT","INT","NOINT_COMM","INT_COMM")

cases1095 = cases1095[run.pars$INT_MULT==80,]

round(100*(1-exp(mean(log(cases1095[,2]/cases1095[,1])))),1)
round(100*quantile(1-cases1095[,2]/cases1095[,1],c(0.25,0.75)),1)
mean(cases1095[,1]-cases1095[,2])*frac_detect
quantile(cases1095[,1]-cases1095[,2],c(0.25,.75))*frac_detect

summary(cases1095$INT_COMM/cases1095$INT)
summary(cases1095$NOINT_COMM/cases1095$NOINT)

# 
# ## Epicenters meeting sub-analysis (2018-03-8)
# # total.ipdays added for SHEA 2018 (2018-04-10)
# # Fraction of facililty-days of intervention at longer stay (12-180 days) facilities
# long.stay.facs = which(pars$tau0>=12 & pars$tau0<180)
# run.dir = savedfold
# myruns = list.files(run.dir, pattern="^manuscript[_]results[_][[:print:]]+[_][[:digit:]]{12}$")
# run.pars = data.frame(t(sapply(myruns,function(str) as.numeric(strsplit(str,"_")[[1]][3:6])))) # Warnings OK
# names(run.pars) = c("RAND_SEED","IND_FAC","INT_MULT","TIME_STAMP")
# fac.int.days = c()
# for (myrun in myruns) {
#   load(paste(run.dir,myrun,sep=""))
#   int3 = subset(interventions, time<=1095 & time.stop>=730)
#   int3$time = pmax(int3$time,730)
#   int3$time.stop = pmin(int3$time.stop,1095)
#   fac.int.days = rbind(fac.int.days, data.frame(
#     total=sum(int3$time.stop-int3$time),
#     total.ipdays = sum(occupancy[int3$fac] * (int3$time.stop-int3$time)),
#     long=with(subset(int3, fac %in% long.stay.facs), sum(time.stop-time)) ))
# }
# 
# sys.ipdays = sum(occupancy[pars$tau0<180])*(1095-730) # Total IP days at all hospitals; added 2018-04-10

# Use INT_MULT = 50, 80, 95
# fac.int.days = fac.int.days[run.pars$INT_MULT %in% c(50,80,95),]
# run.pars = run.pars[run.pars$INT_MULT %in% c(50,80,95),]

tiff(paste0(graffold,"long_share_boxplots.tiff"),width=1800,height=1500,pointsize=40)
bp = boxplot(100*fac.int.days$long/fac.int.days$total~I(100-as.numeric(run.pars$INT_MULT)),
  ylim=c(0,max(100*fac.int.days$long/fac.int.days$total)),
  xlab = "Transmission parameter reduction (%)",
  ylab = "Proportion of intervention hospitals that are long-stay (%)")
abline(h = 100*length(long.stay.facs)/sum(pars$tau0<=180), col="red")
text(0.5+length(bp$names)/2, 120*length(long.stay.facs)/sum(pars$tau0<=180), 
  paste0("Proportion of hospitals that are long-stay (",round(100*length(long.stay.facs)/sum(pars$tau0<=180),1),"%)"), col="red")
dev.off()

tiff(paste0(graffold, "intervention_share_ipdays_boxplots.tiff"),
     width=1800,height=1500,pointsize=40)
# tiff("ICEID_intervention_share_ipdays_boxplots.tiff",width=1800,height=1500,pointsize=48)
bp = boxplot(100*fac.int.days$total.ipdays/sys.ipdays~I(100-as.numeric(run.pars$INT_MULT)),
  ylim=c(0,max(100*fac.int.days$total.ipdays/sys.ipdays)),
  xlab = "Transmission parameter reduction (%)",
  ylab = "Fraction of inpatient-days under intervention (%)")
dev.off()

#############################################################
## Clearance stuff
# Figure update: Figure 1
# A modified CREvsLOS_NHSN plot with two HRRS picked out as examples for short stay
# Run all preparatory steps in CRE_Isaac/SHEA2017/NY_SIS_modeling_SHEAversionFROZEN.R first
# NHSN: plot for beta estimates
# Sequence generated for WIP slides (2017-11-29)
# tiff(paste0(graffold, "CREvsLOS_NHSN_2HRRs_MS.tiff"),
#      height=2000,width=2000,pointsize=60)
# plot(cre_count$avg_los,cre_count$Freq/cre_count$Freq.y,xlim=c(2,50),log="xy",
# 	xlab="Median length of stay (days)",ylab="Percent of patients CRE lab positive",pch=19,cex=0.5,
# 	col="grey40", axes=FALSE)
# axis(1)
# axis(2, at=5*10^(-(5:2)), labels=c(0.005,0.05,0.5,5))
# box()
# 
# t0 = seq(2,15,by=0.2)
# pred303 = exp(predict(mylnlmhrr,data.frame(avg_los=t0,hrrnum=303)))
# dat303 = subset(cre_count_hrr,hrrnum==303 & avg_los<15)
# pred451 = exp(predict(mylnlmhrr,data.frame(avg_los=t0,hrrnum=451)))
# dat451 = subset(cre_count_hrr,hrrnum==451 & avg_los<15)
# 
# points(dat303$avg_los, dat303$Freq/dat303$Freq.y,pch=19,cex=0.7,col="red")
# lines(t0, pred303,col="red",lwd=2)
# points(dat451$avg_los, dat451$Freq/dat451$Freq.y,pch=19,cex=0.7,col="blue")
# lines(t0, pred451,col="blue",lwd=2)
# 
# # lines(t0<-seq(2,15,by=0.2),exp(predict(mylnlm1,data.frame(avg_los=t0))),col=ncezid.colors[4],lwd=1)
# lines(t0<-seq(15,50,by=0.2),exp(predict(mylnlm2,data.frame(avg_los=t0))),col=ncezid.colors[5],lwd=2)
# abline(v=15,lty=2)
# text(c(5,30),0.05,c("Short stay","Long stay"))
# title(main = 
#  "Positive CRE lab tests per admission (NHSN 2015)")
# dev.off()

###########################################
# CID manuscript figures (EPS, etc.)
# 2019-02-15
windows(width=11)
winfrac=0.2
par(fig=c(winfrac,1,0,1))
postscript("???.eps",..., colormodel="cmyk", family="Times")
mypar=par(no.readonly = TRUE)
par(fig=c(0,winfrac,0,1),mar=mypar$mar+c(0,-2,0,1),new=TRUE)

######
## Fig 1: adapted from MIND/notes_20170919.R
# A modified CREvsLOS_NHSN plot with two HRRS picked out as examples for short stay
# Run all preparatory steps in CRE_Isaac/SHEA2017/NY_SIS_modeling_SHEAversionFROZEN.R first

# NHSN: plot for beta estimates
# Sequence generated for WIP slides (2017-11-29)
# postscript(file=paste0(graffold, "revised_fig1.eps"),
#            height=5,width=5,
#            colormodel="cmyk", family="Times", paper="special", 
#            horizontal=FALSE)
# plot(cre_count$avg_los,cre_count$Freq/cre_count$Freq.y,xlim=c(2,50),log="xy",
# 	xlab="Median length of stay (days)",ylab="Proportion of patients CRE lab positive",pch=19,cex=0.5,
# 	col="grey40")
# 
# t0 = seq(2,15,by=0.2)
# pred303 = exp(predict(mylnlmhrr,data.frame(avg_los=t0,hrrnum=303)))
# dat303 = subset(cre_count_hrr,hrrnum==303 & avg_los<15)
# pred451 = exp(predict(mylnlmhrr,data.frame(avg_los=t0,hrrnum=451)))
# dat451 = subset(cre_count_hrr,hrrnum==451 & avg_los<15)
# 
# points(dat303$avg_los, dat303$Freq/dat303$Freq.y,pch=19,cex=0.6,col="red")
# lines(t0, pred303,col="red")
# points(dat451$avg_los, dat451$Freq/dat451$Freq.y,pch=19,cex=0.6,col="blue")
# lines(t0, pred451,col="blue")
# 
# lines(t0<-seq(15,50,by=0.2),exp(predict(mylnlm2,data.frame(avg_los=t0))))
# 
# abline(v=15,col="grey",lty=2)
# text(c(5,30),0.05,c("Short stay","Long stay"))
# dev.off()
## End figure 1
######

# Hack to redo figures
mycode = readLines("TN_contain_manuscript.R")
mycode = gsub("([Tt])ransmission parameter[s]*", "//1ransmissibility", mycode)
mycode1 = readLines("TN_contain.R") # Figure 3 - rainbow plot
mycode1 = gsub("Reduction in transmission", "Reduction in transmissibility", mycode1)

fig2a = grep("^#!#[[:print:]]+FIG 2A", mycode)
fig2b = grep("^#!#[[:print:]]+FIG 2B", mycode)

postscript(paste0(graffold,"revised_fig2.eps"),
           height=4, width=8, colormodel="cmyk", family="Times", 
           pointsize=10) # paper="special", horizontal=FALSE)
par(fig=c(0,7/12,0,1))
eval(parse(text=mycode[fig2a[1]:fig2a[2]]))
par(fig=c(7/12,1,0,1), new=TRUE)
eval(parse(text=mycode[fig2b[1]:fig2b[2]]))
dev.off()

fig3 = grep("^#!#[[:print:]]+FIG 3", mycode1)
postscript("revised_fig3.eps", height=5, width=8, colormodel="cmyk", family="Times", paper="special", horizontal=FALSE)
eval(parse(text=mycode1[fig3[1]:fig3[2]]))
dev.off()

fig4a = grep("^#!#[[:print:]]+FIG 4A", mycode)
fig4b = grep("^#!#[[:print:]]+FIG 4B", mycode)
# windows(width=12, height=4)
# tiff("revised_fig4.tiff", height=2000, width=6000, pointsize=60)
postscript("revised_fig4.eps", height=3.5, width=8, colormodel="cmyk", family="Times", pointsize=10) # paper="special", horizontal=FALSE)
par(fig=c(0,1/3,0,1))
eval(parse(text=mycode[fig4a[1]:fig4a[2]]))
par(fig=c(1/3,2/3,0,1), new=TRUE)
eval(parse(text=mycode[fig4b[1]:fig4b[2]]))
par(fig=c(2/3,1,0,1), new=TRUE)
plot(100-run.pars$INT_MULT, 100*fac.int.days$long.ipdays/fac.int.days$total.ipdays, pch=19, cex=0.8, ylim=c(0,20),
  xlab="Percent reduction in transmissibility",
  ylab="Long stay share (%) of inpatient days /nunder intervention (third year of outbreak)")
fit_2=loess(I(100*fac.int.days$long.ipdays/fac.int.days$total.ipdays)~I(100-run.pars$INT_MULT))
lines(0:100,predict(fit_2,0:100),col="blue", lwd=2)
long.occ.share = 100*sum(occupancy[long.stay.facs])/sum(occupancy[pars$tau0<=180])
abline(h=long.occ.share, lty=2, lwd=1)
text(45, 1.25*long.occ.share, paste0(round(long.occ.share,1),"%"), cex=0.8)
text(45, 0.75*long.occ.share, "Long stay share of patients", cex=0.8)
dev.off()

# Trying to straighten out the figure (2019-04-05)
postscript("rerevised_fig4.eps", height=15, width=4, colormodel="cmyk", family="Times", pointsize=10) # paper="special", horizontal=FALSE)
par(fig=c(0,1,2/3,1), mar=c(5,6,0,0)+0.1)
eval(parse(text=gsub("blue", "black", gsub("3 years", "/n3 years", mycode[fig4a[1]:fig4a[2]])))) # gsub added 2019-04-05, 2019-04-08
par(fig=c(0,1,1/3,2/3), mar=c(5,6,0,0)+0.1, new=TRUE)
eval(parse(text=gsub("blue", "black", gsub("intervention////n[(]third", "/nintervention (third", mycode[fig4b[1]:fig4b[2]]))))
par(fig=c(0,1,0,1/3), mar=c(5,6,0,0)+0.1, new=TRUE)
plot(100-run.pars$INT_MULT, 100*fac.int.days$long.ipdays/fac.int.days$total.ipdays, pch=19, cex=0.8, ylim=c(0,20),
  xlab="Percent reduction in transmissibility",
  ylab="Long stay share (%) of inpatient/ndays under intervention/n(third year of outbreak)")
fit_2=loess(I(100*fac.int.days$long.ipdays/fac.int.days$total.ipdays)~I(100-run.pars$INT_MULT))
lines(0:100,predict(fit_2,0:100), lwd=2)
long.occ.share = 100*sum(occupancy[long.stay.facs])/sum(occupancy[pars$tau0<=180])
abline(h=long.occ.share, lty=2, lwd=1)
text(45, 1.25*long.occ.share, paste0(round(long.occ.share,1),"%"), cex=0.8)
text(45, 0.75*long.occ.share, "Long stay share of patients", cex=0.8)
dev.off()

############
# Manuscript review/revision (2019-02-19)
int_delay = 7
# Uniform beta
for (int_mult in c(0.80, 0.95, 0.50, 0.90, 0.20)) {
source("TN_contain_mainloop.R")
time.stamp = strftime(Sys.time(),format="%Y%m%d%H%M")
save(index_fac, int_mult, int_delay, out, go_up_down, up_by_case_prob, 
  pps.time, pps.neg, out.noint, trigger_event, interventions, new.cases, new.cases.noint,
  file=paste(savedruns1,index_fac,100*int_mult,time.stamp,sep="_")) 
}

run.dir = savedfold
myruns = list.files(run.dir, pattern="^manuscript[[:print:]]+[_]uniform")

sens.analysis1 = c()
for (myrun in myruns) {
  int_delay = 30 # Default
  load(paste0(run.dir,myrun))
  
  sens.analysis1 = rbind(sens.analysis1, data.frame(
    INT_MULT=int_mult, INT_DELAY=int_delay,
    CASES1095_NOINT=sum(occupancy*out.noint[findInterval(1095,out.noint[,"time"]),-1]),
    CASES1095_INT=sum(occupancy*out[findInterval(1095,out[,"time"]),-1])
    ))
}

with(sens.analysis1, xtabs(100-100*CASES1095_INT/CASES1095_NOINT~INT_MULT+INT_DELAY))

#         INT_DELAY
# INT_MULT        7       30
#     0.2  93.85237 91.64930
#     0.5  86.97187 83.50516
#     0.8  74.80781 72.44900
#     0.9  50.86542 49.59665
#     0.95 29.86309 29.22411
# 
# 1-exp(quantile(log(cases1095[,2]/cases1095[,1])))
#        0%       25%       50%       75%      100% 
# 0.8352350 0.7742599 0.7464079 0.7251531 0.6665166

# Network figure similar to net_layout from SHEA 2018