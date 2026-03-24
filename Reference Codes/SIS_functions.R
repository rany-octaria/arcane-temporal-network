##################################################################
## Helper functions
try(library(deSolve))
# DE function
SISmultifac=function(Time, State, Pars){
	with(as.list(Pars), {
		
		dState <- beta*State*(1-State)-State*(gamma+1/tau0)+c(State%*%in.fractions)/tau0
		return(list(dState))
	})
}
# End DE function

# Map PRVDR_NUM function
merge.facs = function(ccn) {
	sub.list=list(c("M",1),c("R",1),c("S",0),c("T",0),c("U",0),c("W",2),c("Y",3),c("Z",1))
	# Title XIX only stuff added 2017-11-17
	titleXIXonly = c("A","B","E","F","G","H","K","L","J","N")
	makeNA = (substr(ccn,3,3) %in% titleXIXonly)
	for (mysub in sub.list) {
		ccn=gsub(mysub[1],mysub[2],ccn)
	}
	ccn = as.numeric(ccn)
	ccn[makeNA] = NA
	ccn
}


# End map PRVDR_NUM function

# From cumulative daily new cases, produce epi-curve every l_period, for n_period (25 weeks default)
EpiCurve = function(HO.cases, l_period = 7, n_period = 25) { 
	res = apply(rbind(0,HO.cases[1:n_period*l_period,]),2,diff)
	rownames(res) = 1:n_period
	res
}

# Single facility functions:
SIShosp = function(v0, p0, tau0, beta, gamma,tau) { # beta = R0*gamma; tau is clock time
	RH = beta*tau0
	CC = -1+0i
	BB = 1-gamma/beta-1/RH
	AA = p0/RH
	sqrtDD = sqrt(BB^2-4*AA*CC)
	x0 = 2*atanh((2*v0-BB)/sqrtDD)
	t = tau*beta
	Re(sqrtDD/2*tanh((sqrtDD*t+x0)/2) + BB/2)
}

NewCases = function(N_pat, tau0, beta, gamma,tau) { # Cumulative total of new cases due to one initial case
	RH = beta*tau0
	BB = 1-gamma/beta-1/RH
	v0 = 1/N_pat # Initial prevalence
	sqrtDD = sqrt(BB^2+4*0i)
	x0 = 2*atanh((2*v0-BB)/sqrtDD)
	t = tau*beta
	v = Re(sqrtDD/2*tanh((sqrtDD*t+x0)/2) + BB/2)
	int_v = (1/beta) * Re( log( cosh((sqrtDD*t+x0)/2)/cosh(x0/2) ) + t*BB/2 )
	N_pat * (v-v0 + (gamma+1/tau0)*int_v)
}
# End single facility functions

# Other functions:
p.discharge=function(p0,R0,tau0,gamma,max=TRUE) {
  if (R0==0) res = p0/(1 + gamma * tau0) else {
    t0=gamma*tau0
    RH=R0*t0
    CC=-1
    BB=1-1/R0-1/RH
    AA=p0/RH
    res=(-BB+c(1,-1)*sqrt(BB^2-4*AA*CC))/(2*CC)
  }
  if (max) max(res) else res}

v.p.discharge=Vectorize(p.discharge)

SISmultifacSteadyState = function(pars) {
  v0 = 0.001 + 0 * rowSums(pars$in.fractions) # small seed value
  within(pars,while(mean(abs(v0 - 
                             (v0<-v.p.discharge(c(v0%*%pars$in.fractions),pars$beta/pars$gamma,pars$tau0,pars$gamma))))>10^(-10)) {})$v0
}

# Maximum eigenvalue (real part); if <0, v=0 is a stable point; if >0, sustains outbreak
maxEV = function(pars) max(Re(eigen(t(pars$in.fractions)/pars$tau0+diag(pars$beta-pars$gamma-1/pars$tau0))$values))

# Top two Re(eigenvalues) of weighted adjacency matrix (ensure influx = outflux?)
EV2 = function(transmat) {
  re.ev = Re(eigen(t(transmat/rowSums(transmat)))$values)
  re.ev = sort(re.ev, decreasing = TRUE)
  re.ev[1:2]
}

# Occupancy consistent with detailed balance
normOccupancy = function(pars) {es = eigen(pars$in.fractions); ev = pars$tau0*es$vectors[,which(abs(es$values-1.0)<1e-6)]; ev/sum(ev)}

R0 = function(beta,gamma,tau,pij) {
  # pij is probability of admission to j given discharge from i
  # if trans.mat is available, pij=prop.table(trans.mat,1)
  mydim = max(length(beta),length(gamma),length(tau),dim(pij))
  if (length(beta)==1) beta = rep(beta,mydim)
  if (length(tau)==1) tau = rep(tau,mydim)
  if (length(gamma)==1) gamma = rep(gamma,mydim)
  solve(diag(beta+gamma+1/tau)-diag(1/tau)%*%pij,gamma)->P
  R0.tilde = beta/(gamma + (1-diag(pij))/tau) # "hospital" R0 adjusted for readmission
  list(P=P,R0=1/P-1,R0.tilde=R0.tilde)
}

##################################################################
