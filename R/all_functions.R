# Log-Normal not working in this edition

##--##--##--##--##--##--##--##--##--##--##--##--##--
# Evaluating the derivative of log(Gamma(x))
# See https://www.nbi.dk/~polesen/borel/node5.html
# sq=1:10000000
# C=sum(1/sq)-log(max(sq)) # 0.57721566490153286060
# C:  Euler-Mascheroni Constant.
psi=function(x,k=10000){
# OBS: This function is not used!
	-0.57721566490153286060+sum(1/(1:k)-1/((1:k)+x-1))}

dpsi.dx=function(x,k=10000){
# Returns the derivative of the psi function above
	sum((1/((0:k)+x))^2)}

##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the normal distribution   ##
##--##--##--##--##--##--##--##--##--##--##--##--##--##
rtrunc.norm=function(n,mu,sigma,a,b){
	# n: Sample size
	# mu, sigma: mean and sd of "parent" distribution
	# a, b: points of left and right truncation
	# returns a sample of size n drawn from a truncated normal distribution
	# Note the effective sample size is reduced due to truncation
	y=rnorm(n,mu,sigma)
	if (!missing(a))
		y=y[y>=a]
	if (!missing(b))
		y=y[y<=b]
	return(y)
}

density.trunc.norm=function(y,eta,a=-Inf,b=Inf){
	parm=natural2parameters.norm(eta)
	dens=ifelse((y<a)|(y>b),0,dnorm(y,mean=parm[1],sd=parm[2]))
	if (!missing(a))
		F.a=pnorm(a,parm[1],parm[2])
	else
		F.a=0
	if (!missing(b))
		F.b=pnorm(b,parm[1],parm[2])
	else
		F.b=1
	const=1/(F.b-F.a)
	return(dens/(F.b-F.a))
}

init.parms.norm=function(y){
	# Returns empirical parameter estimates mu and sd
	parm=c(mu=mean(y),sd=sqrt(var(y)))
}

sufficient.T.norm=function(y){
	return(suff.T=cbind(y,y^2))
}

average.T.norm=function(y){
	return(apply(sufficient.T.norm(y),2,mean))
}

natural2parameters.norm=function(eta){
# eta: The natural parameters in a normal distribution
# returns (mu,sigma)
	return(c(mu=-0.5*eta[1]/eta[2],sd=sqrt(-0.5/eta[2])))
}

parameters2natural.norm=function(parms){
	# parms: The parameters mu and sd in a normal distribution
	# returns the natural parameters
	return(c(eta.1=parms[1],eta.2=-0.5)/parms[2]^2)
}

get.y.seq.norm=function(y,y.min,y.max,n=100){
	mu=mean(y,na.rm=T)
	sd=var(y,na.rm=T)^0.5
	lo=max(y.min,mu-3.5*sd)
	hi=min(y.max,mu+3.5*sd)
	return(seq(lo,hi,length=n))
}

get.grad.E.T.inv.norm=function(eta){
# eta: Natural parameter
# return the inverse of E.T differentiated with respect to eta' : p x p matrix
	return(A=solve(0.5*matrix(c(-1/eta[2],eta[1]/eta[2]^2,eta[1]/eta[2]^2,1/eta[2]^2-eta[1]^2/eta[2]^3),ncol=2)))
}

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Log Normal distribution   ##
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

sufficient.T.lognorm=function(y){
	return(suff.T=cbind(log(y),log(y)^2))
}

average.T.lognorm=function(y){
	return(apply(sufficient.T.lognorm(y),2,mean))
}

rtrunc.lognorm=function(n,mu,sigma,a,b){
	# n: Sample size
	# logmu, logsd: parameters of the un-truncated distribution
	# a, b: points of left and right truncation
	# returns a sample of size n drawn from a truncated log-normal distribution
	# Note the effective sample size is reduced due to truncation
	y=rlnorm(n,mu,sigma)
	if (!missing(a))
		y=y[y>=a]
	if (!missing(b))
		y=y[y<=b]
	return(y)
}

density.trunc.lognorm=function(y,eta,a=-Inf,b=Inf){
	parm=natural2parameters.norm(eta)
	dens=ifelse((y<a)|(y>b),0,dlnorm(y,meanlog=parm[1],sdlog=parm[2]))
	if (!missing(a))
		F.a=plnorm(a,parm[1],parm[2])
	else
		F.a=0
	if (!missing(b))
		F.b=plnorm(b,parm[1],parm[2])
	else
		F.b=1
	const=1/(F.b-F.a)
	return(dens/(F.b-F.a))
}

init.parms.lognorm=function(y){
	# Y~LN(mu,sigma) => X=log(Y)~N(mu,sigma)
	# Returns empirical parameter estimates for mu and sd
#browser()
	x=log(y)
	parm=c(mu=mean(x),sd=sqrt(var(x)))
}

get.y.seq.lognorm=function(y,y.min,y.max,n=100){
	x=log(y)
	mu=mean(x,na.rm=T)
	sd=var(x,na.rm=T)^0.5
	lo=max(y.min,exp(mu-3.5*sd))
	hi=min(y.max,exp(mu+3.5*sd))
	return(seq(lo,hi,length=n))
}

##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Gamma distribution    ##
##--##--##--##--##--##--##--##--##--##--##--##--##--##

rtrunc.gamma=function(n,alpha,beta,a,b){
	# n: Sample size
	# alpha, beta: shape and rate of "parent" distribution
	# a, b: points of left and right truncation
	# returns a sample of size n drawn from a truncated normal distribution
	# Note the effective sample size is reduced due to truncation
	y=rgamma(n,shape=alpha,rate=beta)
	if (!missing(a))
		y=y[y>=a]
	if (!missing(b))
		y=y[y<=b]
	return(y)
}

density.trunc.gamma=function(y,eta,a,b){
	parm=natural2parameters.gamma(eta)
	dens=ifelse((y<a)|(y>b),0,dgamma(y,shape=parm[1],rate=parm[2]))
	if (!missing(a))
		F.a=pgamma(a,shape=parm[1],rate=parm[2])
	else
		F.a=0
	if (!missing(b))
		F.b=pgamma(b,shape=parm[1],rate=parm[2])
	else
		F.b=1
	const=1/(F.b-F.a)
	return(dens/(F.b-F.a))
}

init.parms.gamma=function(y){
	# Returns  parameter estimates mu and sd
	amean=mean(y);avar=var(y)
	a=amean^2/avar
	parm=c(alpha=a,beta=a/amean)
}

sufficient.T.gamma=function(y){
	return(suff.T=cbind(log(y),y))
}

average.T.gamma=function(y){
	return(apply(cbind(log(y),y),2,mean))
}

natural2parameters.gamma=function(eta){
	# eta: The natural parameters in a gamma distribution
	# returns (alpha,beta)
	return(c(alpha=eta[1]+1,beta=-eta[2]))
}

parameters2natural.gamma=function(parms){
	# parms: The parameters alpha and beta in a gamma distribution
	# returns the natural parameters
	return(c(eta.1=parms[1]-1,eta.2=-parms[2]))
}

get.y.seq.gamma=function(y,y.min=1e-6,y.max,n=100){
# Bør chekkes
	mu=mean(y,na.rm=T)
	sd=var(y,na.rm=T)^0.5
	lo=max(y.min,mu-5*sd)
	hi=min(y.max,mu+5*sd)
	return(seq(lo,hi,length=n))
}

get.grad.E.T.inv.gamma=function(eta){
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta' : p x p matrix
	return(A=solve(matrix(c(-1/eta[1]^2+dpsi.dx(eta[1]),-1/eta[2],-1/eta[2],(eta[1]+1)/eta[2]^2),ncol=2)))
}

##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Poisson distribution   ##
##--##--##--##--##--##--##--##--##--##--##--##--##--##

rtrunc.pois=function(n,lambda,a,b){
	# n: Sample size
	# lambda: mean and var of "parent" distribution
	# a, b: points of left and right truncation
	# # OBS: a, and b are included in the domain
	# returns a sample of size n drawn from a truncated Poisson distribution
	# Note the effective sample size is reduced due to truncation
	y=rpois(n,lambda)
	if (!missing(a))
		y=y[y>=a]
	if (!missing(b))
		y=y[y<=b]
	return(y)
}

density.trunc.pois=function(y,eta,a=0,b){
	parm=exp(eta)
		dens=ifelse((y<a)|(y>b),0,dpois(y,parm))
	if (!missing(a))
		F.a=ppois(a-1,parm)
	else
		F.a=0
	if (!missing(b))
		F.b=ppois(b,parm)
	else
		F.b=1
	return(dens/(F.b-F.a))
}

init.parms.pois=function(y){
	# Returns empirical parameter estimate for lambda
	parm=mean(y)+5
}

sufficient.T.pois=function(y){
	return(suff.T=y)
}

average.T.pois=function(y){
	return(mean(y))
}

density.pois=function(y,eta){
	parms=exp(eta)
	dpois(y,parms)
}

natural2parameters.pois=function(eta){
	# eta: The natural parameters in a Poisson distribution
	# returns (mu,sigma)
	return(lambda=exp(eta))
}

parameters2natural.pois=function(parms){
	# parms: The parameter lambda in a Poisson distribution
	# returns the natural parameters
	return(eta=log(parms))
}

get.grad.E.T.inv.pois=function(eta){
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
	return(A=exp(-eta))
}

get.y.seq.pois=function(y,y.min=0,y.max,n=100){
	mu=mean(y,na.rm=T)
	var.y=var(y,na.rm=T)
	lo=max(round(y.min),0)
	hi=min(y.max,round(mu+10*sqrt(var.y)))
	return(lo:hi)
}


##--##--##--##--##--##--##--##--##--##--##--##--##--##
##   Functions related to the Binomial distribution   ##
##--##--##--##--##--##--##--##--##--##--##--##--##--##


rtrunc.binomial=function(n,prob,a,b,...){
	# n: Sample size
	# prob: probability of success on each trial in the "parent" distribution
	# a, b: points of left and right truncation
	# # OBS: a, and b are included in the domain
	# returns a sample of size n drawn from a truncated binomialson distribution
	# Note the effective sample size is reduced due to truncation
	y=rbinom(n,...,prob)
	if (!missing(a))
		y=y[y>=a]
	if (!missing(b))
		y=y[y<=b]
	return(y)
}

density.trunc.binomial=function(y,eta,a=0,b,...){
	my.dbinom=function(nsize){
		dbinom(y,size=nsize,prob=proba)}
	my.pbinom=function(z,nsize){
		pbinom(z,size=nsize,prob=proba)}
	proba=1/(1+exp(-eta))
	dens=ifelse((y<a)|(y>b),0,my.dbinom(...))
	if (!missing(a))
		F.a=my.pbinom(a-1,...)
	else
		F.a=0
	if (!missing(b))
		F.b=my.pbinom(b,...)
	else
		F.b=1
	return(dens/(F.b-F.a))
}

init.parms.binomial=function(y,...){
	# Returns empirical parameter estimate for lambda
	parm=mean(y/...)
}

sufficient.T.binomial=function(y){
	return(suff.T=y)
}

average.T.binomial=function(y){
	return(mean(y))
}

density.binomial=function(y,eta,...){
	parms=1/(1+exp(-eta))
	dbinom(y,...,parms)
}

natural2parameters.binomial=function(eta){
	# eta: The natural parameters in a binomial distribution
	# returns (p)
	return(p=1/(1+exp(-eta)))
}

parameters2natural.binomial=function(parms){
	# parms: The probability parameter p in a binomial distribution
	# returns the natural parameters
	return(eta=log(parms/(1-parms)))
}

get.grad.E.T.inv.binomial=function(eta,...){
	# eta: Natural parameter
	# return the inverse of E.T differentiated with respect to eta
	exp.eta=exp(eta)
	return(A=((1+exp.eta)^2/exp.eta)/...)
}

get.y.seq.binomial=function(y,y.min=0,y.max,n=100,...){
	nsize=0+...
	y.lo=round(y.min)
	y.hi=round(y.max)
	lo=max(y.lo,0)
	hi=min(y.max,nsize)
	return(lo:hi)
}

######~~######~~######~~######~~######~~######~~######~~######~~######~~######~~######~~######~~######~~######
# ----------**----------**----------**----------**----------**----------**----------**----------**----------**
######~~######~~######~~######~~######~~######~~######~~######~~######~~######~~######~~######~~######~~######

ml.estimation.trunc.dist=function(y,y.min=-Inf,y.max=Inf,family="Gaussian",tol=1e-5,max.it=25,delta=0.33,...){
	# ML-estimation of the parameters of the distribution of the specified
	# family, truncated at y.min and y.max
	# Inspired by Salvador: Pueyo: "Algorithm for the maximum likelihood estimation of
	# the parameters of the truncated normal and lognormal distributions"

	# 12-12-2020: Rewritten in vector and matrix format - Se notes...

	get.T.minus.E.T=function(eta){
		# y Sequence spannig the domain of the truncated distribution
		# Calculates T.bar-E(T|eta_j) by numerical integration
		delta.y=y.seq[2]-y.seq[1]                                          # step length, length(y.seq)=L
		trunc.density=density.trunc(y.seq,eta,y.min,y.max)                 # L vector
		T.f=sufficient.T(y.seq)*trunc.density                              # L x p matrix
		if (length(eta)>1){
			E.T.j=delta.y*apply(T.f,2,sum)                                  # 1 x p
			if (cont.dist==T)
				E.T.j=E.T.j-delta.y*0.5*(T.f[1,]+T.f[length(y.seq),])
			}
		else{
			E.T.j=delta.y*sum(T.f)
			if (cont.dist==T)
				E.T.j=E.T.j-delta.y*0.5*(T.f[1]+T.f[length(y.seq)])
			}
		T.bar.minus.E.T.j=T.avg-E.T.j                                      # 1 x p
		return(T.bar.minus.E.T.j)
	}

# Some initialisations
	if (family=="Gaussian"){
		cat("Normal\n")
		init.parms=init.parms.norm
		sufficient.T=sufficient.T.norm
		average.T=average.T.norm
		natural2parameters=natural2parameters.norm
		parameters2natural=parameters2natural.norm
		density.trunc=density.trunc.norm
		get.grad.E.T.inv=get.grad.E.T.inv.norm
		get.y.seq=get.y.seq.norm
		cont.dist=T
	}
	if (family=="LogNormal"){
		cat("Log Normal\n")
		init.parms=init.parms.lognorm
		sufficient.T=sufficient.T.lognorm
		average.T=average.T.lognorm
		natural2parameters=natural2parameters.norm
		parameters2natural=parameters2natural.norm
		density.trunc=density.trunc.lognorm
		get.grad.E.T.inv=get.grad.E.T.inv.norm
		get.y.seq=get.y.seq.lognorm
		cont.dist=T
	}
	if (family=="Gamma"){
		cat("Gamma\n")
		init.parms=init.parms.gamma
		sufficient.T=sufficient.T.gamma
		average.T=average.T.gamma
		natural2parameters=natural2parameters.gamma
		parameters2natural=parameters2natural.gamma
		density.trunc=density.trunc.gamma
		get.grad.E.T.inv=get.grad.E.T.inv.gamma
		get.y.seq=get.y.seq.gamma
		cont.dist=T
	}
	if (family=="Poisson")
	{
		cat("Poisson\n")
		init.parms=init.parms.pois
		sufficient.T=sufficient.T.pois
		average.T=average.T.pois
		natural2parameters=natural2parameters.pois
		parameters2natural=parameters2natural.pois
		density.trunc=density.trunc.pois
		get.grad.E.T.inv=get.grad.E.T.inv.pois
		get.y.seq=get.y.seq.pois
		cont.dist=F
	}
	if (family=="Binomial")
	{
		cat("Binomial\n")
		init.parms=init.parms.binomial
		sufficient.T=sufficient.T.binomial
		average.T=average.T.binomial
		natural2parameters=natural2parameters.binomial
		parameters2natural=parameters2natural.binomial
		density.trunc=density.trunc.binomial
		get.grad.E.T.inv=get.grad.E.T.inv.binomial
		get.y.seq=get.y.seq.binomial
		cont.dist=F
	}
#  browser()
	T.avg=average.T(y)
	eta.j=parameters2natural(init.parms(y))
	y.seq=get.y.seq(y,y.min,y.max,n=100,...)     # y-values to be used for calculation of the expectations
	it=0
	delta.L2=10000
# Now iterate
	while ((delta.L2>tol)&(it<max.it)){
		parm.j=natural2parameters(eta.j)
		T.minus.E.T=get.T.minus.E.T(eta.j)
		grad.E.T.inv=get.grad.E.T.inv(eta.j) # p x p
		delta.eta.j.plus.1=delta*grad.E.T.inv%*%T.minus.E.T
		eta.j=eta.j+delta.eta.j.plus.1
		delta.L2=sum(delta.eta.j.plus.1^2)
		it=it+1
		cat("it: ",it, "tol: ",delta.L2," - parm: ",round(parm.j,3),"\n")
	}
	parm=natural2parameters(eta.j)
	return(parm)
}