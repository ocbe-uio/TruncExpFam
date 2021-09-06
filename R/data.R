valid_fam_parm <- list(
	beta = list(
		family = c("beta"),
		parms = c("shape1", "shape2")
	),
	binomial = list(
		family = c("binomial"),
		parms  = c("size", "prob")
	),
	chisq = list(
		family = c("chisq"),
		parms  = c("df")
	),
	contbernoulli = list(
		family = c("contbernoulli"),
		parms  = c("lambda")
	),
	exp = list(
		family = c("exp"),
		parms  = c("rate")
	),
	gamma = list(
		family = c("gamma"),
		parms  = c("shape", "rate")
	),
	gamma = list(
		family = c("gamma"),
		parms  = c("shape", "scale")
	),
	invgamma = list(
		family = c("invgamma"),
		parms  = c("shape", "rate")
	),
	invgamma = list(
		family = c("invgamma"),
		parms  = c("shape", "scale")
	),
	invgauss = list(
		family = c("invgauss"),
		parms  = c("m", "s")
	),
	lognormal = list(
		family = c("lognormal"),
		parms  = c("meanlog", "sdlog")
	),
	nbinom = list(
		family = c("nbinom"),
		parms  = c("size", "prob", "mu")
	),
	normal = list(
		family = c("normal", "gaussian"),
		parms  = c("mean", "sd")
	),
	gaussian = list(
		family = c("normal", "gaussian"),
		parms  = c("mean", "sd")
	),
	poisson = list(
		family = c("poisson"),
		parms  = c("lambda")
	)
)
valid_distros <- names(valid_fam_parm)
