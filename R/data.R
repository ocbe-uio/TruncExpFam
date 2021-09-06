valid_fam_parm <- list(
	beta = list(
		family = c("beta"),
		parms = c("shape1", "shape2"),
		cont = TRUE
	),
	binomial = list(
		family = c("binomial"),
		parms  = c("size", "prob"),
		cont = FALSE
	),
	chisq = list(
		family = c("chisq"),
		parms  = c("df"),
		cont = TRUE
	),
	contbern = list(
		family = c("contbern"),
		parms  = c("lambda"),
		cont = TRUE
	),
	exp = list(
		family = c("exp"),
		parms  = c("rate"),
		cont = TRUE
	),
	gamma = list(
		family = c("gamma"),
		parms  = c("shape", "rate"),
		cont = TRUE
	),
	gamma = list(
		family = c("gamma"),
		parms  = c("shape", "scale"),
		cont = TRUE
	),
	invgamma = list(
		family = c("invgamma"),
		parms  = c("shape", "rate"),
		cont = TRUE
	),
	invgamma = list(
		family = c("invgamma"),
		parms  = c("shape", "scale"),
		cont = TRUE
	),
	invgauss = list(
		family = c("invgauss"),
		parms  = c("m", "s"),
		cont = TRUE
	),
	lognormal = list(
		family = c("lognormal"),
		parms  = c("meanlog", "sdlog"),
		cont = TRUE
	),
	nbinom = list(
		family = c("nbinom"),
		parms  = c("size", "prob"),
		cont = FALSE
	),
	normal = list(
		family = c("normal", "gaussian"),
		parms  = c("mean", "sd"),
		cont = TRUE
	),
	gaussian = list(
		family = c("normal", "gaussian"),
		parms  = c("mean", "sd"),
		cont = TRUE
	),
	poisson = list(
		family = c("poisson"),
		parms  = c("lambda"),
		cont = FALSE
	)
)
valid_distros <- names(valid_fam_parm)
