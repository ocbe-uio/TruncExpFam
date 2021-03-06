#' @title Generates an rtrunc-dispatchable class
#' @description Matches a list of arguments to an rtrunc method
#' @param n sample size
#' @param family distribution family
#' @param parms list of parameters passed to rtrunc (through the `...` element)
#' @return A character string.
#' @author Waldir Leoncio
genRtruncClass <- function(n, family, parms) {

	# Dropping a and b (parameters not used for validating) -- #
	parms <- parms[!(parms %in% c("a", "b"))]

	# Validating --------------------------------------------- #
	validation_family_parms <- validateFamilyParms(family, parms)
	if (validation_family_parms$is_valid) {
		family <- validation_family_parms$family_name
		return(family)
	}
}

validateFamilyParms <- function(family, parms, verbose=FALSE) {
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
		invgamma = list(
			family = c("invgamma"),
			parms  = c("shape", "rate")
		),
		invgauss = list(
			family = c("invgauss"),
			parms  = c("m", "s")
		),
		lognormal = list(
			family = c("lognormal", "lognormal"),
			parms  = c("meanlog", "sdlog")
		),
		nbinom = list(
			family = c("nbinom"),
			parms  = c("size", "prob")
		),
		normal = list(
			family = c("normal", "gaussian"),
			parms  = c("mean", "sd")
		),
		poisson = list(
			family = c("poisson"),
			parms  = c("lambda")
		)
	)
	matched <- list(family = FALSE, parameters = FALSE)
	for (fam in names(valid_fam_parm)) {
		if (any(family == valid_fam_parm[[fam]]$family)) {
			if(verbose) message("Matched family: ", family)
			matched$family <- TRUE
			family <- valid_fam_parm[[fam]]$family[1] # use standard family name
			parms_text <- paste(parms, collapse=", ")
			parms_expected <- valid_fam_parm[[fam]]$parms
			parms_expected_text <- paste(parms_expected, collapse=", ")
			if (all(sort(parms) == sort(parms_expected))) {
				if (verbose) message("Matched parameters: ", parms_text)
				matched$parameters <- TRUE
			} else {
				stop(
					"The {", parms_text, "} ",
					"parameter set does not match the ", family, " family. ",
					"Expected set of parameters: {", parms_expected_text, "}. ",
					"Please change the family to match the expected ",
					"parameters or use a different family."
				)
			}
		}
	}
	return(list(is_valid = all(unlist(matched)), family_name = family))
}
