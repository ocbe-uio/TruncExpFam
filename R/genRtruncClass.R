#' @title Generates an rtrunc-dispatchable class
#' @description Matches a list of arguments to an rtrunc method
#' @param ... parameter list passed to `rtrunc`
#' @return A character string.
#' @author Waldir Leoncio
genRtruncClass <- function(n, family, parms) {

	# Dropping a and b (parameters not used for validating) -- #
	parms <- parms[!(parms %in% c("a", "b"))]

	# Validating --------------------------------------------- #
	is_family_parms_valid <- validateFamilyParms(family, parms)
	if (is_family_parms_valid) {
		return(family)
	}
}

validateFamilyParms <- function(family, parms, verbose=FALSE) {
	valid_fam_parm <- list(
		normal = list(
			family = c("gaussian", "normal"),
			parms  = c("mean", "sd")
		),
		poisson = list(
			family = c("poisson"),
			parms  = c("lambda")
		),
		binomial = list(
			family = c("binomial"),
			parms  = c("size", "prob")
		),
		gamma = list(
			family = c("gamma"),
			parms  = c("shape", "rate")
		),
		lognormal = list(
			family = c("log-normal", "lognormal"),
			parms  = c("meanlog", "sdlog")
		),
		contbernoulli = list(
			family = c("contbernoulli"),
			parms  = c("lambda")
		),
		chisq = list(
			family = c("chisq"),
			parms  = c("df")
		)
	)
	matched <- list(family = FALSE, parameters = FALSE)
	for (fam in names(valid_fam_parm)) {
		if (any(family == valid_fam_parm[[fam]]$family)) {
			if(verbose) message("Matched family: ", family)
			matched$family <- TRUE
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
					"Please change the family to match the expected ", "parameters or use a different family."
				)
			}
		}
	}
	return(all(unlist(matched)))
}
