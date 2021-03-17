#' @title Generates an rtrunc-dispatchable class
#' @description Matches a list of arguments to an rtrunc method
#' @param ... parameter list passed to `rtrunc`
#' @return A character string.
#' @author Waldir Leoncio
#' @export
genRtruncClass <- function(n, family, parms) {
	validateFamilyParms(family, parms)
}

validateFamilyParms <- function(family, parms) {
	valid_fam_parm <- list(
		normal = list(
			family = c("gaussian", "normal"),
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
			message("Matched family: ", family)
			matched$family <- TRUE
			parms_text <- paste(parms, collapse=", ")
			parms_expected <- valid_fam_parm[[fam]]$parms
			parms_expected_text <- paste(parms_expected, collapse=", ")
			if (all(sort(parms) == sort(parms_expected))) {
				message("Matched parameters: ", parms_text)
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