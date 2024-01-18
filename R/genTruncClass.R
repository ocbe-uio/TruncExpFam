#' @title Generates an rtrunc-dispatchable class
#' @description Matches a list of arguments to an rtrunc method
#' @param n sample size
#' @param family distribution family
#' @param parms list of parameters passed to rtrunc (through the `...` element)
#' @return A character string.
#' @author Waldir Leoncio
genrtruncClass <- function(n, family, parms) {

  # Dropping a and b (parameters not used for validating) -- #
  parms <- parms[!(parms %in% c("a", "b"))]

  # Validating --------------------------------------------- #
  validation_family_parms <- validateFamilyParms(family, parms)
  if (validation_family_parms$is_valid) {
    family <- validation_family_parms$family_name
    return(family)
  }
}
