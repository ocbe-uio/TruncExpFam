probdist <- setRefClass(
  Class  = "probdist",
  fields = list(
    "parms"      = "numeric",
    "family"     = "character",
    "nat_parms"  = "numeric"
  )
)

probdist$methods(
  initialize = function(..., family) {
    # Retrieving parameters and detecting if they are natural ------------------
    parms <<- c(...)
    is_natural <- all(substr(names(parms), 1, 3) == "eta")

    # Validation ---------------------------------------------------------------
    family <<- useStandardFamilyName(family)
    if (!is_natural) {
      validateFamilyParms(.self$family, names(parms))
      valDomFun <- get(paste0("validateDomain.trunc_", .self$family))
      valDomFun(NA, as.list(parms))
    }

    # Converting parameters ----------------------------------------------------
    if (is_natural) {
      nat_parms <<- parms
      convFun <- get(paste0("natural2parameters.trunc_", .self$family))
      parms <<- convFun(nat_parms)
    } else {
      convFun <- get(paste0("parameters2natural.trunc_", .self$family))
      nat_parms <<- unclass(convFun(parms))
    }
  },
  show = function() {
    cat("Family:             ")
    cat(family)
    cat("\nParameters:         ")
    cat(parms)
    cat("\nNatural parameters: ")
    cat(nat_parms)
  }
)
