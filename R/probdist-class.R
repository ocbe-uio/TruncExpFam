probdist <- setRefClass(
  Class  = "probdist",
  fields = list(
    "parms"      = "numeric",
    "family"     = "character",
    "nat_parms"  = "numeric"
  )
)

probdist$methods(
  initialize = function(..., family, is_natural = FALSE) {
    # Validation ---------------------------------------------------------------
    parms <<- c(...)
    family <<- useStandardFamilyName(family)
    validateFamilyParms(.self$family, names(parms))
    valDomFun <- get(paste0("validateDomain.trunc_", .self$family))
    valDomFun(NA, as.list(parms))
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
