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
    is_natural <- all(substr(names(parms), 1L, 3L) == "eta")

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
    max_name_length <- max(nchar(names(parms)), nchar(names(nat_parms)))
    max_value_length <- max(nchar(parms), nchar(nat_parms)) + 1L

    cat("Family:             ")
    cat(titleCase(family))
    cat("\nParameters:         ")
    printParm(parms, max_name_length, max_value_length)
    cat("\nNatural parameters: ")
    printParm(nat_parms, max_name_length, max_value_length)
  }
)

printParm <- function(parms, max_name_width = 7L, max_value_width = 10L) {
  for (p in names(parms)) {
    name_width <- nchar(p)
    name_value <- paste(append(p, rep(" ", max_name_width - name_width)), collapse = "")
    max_tot_width <- max_name_width + max_value_width
    parm_value <- formatC(parms[[p]], width = max_tot_width - nchar(name_value), flag = "-")
    cat(name_value, "=", parm_value)
  }
}

titleCase <- function(txt) {
  txt <- strsplit(txt, "")[[1]]
  return(paste(append(toupper(txt[[1]]), txt[-1]), collapse = ""))
}
