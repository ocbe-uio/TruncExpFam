rescaledDensities <- function(y, a, b, densFunction, probFunction, ...) {
  dens <- ifelse(
    test = (y < a) | (y > b),
    yes  = 0,
    no   = densFunction(y, ...)
  )
  F.a <- probFunction(a, ...) # TODO: check. a can be 0 and still have F.a > 0
  F.b <- probFunction(b, ...)
  dens <- dens / (F.b - F.a)
  attributes(dens) <- attributes(y)
  return(dens)
}
