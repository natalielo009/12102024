R2 <- function (some.model) {
   # tested model classes: lm, glm, MASS::polr, nnet::multinom
   # does not work for family=quasibinomial; see
   # <https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf>
   null.model <- update(some.model, ~ 1)
   n <- nrow(model.frame(some.model))
   R2.numerator <- (1 - exp((-2*logLik(some.model) - -2*logLik(null.model))/n))
   R2.denominator <- (1 - exp(2*logLik(null.model)/n))
   return(as.numeric(R2.numerator / R2.denominator))
}
