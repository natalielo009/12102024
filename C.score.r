C.score <- function (some.binomial.glm) {
   # based on Harrell (2015) and the Hmisc package
   observeds <- as.numeric(model.response(model.frame(some.binomial.glm)))-1
   mean.rank <- mean(rank(predict(some.binomial.glm))[observeds==1])
   C.numerator <- mean.rank-(sum(observeds)+1)/2
   C.denominator <- length(observeds)-sum(observeds)
   return(C.numerator / C.denominator)
}
