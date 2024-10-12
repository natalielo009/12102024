explorer.num <- function (numeric.vector, bcn.results=FALSE) {
   # (C) 2020: Stefan Th. Gries <http://www.stgries.info>
   library(car)
   has.missing.data <- sum(is.na(numeric.vector))>0
   has.zeros <- sum(numeric.vector==0, na.rm=TRUE)>0
   has.negatives <- sum(sign(numeric.vector)==-1, na.rm=TRUE)>0
   layout(matrix(c(1,2,3,5,4,5), byrow=TRUE, ncol=2))
   qwe <- hist(
      numeric.vector,
      main=paste("Raw values of", deparse(substitute(numeric.vector))))
   hist(numeric.vector,
        breaks=length(qwe$breaks)*2,
        main=paste("Raw values of", deparse(substitute(numeric.vector))))

   plot(sort(numeric.vector), type="h",
        main=paste("Raw values of", deparse(substitute(numeric.vector)))); grid()
   plot(ecdf(numeric.vector), verticals=TRUE,
        main=paste("Raw values of", deparse(substitute(numeric.vector)))); grid()
   qwe <- car::symbox(numeric.vector, powers=seq(-3, 3, 0.25), trans=bcnPower,
                      main=paste("Symbox (bcn) for", deparse(substitute(numeric.vector))),
                      col=rep(c("lightgrey", "green", "lightgrey"), c(16, 1, 8)))

      asd <- tapply(qwe$out, qwe$group, c)
      collector <- vector(mode="list", length=length(asd)); names(collector) <- names(asd)
      for (i in seq(asd)) { collector[[i]] <- table(asd[[i]]>qwe$stats[3,as.numeric(names(asd)[i])]) }
      for (i in seq(collector)) {
         if ("FALSE" %in% names(collector[[i]])) {
            text(as.numeric(names(collector)[i]),
                 par("usr")[[3]], pos=3,
                 collector[[i]]["FALSE"])
         }
         if ("TRUE" %in% names(collector[[i]])) {
            text(as.numeric(names(collector)[i]),
                 par("usr")[[4]], pos=1,
                 collector[[i]]["TRUE"])
         }
      }

      # text(as.numeric(names(tapply(qwe$out, qwe$group, length))),
      #      par("usr")[[3]], pos=3,
      #      labels=tapply(qwe$out, qwe$group, length))

   diffs <- sort(unique(diff(sort(numeric.vector))))
   zero.diffs <- which(sapply(diffs, all.equal, 0)==TRUE)
      diffs[zero.diffs] <- 0
   smallest.meaningful.diff <- ifelse(any(diffs>0), min(diffs[diffs>0]), 0)

   if (bcn.results) {
      # apply bcn
      bcn.parameters <- powerTransform(numeric.vector ~ 1, family="bcnPower")
      bcn.transformed <- bcnPower(
         numeric.vector,
         lambda=bcn.parameters[[1]],
         gamma=bcn.parameters[[2]])
      # then most plots as before
      layout(matrix(c(1,2,3,4), byrow=TRUE, ncol=2))
      qwe <- hist(
         bcn.transformed,
         main=paste("BCn-transformed values of", deparse(substitute(numeric.vector))))
      hist(
         bcn.transformed,
         breaks=length(qwe$breaks)*2,
         main=paste("BCn-transformed values of", deparse(substitute(numeric.vector))))
      plot(sort(bcn.transformed), type="h",
           main=paste("BCn-transformed values of", deparse(substitute(numeric.vector)))); grid()
      plot(ecdf(bcn.transformed), verticals=TRUE,
           main=paste("BCn-transformed values of", deparse(substitute(numeric.vector)))); grid()
   }
   par(mfrow=c(1, 1))
   output <- list(
      "Special data points"=c(
         "Missing data"=ifelse(has.missing.data,
                               paste0(has.missing.data, ":", sum(is.na(numeric.vector))),
                               has.missing.data),
         "Zeros"=ifelse(has.zeros,
                        paste0(has.zeros, ":", sum(numeric.vector==-0, na.rm=TRUE)),
                        has.zeros),
         "Negatives"=ifelse(has.negatives,
                        paste0(has.negatives, ":", sum(sign(numeric.vector)==-1, na.rm=TRUE)),
                        has.negatives)),
      "Summary"=summary(numeric.vector),
      "Types and tokens"=c(
         "Types"=length(unique(numeric.vector)),
         "Tokens"=length(numeric.vector)),
      "Freqs of freqs"=table(table(numeric.vector, useNA="always")),
      "Smallest 'meaningful' difference"=smallest.meaningful.diff)
   if (bcn.results) {
      output[["Box-Cox n parameters"]] <- c(
         "lambda"=bcn.parameters[[1]],
         "gamma"=bcn.parameters[[2]])
   }
   return(output)
}
