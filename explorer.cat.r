explorer.cat <- function (categorical.variable, plotty=FALSE, roundto=3) {
   # (C) 2020: Stefan Th. Gries <http://www.stgries.info>
   has.missing.data <- sum(is.na(categorical.variable))>0
   temp <- table(categorical.variable, dnn=NULL, useNA="ifany")

   if (plotty) { plot(
      temp, type="h", col=ifelse(!is.na(names(temp)), "black", "red"),
      main=paste("Frequencies of", deparse(substitute(categorical.variable)))); grid()
   }

   output <- list(
      "Missing data"=ifelse(has.missing.data,
                            paste0(has.missing.data, ":", sum(is.na(categorical.variable))),
                            has.missing.data),
      "Frequencies"=temp,
      "Percentages"=round(prop.table(temp), roundto),
      "Freqs of freqs"=table(temp, dnn=NULL))

   if (has.missing.data) {
      output[["Types and tokens"]] <- c(
         "Types (incl. NAs)"=length(temp),
         "Tokens (incl. NAs)"=length(categorical.variable),
         "types (excl. NAs)"=length(temp)-1,
         "Tokens (excl. NAs)"=sum(!is.na(categorical.variable)))
   } else {
      output[["Types and tokens"]] <- c(
         "types (no NAs)"=length(temp),
         "Tokens (no NAs)"=sum(!is.na(categorical.variable)))
   }
   return(output)
}
