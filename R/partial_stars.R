partial_stars <- function(pcor, ci = FALSE, digits = 2){
  pcor$stars <- pcor$r
  if( class(pcor) == c("psych", "corr.p")) {
    ct = 1
    # c2 = 0
    for (j in 1:ncol(pcor$stars)) {
      var <- rownames(pcor$stars)[j]
      coln <- c()
      for (i in 1:ncol(pcor$stars)){
        if (i <= j) {
          coln[i] <- NA
        } else {
          star <- ifelse(pcor$p[i,j] >= .05, "", ifelse(pcor$p[i,j] <= .01, "**", "*"))
          if (ci == T) {
            conf = paste0("\n[", round(pcor$ci$lower[ct], digits), ", ", round(pcor$ci$upper[ct], digits),"] ")
          } else {conf = ""}
          coln[i] <- paste0(round(as.numeric(pcor$stars[i,j]), digits), star, conf)
          ct = ct + 1
        }
      }
      pcor$stars[,j] <- coln
    }
  }
}
