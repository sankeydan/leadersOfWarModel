rnormWithMinMax =  function(n, m, s, lwr, upr) {
  samp <- rnorm(n, m, s)
  samp[samp < lwr] <- lwr
  samp[samp > upr] <- upr
  samp
}
