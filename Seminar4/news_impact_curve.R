###### News Impact Curve Plot ######

# Plot the News Impact Curve for normal GARCH, tGARCH, apARCH, tapARCH

models <- c(GARCH, tGARCH, apARCH, tapARCH)

for (i in 1:4) {
  x <- models[[i]]
  vmodel  <- x@model$modeldesc$vmodel
  ni <- newsimpact(z = NULL, x)
  ni.y <- ni$zy
  ni.x <- ni$zx
  xf <- ni$xexpr
  yf  <- ni$yexpr
  if (i == 1) {
    png(filename = "news_impact.png")
    plot( ni.x, ni.y, ylab = yf, xlab = xf, type = "l", lwd = 2, col = i, main = "News Impact Curve",
          ylim = c(0.0005,0.004))
    grid()
  } else {
    lines(ni.x, ni.y, col = i)
  }
}

legend("bottomleft", legend = c("GARCH", "tGARCH", "apARCH", "tapARCH"), col = 1:4, lty = 1)
dev.off()
