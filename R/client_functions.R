

plotROCGLM = function (roc_glm)
{
  params = roc_glm$parameter

  x = seq(0, 1, 0.002)
  y = pnorm(params[1] + params[2]*qnorm(x))

  plot(1 - x, 1 - y, type = "l", xlab = "FPR", ylab = "TPR")
  polygon(x = c(1 - x, rev(1 - x)), y = c(1 - y, rep(0, length(y))),
    col = rgb(54,100, 139, 100, maxColorValue = 255), border = NA)
  lines(1 - x, 1 - y, lwd = 2)
}
