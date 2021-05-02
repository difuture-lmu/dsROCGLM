#'
#' @title Printer for ROC-GLM
#' @description This function provides a printer for the ROC-GLM object returned from `dsROCGLM`.
#' @param roc_glm [list()] List containing the ROC-GLM parameter returned from `dsROCGLM`.
#' @author Daniel S.
#' @export
print.ROC.GLM = function (roc_glm)
{
  cat("\nROC-GLM after Pepe:\n\n\tBinormal form: pnorm(", round(roc_glm$parameter[1], 2), " + ",
    round(roc_glm$parameter[2], 2), "*qnorm(t))\n\n\tAUC and ", 1 - roc_glm$alpha, " CI: [", round(roc_glm$ci[1], 2), "----",
    round(roc_glm$auc, 2), "----", round(roc_glm$ci[2], 2), "]\n\n", sep = "")
}
