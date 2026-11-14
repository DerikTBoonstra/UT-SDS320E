# ggplot ------------------------------------------------------------------
# Run the code below to save the residual diagnostic function into your R 
# environment if you want to use ggplot
residual_diagnostics <- function(model){
  pkgs <- c("qqplotr", "patchwork", "forecast")
  installed <- pkgs %in% installed.packages()[, "Package"]
  if (any(!installed)) {
    install.packages(pkgs[!installed])
  }
  library("qqplotr")
  library("patchwork")
  
  residuals <- model$residuals
  
  p1 <- ggplot(aes(sample = residuals), data = NULL)+
    stat_qq_band(color = "lightgrey")+
    stat_qq_line(color = "red") +
    stat_qq_point()+
    labs(x = "Theoretical Residuals", y = "Sample Residuals")+
    theme_minimal()
  
  p2 <- ggplot(aes(model$fitted.values, residuals), data = NULL)+
    geom_point()+
    geom_hline(yintercept = 0, color = "red")+
    labs(x = "Fitted Values", y = "Residual")+
    theme_minimal()
  
  p3 <- forecast::ggAcf(residuals)+
    theme_minimal()
  
  ## Return
  p1 / (p2 + p3)
}

# Base R ------------------------------------------------------------------
# Run the code below to save the residual diagnostic function into your R 
# environment if you want to use base R graphics
residual_diagnostics <- function(model) {
  
  if (!requireNamespace("car", quietly = TRUE)) {
    install.packages("car")
  }
  
  residuals <- model$residuals
  
  layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE),
         heights = c(1.5, 1))
  par(mar = c(4, 4, 1, 1))
  
  car::qqPlot(residuals,
              col.lines = "red",
              xlab = "Theoretical Residuals",
              ylab = "Sample Residuals")
  
  plot(model$fitted.values, residuals,
       pch = 19,
       xlab = "Fitted Values",
       ylab = "Residual")
  abline(h = 0, col = "red")
  
  plot(acf(residuals, plot = FALSE)[1:20])
}