#' plot weibull_plot
#' @param dataset
#' @return estimation and rsquared
#' @export
weibull_plot <- function(weib.df){
  # x, condition, status
  tmp.df <- weib.df[order(weib.df$x),]
  tmp.df$K <- nrow(tmp.df):1
  tmp.df$revK <- 1/tmp.df$K
  tmp.df$hazard <- tmp.df$revK * tmp.df$status
  tmp.df$cumhazard <- cumsum(tmp.df$hazard)
  tmp.df$log_cumhazard <- log(tmp.df$cumhazard)
  tmp.df$X <- log(tmp.df$x)
  tmp.df$Y <- 1 - exp(- tmp.df$cumhazard)
  tmp.df$Ft <- log10(1/(1-tmp.df$Y))
  
  result.lm <- lm(log10(Ft) ~ log10(x), data=tmp.df)
  (mhat <- result.lm$coefficients[2])
  B <- result.lm$coefficients[1]
  (etahat <- exp( - B / mhat))

    
  out <-  ggplot(tmp.df, aes(x=x, y=Ft)) + 
    geom_abline(intercept = B, slope = mhat) +
    geom_point() + 
    scale_x_log10() +
    scale_y_log10() +
    xlab("observed value") +
    ylab("F(t) %")
  print(out)
  result.lm <- lm(log_cumhazard ~ X, data=tmp.df)  
  (mhat <- result.lm$coefficients[2])
  B <- result.lm$coefficients[1]
  (etahat <- exp( - B / mhat))
  (rsquared <- summary(result.lm)$r.squared)
  
  result.df <- data.frame(mhat = mhat,
                          etahat = etahat,
                          rsquared = rsquared)
  return(result.df)
}
