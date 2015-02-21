#' plot weibull_plot
#' @param x is data, status is failure, condtion(not yet)
#' @return estimation and rsquared
#' @export
weibull_plot <- function(x, status, condition=NULL){
  # x, condition, status
  weib.df <- data.frame(x = x,
                        condition = condition,
                        status = status)
  tmp.df <- weib.df[order(weib.df$x),]
  tmp.df$K <- nrow(tmp.df):1
  tmp.df$revK <- 1/tmp.df$K
  tmp.df$hazard <- tmp.df$revK * tmp.df$status
  tmp.df$cumhazard <- cumsum(tmp.df$hazard)
  tmp.df$log_cumhazard <- log(tmp.df$cumhazard)
  tmp.df$X <- log(tmp.df$x)
  
  tmp.df$Ft <- 1 - exp(- tmp.df$cumhazard)
  tmp.df$transFt <- log10(log10(1/(1-tmp.df$Ft)))
  tmp.df <- tmp.df[tmp.df$status==1,]
  
  result.lm <- lm(transFt ~ log10(x), data=tmp.df)
  mhat <- result.lm$coefficients[2]
  B <- result.lm$coefficients[1]

  probs <- c(10^(-10:0), 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99, 99.9)/100
  out <-  ggplot(tmp.df, aes(x=x, y=transFt)) +
    geom_point() + 
    scale_y_continuous(breaks = log10(log10(1/(1-probs))), labels = probs*100) +
    scale_x_log10() +
    geom_abline(intercept = B, slope = mhat) +
    xlab("observed value") +
    ylab("F(t) %")
  print(out)


  # estimation
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
