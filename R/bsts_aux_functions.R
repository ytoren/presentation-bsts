library(bsts)

bsts_plot_components <- function(m) {
  burn <- SuggestBurn(0.1, m)
  
  data <- cbind.data.frame(
    sapply(
      dimnames(m$state.contributions)$component,
      function (y) {
        colMeans(m$state.contributions[-(1:burn),y,])
      }
    ),
    Date = as.Date(m$timestamp.info$timestamps)
  ) %>%
    gather(key = "Component", value = "Value", -Date)
  
  p <- data %>%
    ggplot(aes(x=Date, y=Value)) %>%
    + geom_line() %>%
    + theme_bw() %>%
    + theme(legend.title = element_blank()) %>%
    + ylab("") %>%
    + xlab("") %>%
    + facet_grid(Component ~ ., scales="free") %>%
    + guides(colour=FALSE) %>%
    + theme(axis.text.x=element_text(angle = -45, hjust = 0))
  
  return(p)
}

## BSTS actual vs predicted
bsts_plot_forecast <- function(m, y, days = 30) {
  ### Get a suggested number of burn-ins
  burn <- SuggestBurn(0.1, m)
  
  ### Predict
  p <- predict.bsts(
    m,
    horizon = days,
    burn = burn,
    quantiles = c(.025, .975),
    newdata = m$predictors
  )
  
  ### Actual versus predicted
  data <- data.frame(
    # fitted values and predictions
    y - colMeans(m$one.step.prediction.errors[-(1:burn),]),
    #as.numeric(p$mean),
    # actual data and dates
    as.numeric(y),
    as.Date(rownames(m$predictors))
  )
  names(data) <- c("Fitted", "Actual", "Date")
  
  ### MAPE (mean absolute percentage error)
  MAPE <- data %>%
    filter(Date > max(Date) - days) %>%
    summarise(
      MAPE=mean(abs(Actual-Fitted)/Actual)
    )
  
  ### 95% forecast credible interval
  posterior.interval <- cbind.data.frame(
    as.numeric(p$interval[1,][0:days]),
    as.numeric(p$interval[2,][0:days]),
    data %>% filter(Date > max(Date) - days) %>% pull(Date)
  )
  names(posterior.interval) <- c("LL", "UL", "Date")
  
  ### Join intervals to the forecast
  data <- data %>% left_join(posterior.interval, by="Date")
  
  ### Plot actual versus predicted with credible intervals for the holdout period
  p <- data %>% 
    ggplot(aes(x=Date)) +
    geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
    geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    ylab("") +
    xlab("") +
    #geom_vline(xintercept=as.numeric(as.Date("1959-12-01")), linetype=2) +
    geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
    ggtitle(paste0("Actual vs. Forecast (MAPE = ", round(100*MAPE,2), "%)")) +
    theme(axis.text.x=element_text(angle = -45, hjust = 0))
  
  return(p)
}