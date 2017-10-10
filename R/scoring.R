
gam_mins <- function(x, y, title='', plot.it=TRUE) {
  fit <- gam(y ~ s(x, 4))
  gammed <- function(x) {
    predict(fit, newdata=data.frame(x))
  }

  dy <- diff(gammed(x))
  ddy <- diff(sign(dy))
  mins <- which(ddy > 0)
  min_index <- mins[which.min(y[mins])]

  if(length(min_index)==0) {
    min_index <- 1
  }

  n <- length(period_prices)
  y <- coredata(period_prices)
  x <- 1:n
  fit <- lm(y[min_index:length(y)] ~ x[min_index:length(y)])
  abline(fit, col='darkgreen')

  period_ssr_avg <- sum(rstudent(fit)^2) / n
  period_slope <- coef(fit)[2]

  if(plot.it) {
    plot(x, y, type='l', main=title)
    lines(x, gammed(x), type='l', col='red')
    grid()
    abline(v=x[min_index], col='darkblue')
    scan()
  }

  return(list(period_slope=period_slope, period_ssr_avg=period_ssr_avg, pdiff=y[length(y)] - y[1]))
}



scorecard <- function(symbols, years_ago=10, end_date=today()) {
  scores <- lapply(symbols, function(symbol) {
    if(length(symbols) > 1) {
      Sys.sleep(.75)
    }
    lnc <- getSymbols(symbol, auto.assign=FALSE)
    pp <- Ad(lnc[paste0(end_date-years(years_ago), '::', end_date)])
    rr <- dailyReturn(pp)
    endp <- last(pp)
    period_quarters <- unique(round_date(index(pp), 'quarter'))

    dd <- lapply(period_quarters, function(period_quarter) {
      ss <- paste0(period_quarter, '/')
      period_prices <- pp[ss]
      period_returns <- rr[ss]

      min_fit <- gam_mins(x, y, title=paste(symbol, period_quarter), plot.it=TRUE)

      startp <- first(period_prices)
      range_start_mean <- mean(pp[paste0(index(startp) - weeks(2), '::', index(startp) + weeks(2))])
      range_end_mean <- mean(pp[paste0(index(endp), '::', index(endp) + weeks(2))])

      data.table(symbol=symbol, quarter=period_quarter, slope=min_fit$period_slope, mean_ssr=min_fit$period_ssr_avg,
                 range_pdiff=range_end_mean-range_start_mean, pdiff=min_fit$pdiff, mean_ret=mean(period_returns))
    })
    rbindlist(dd)
  })
  rbindlist(scores)
}
