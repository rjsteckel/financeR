
#'
#' @export
#' @import tidyquant
get_prices <- function(symbols, from='2000-01-01') {
  prices <- tidyquant::tq_get(symbols, get='stock.prices', from=from, complete_cases=TRUE)
  if(length(symbols)==1) {
    #add symbol column
    prices <- prices %>% dplyr::mutate(symbol=symbols)
  }

  return(prices)
}

#'
#' @export
#' @import tidyquant
#' @importFrom magrittr "%>%"
#'
get_returns <- function(prices) {
  returns <- prices %>%
    dplyr::group_by(symbol) %>%
    tidyquant::tq_mutate(select=adjusted, mutate_fun=periodReturn, period='daily', type='log')
  return(returns)
}



calculateGrowth <- function(a, b) {
  if(is.na(a) || is.infinite(a) || is.na(b) || is.infinite(b) || abs(a) < .001) {
    return(NA)
  }
  if(sign(a)==-1 && sign(b)==-1)
    return(-abs(b)/abs(a))
  if(sign(a)==-1 && sign(b)==+1)
    return(NA)
  if(sign(a)==+1 && sign(b)==-1)
    return(NA)
  return(round(abs(b)/abs(a), 2)*sign(b))
}


#'
#'
#' @export
#'
plot_growth_financials <- function(financials, varnames) {
  if(length(varnames) > 6) {
    stop('Max of 6 variables can be plotted')
  }
  n <- length(varnames)
  op <- par(mfrow=c(ceiling(n/2), 2))
  for(varname in varnames) {
    ss <- lapply(unique(financials$symbol), function(s) {
      series <- with(financials[symbol==s], xts(get(varname), order.by=periodenddate))
      grdata <- c(1, sapply(2:length(series), function(i) calculateGrowth(coredata(series)[i-1], coredata(series)[i])))
      grseries <- xts(grdata, order.by=index(series))
      colnames(grseries) <- s
      grseries
    })

    for(i in 1:length(ss)) {
      index(ss[[i]]) <- lubridate::round_date(index(ss[[i]]), 'quarter')
    }

    mseries <- do.call(merge, ss)
    p <- plot.xts(mseries, main=varname, type='b')
    addLegend("topright", on=1, legend.names=colnames(mseries), lty=1)
    print(p)
  }
  par(op)
}

