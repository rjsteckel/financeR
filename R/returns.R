
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
get_returns <- function(prices) {
  returns <- prices %>%
    dplyr::group_by(symbol) %>%
    tidyquant::tq_mutate(select=adjusted, mutate_fun=periodReturn, period='daily', type='log')
  return(returns)
}
