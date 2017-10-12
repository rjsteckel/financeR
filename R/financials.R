
#'
#' @export
#'
#' @import data.table
#'
corefinancials <- function(symbols, cache_dir=Sys.getenv('FIN_CACHE_DIR')) {
  base_url <- 'http://edgaronline.api.mashery.com/v2/corefinancials'
  endpoint <- 'qtr' #ttm (last 12 months)
  api_key <- Sys.getenv('EDGAR_APIKEY')
  if(is.null(api_key)) {
    stop('EDGAR_APIKEY environment variable must be set to use api functions')
  }

  start_quarter <- round_date(today(), 'quarter') - months(3) - days(1)
  end_quarter <- round_date(today(), 'quarter') - years(4) - days(1)

  fields <- c('commonstock', 'netincome', 'totalrevenue', 'grossprofit',
              'totalassets','totalliabilities', 'totalcurrentassets',
              'totalcurrentliabilities')

  rr <- lapply(symbols, function(symbol) {
    if(length(symbols) > 1) {
      Sys.sleep(.5)
    }

    cache_filename <- paste0(cache_dir, '/', symbol, '-', format(start_quarter, '%Y%m%d'), '_', format(end_quarter, '%Y%m%d'), '.fin')
    print(paste('Retrieving', symbol, 'financials'))
    tryCatch({
      if(file.exists(cache_filename)) {
        print('Loading cache file')
        fread(cache_filename)
      } else {
        params <- paste0('primarysymbols=', symbol, '&fields=periodenddate,', paste0(fields, collapse=','))
        #params <- paste0(params, '&fiscalperiod=2005q1~2017q4')
        url <- paste0(base_url, '/', endpoint, '?', params, '&appkey=', api_key)
        page <- httr::GET(url)
        page_content <- httr::content(page)

        total <- page_content$result$totalrows
        rows <- page_content$result$rows
        results <- lapply(rows, function(row) {
          fields <- sapply(row$values, function(value) { value$field })
          values <- sapply(row$values, function(value) { value$value })
          dt <- data.table(t(values))
          colnames(dt) <- fields
          #add quarter
          dt
        })
        results <- rbindlist(results, fill=TRUE)
        assertthat::are_equal(nrow(results), total)
        results$symbol <- symbol
        results[, periodenddate:=lubridate::mdy(periodenddate)]
        numcols <- colnames(results)[! colnames(results) %in% c('periodenddate', 'symbol')]
        results[, (numcols) := lapply(.SD, as.numeric), .SDcols=numcols]
        #results[, working_capital:=totalcurrentassets/totalcurrentliabilities]

        #write to cache
        write.csv(results, file=cache_filename, row.names=FALSE)

        results
      }
    })
  })

  rrd <- rbindlist(rr, fill=TRUE)
  return(rrd)
}
