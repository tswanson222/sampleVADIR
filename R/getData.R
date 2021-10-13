#' Data import function to accommodate multiple filetypes
#'
#' Allows for easy data importation. Automatically detects filetype and applies
#' appropriate function for importing.
#'
#' @param filename Character string specifying the path to desired datafile
#' @param filetype Character string indicating filetype. Useful if no file
#'   extension is provided in \code{filename}. If file extension is provided in
#'   \code{filename} (recommended), then this argument is ignored. Accommodates
#'   \code{"csv", "rds", "xlsx", "rdata", "sav", "txt"}
#' @param fixDates Logical. Determines whether to adjust date format.
#' @param ... Additional arguments
#'
#' @return Imported datafile
#' @export
#'
#' @importFrom utils read.csv read.table
getData <- function(filename, filetype = 'csv', fixDates = FALSE, ...){
  files <- c('csv', 'rds', 'xlsx', 'rdata', 'sav', 'txt')
  if(any(endsWith(tolower(filename), files))){
    filetype <- files[which(endsWith(tolower(filename), files))]
  } else {
    filetype <- match.arg(tolower(gsub('^[.]', '', filetype)), files)
  }
  if(startsWith(filetype, 'rd')){filetype <- ifelse(filetype == 'rds', 'RDS', 'RData')}
  FUN <- switch(filetype, csv = read.csv, RDS = readRDS, xlsx = rio::import,
                sav = function(x) as.data.frame(haven::read_sav(x)),
                RData = load, txt = read.table)
  file <- ifelse(endsWith(filename, filetype), filename,
                 paste0(filename, '.', filetype))
  out <- suppressWarnings(tryCatch({
    FUN(file, ...)}, error = function(e){
      message(paste0(filename, ' not loaded'))}))
  if('OMB_ETHNC_NAT_ORIG_CD' %in% colnames(out)){
    out <- out[which(out$OMB_ETHNC_NAT_ORIG_CD != 'Z'), ]
    #out <- subset(out, OMB_ETHNC_NAT_ORIG_CD != 'Z')
  }
  if('RANK_CD' %in% colnames(out)){
    out <- out[which(!out$RANK_CD %in% c('ROTC', '')), ]
    #out <- subset(out, !RANK_CD %in% c('ROTC', ''))
  }
  if('POST_911_DPLY_IND_CD' %in% colnames(out)){
    out$POST_911_DPLY_IND_CD <- trimws(out$POST_911_DPLY_IND_CD)
  }
  if(isTRUE(fixDates)){
    months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
    date <- function(x){as.Date(x, format = '%d-%m-%y')}
    for(i in 1:12){
      out$PN_BRTH_DT <- gsub(months[i], i, out$PN_BRTH_DT)
      out$PNL_TERM_DT <- gsub(months[i], i, out$PNL_TERM_DT)
      out$PNL_BGN_DT <- gsub(months[i], i, out$PNL_BGN_DT)
    }
    out$PN_BRTH_DT <- date(out$PN_BRTH_DT)
    out$PNL_TERM_DT <- date(out$PNL_TERM_DT)
    out$PNL_BGN_DT <- date(out$PNL_BGN_DT)
  }
  return(out)
}
