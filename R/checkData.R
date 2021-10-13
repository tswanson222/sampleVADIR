#' Compare old and new versions of VADIR to find repeats
#'
#' Can be used to identify whether a new version of VADIR contains any old
#' responses. Can also automatically remove repeated responses.
#'
#' @param old Past version of VADIR
#' @param new New version of VADIR
#' @param fix Logical. Determines whether to automatically remove repeated
#'   responses.
#' @param dates Logical. Determines whether to include date variables when
#'   comparing datasets. Recommended to keep \code{FALSE}.
#'
#' @return Returns a message that no repeated responses exist if there are none.
#'   Otherwise, returns either a warning that repeated responses exist, or
#'   returns the new VADIR dataset without repeated responses if \code{fix =
#'   TRUE}.
#' @export
checkData <- function(old, new, fix = FALSE, dates = FALSE){
  vars <- intersect(colnames(old), colnames(new))
  if(!isTRUE(dates)){vars <- setdiff(vars, c('PN_BRTH_DT', 'PNL_TERM_DT', 'PNL_BGN_DT'))}
  new1 <- new[, vars]
  old1 <- old[, vars]
  new2 <- apply(new1, 1, paste0, collapse = '_')
  old2 <- apply(old1, 1, paste0, collapse = '_')
  if(any(old2 %in% new2)){
    if(isTRUE(fix)){
      return(new[-which(new2 %in% old2), ])
    } else warning('Old participants found in new dataset')
  } else {
    message('No overlap across the datasets!')
  }
}
