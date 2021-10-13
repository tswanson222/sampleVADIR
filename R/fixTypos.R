#' Fix typos in VADIR dataset
#'
#' If there are known typos, the correct values of those incorrect responses can
#' be provided and fixed across the dataset.
#'
#' @param data VADIR dataset
#' @param old Character vector containing typos
#' @param new Character vector in the same order as \code{old}, containing
#'   corresponding values to fix typos to.
#' @param var Variable name for which typos should be corrected
#'
#' @return VADIR dataset with typos corrected
#' @export
#'
#' @examples
#' data <- fixTypos(data = VADIR_fake, old = c('CW02', 'CW0-2', 'PV1'),
#'                  new = c('CWO2', 'CWO2', 'PVT'), var = 'RANK_CD')
fixTypos <- function(data, old, new = NULL, var = 'RANK_CD'){
  if(is.null(new) & is(old, 'list') & length(old) == 2){
    if(all(tolower(names(old)) %in% c('old', 'new'))){
      new <- old$new; old <- old$old
    } else {
      new <- old[[2]]; old <- old[[1]]
    }
  }
  stopifnot(length(old) == length(new))
  old <- toupper(old)
  new <- toupper(new)
  for(i in seq_along(old)){
    ierr <- which(data[, var] == old[i])
    if(length(ierr) > 0){data[ierr, var] <- new[i]}
  }
  return(data)
}
