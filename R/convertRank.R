#' Convert rank values into pay grades
#'
#' Used in \code{sampleVADIR()} function. Probably not for user.
#'
#' @param data VADIR dataset
#' @param ranks Rank dataset, or path to rank dataset
#' @param payRanks Number of pay grades to use
#'
#' @return VADIR dataset with ranks converted to pay grade categories
#' @export
#'
#' @examples
#' \dontrun{
#' out <- convertRank(data, ranks = 'rankDat.RDS', payRanks = 4)
#' }
convertRank <- function(data, ranks = 'rankDat.RDS', payRanks = 4){
  colnames(data) <- toupper(colnames(data))
  data <- fixTypos(data = data, old = c('CW02', 'CW0-2', 'PV1'),
                   new = c('CWO2', 'CWO2', 'PVT'), var = 'RANK_CD')
  if(is.character(rankDat)){
    rankDat <- tryCatch({readRDS(rankDat)}, error = function(e){TRUE})
    if(isTRUE(rankDat)){stop('rankDat improperly specified. Must provide data.frame or filepath.')}
  } else if(any(sapply(c('data.frame', 'matrix'), function(i) is(rankDat, i)))){
    if(is.null(colnames(rankDat))){stop('Need named variables for rankDat')}
  } else stop('Must provide either data.frame, matrix, or filepath for rankDat')
  if(!payRanks %in% c(4, 5, 7)){stop('payRanks must be set to either 4, 5, or 7')}
  payRanks <- attr(rankDat, paste0('payCat', payRanks))
  if(length(setdiff(unique(data$RANK_CD), unique(rankDat$Initials))) > 0){
    stop('Check "RANK_CD" of data and compare with "rankDat". Perhaps there is a typo to correct.')
  } else {
    rankDat <- rankDat[rankDat$Initials %in% data$RANK_CD, ]
    rankDat <- rankDat[order(rankDat$Initials), ]
    rankDat <- rankDat[!duplicated(rankDat$Initials), ]
    data$RANK_CD <- factor(data$RANK_CD, levels = rankDat$Initials)
    if(all(levels(data$RANK_CD) == rankDat$Initials)){
      levels(data$RANK_CD) <- rankDat[, paste0('PayCat', length(payRanks))]
      data$RANK_CD <- as.character(data$RANK_CD)
    } else stop('There may be a typo or error in the "RANK_CD" variable')
  }
  return(data)
}
