#' Return correlations for demographics between population and sample
#'
#' Used to evaluate the representativeness of the sample with regard to the
#' population. Males and females evaluated separately.
#'
#' @param out Output of \code{\link{sampleVADIR}}
#' @param data Original VADIR data
#' @param metric Function for measuring similarity between population and sample
#' @param zeros Should empty strata be included?
#'
#' @return Similarity values for males and females
#' @export
#'
#' @importFrom stats cor
testStrata <- function(out, data = NULL, metric = cor, zeros = FALSE){
  x <- switch(2 - is.null(data), out$data, data)
  if(dim(table(x$RANK_CD)) > 7){x <- convertRank(x)}
  x1 <- out$Females
  x2 <- out$Males
  x$cats <- interaction(x[, attr(x1, 'stratvars')])
  d1 <- prop.table(table(x[which(x$PN_SEX_CD == 'F'), 'cats']))
  d2 <- prop.table(table(x[which(x$PN_SEX_CD == 'M'), 'cats']))
  p1 <- prop.table(table(x1$cats))
  p2 <- prop.table(table(x2$cats))
  if(zeros){
    pp1 <- d1
    pp1[match(names(p1), names(pp1))] <- p1
    pp1[-match(names(p1), names(pp1))] <- 0
    pp2 <- d2
    pp2[match(names(p2), names(pp2))] <- p2
    pp2[-match(names(p2), names(pp2))] <- 0
    p1 <- pp1
    p2 <- pp2
  } else {
    d1 <- d1[names(d1) %in% names(p1)]
    d2 <- d2[names(d2) %in% names(p2)]
    d1 <- d1[names(p1)]
    d2 <- d2[names(p2)]
  }
  out <- c(Females = metric(d1, p1), Males = metric(d2, p2))
  return(out)
}

# convertRank: used in the testStrata function
convertRank <- function(data, ranks = 'rankDat.RDS', payRanks = 4){
  colnames(data) <- toupper(colnames(data))
  data <- fixTypos(data = data, old = c('CW02', 'CW0-2', 'PV1'),
                   new = c('CWO2', 'CWO2', 'PVT'), var = 'RANK_CD')
  if(is.character(rankDat)){
    if(identical(rankDat, 'rankDat')){
      rankDat <- sampleVADIR::rankDat
    } else {
      rankDat <- tryCatch({readRDS(rankDat)}, error = function(e){TRUE})
    }
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
