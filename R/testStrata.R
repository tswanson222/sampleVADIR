#' Return correlations for demographics between population and sample
#'
#' Used to evaluate the representativeness of the sample with regard to the
#' population. Males and females evaluated separately.
#'
#' @param out Output of \code{sampleVADIR()}
#' @param data Original VADIR data
#' @param metric Function for measuring similarity between population and sample
#' @param zeros Should empty strata be included?
#'
#' @return Similarity values for males and females
#' @export
#'
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' testStrata(out, data)
#' }
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
  females <- metric(d1, p1)
  males <- metric(d2, p2)
  c(Females = females, Males = males)
}
