#' Draw stratified samples from VADIR database
#'
#' Core function used to pull a stratified sample from VADIR based on a variety
#' of parameters.
#'
#' Performs stratification separately for males and females, where males and
#' females are sampled at a 1:1 ratio, regardless of population ratio.
#'
#' With a large dataset (which is typical for VADIR), setting any of the
#' date-related variables to \code{TRUE} can drastically increase computation
#' time. The relevant arguments include: \code{ageDischarge, ageEnlist, ageNow,
#' yearsServed}.
#'
#' @param data VADIR dataset
#' @param n Total desired sample size
#' @param vars Character vector indicating which variables to use in
#'   stratification
#' @param rankDat Dataset linking ranks to pay grade, or character string
#'   indicating where to pull that dataset from. Recommended to leave as
#'   \code{"rankDat"} in order to use package-supplied dataset.
#' @param payRanks Number of pay grades to use when converting rank variable.
#'   Only options are either 4 or 7.
#' @param post911 Logical. Determines whether to only consider individuals
#'   deployed after 9/11/2001
#' @param dischargedAfter Character string indicating what date to restrict
#'   sampling to based on discharge date. Can set to \code{FALSE} if this is to
#'   be ignored. Can also set to \code{'past-year'} in order to only sample
#'   people who were discharged within the past year (given the current date).
#' @param until Upper limit to when service was started. \code{NULL} means there
#'   is no upper limit
#' @param ageDischarge Logical. Determines whether to use age at discharge as a
#'   stratum.
#' @param ageEnlist Logical. Determines whether to use age at enlist as a
#'   stratum.
#' @param ageNow Logical. Determines whether to use current age as a stratum.
#' @param yearsServed Logical. Determines whether to use total years served as a
#'   stratum.
#' @param dateformat Character string indicating the expected date format.
#'   Should be automatically detected.
#' @param params Optional list of parameters to override defaults in function.
#'   Creates an easy way to interface with the function if performing the
#'   stratification multiple times. Allows the user to avoid writing the same
#'   arguments multiple times.
#' @param formats Should be \code{"default"}
#' @param typos List containing typos to be fixed, as well as what they should
#'   be changed to. Leave at \code{list()} to ignore. Typos can also be fixed
#'   prior to stratification by using the \code{\link{fixTypos}} function.
#' @param rmDeviates Logical. Determines whether rows with unexpected response
#'   values are removed. If \code{FALSE}, and deviate response values are
#'   detected, the function will stop.
#' @param timeCats Logical or numeric. Determines whether the time-related
#'   variables should be treated as categorical variables. If \code{TRUE}, this
#'   defaults to 4.
#' @param saveData Logical. Determines whether to save the full dataset in the
#'   output. Specifically, returns the full dataset of candidates (i.e., some
#'   people may be removed from consideration due to errors or unexpected
#'   responses).
#' @param onlyIDs Logical. Determines whether to only return ID values for
#'   selected individuals rather than a full dataset.
#' @param oversample Logical. Determines whether to oversample or undersample
#'   based on limitations due to available proportions of strata in subsample.
#' @param exclude Logical. Determines whether to exclude people missing a zip
#'   code, as well as people with \code{"NTC"} as their zip code value.
#' @param seed Numeric value indicating the seed to set for the stratification
#'   procedure. Allows for reproducible results.
#'
#' @return A list containing the males and females who were sampled from VADIR
#' @export
#'
#' @importFrom methods is
#' @importFrom stats quantile setNames
#'
#' @examples
#' \donttest{
#' params <- list(
#'   n = 7000,
#'   vars = c('PN_Sex_CD', 'PN_BRTH_DT', 'SVC_CD', 'PNL_CAT_CD', 'RANK_CD',
#'            'PNL_TERM_DT', 'PNL_BGN_DT', 'OMB_RACE_CD',
#'            'OMB_ETHNC_NAT_ORIG_CD', 'POST_911_DPLY_IND_CD'),
#'   rankDat = 'rankDat',
#'   payRanks = 4,
#'   post911 = FALSE,
#'   until = NULL,
#'   dischargedAfter = FALSE,
#'   ageDischarge = TRUE,
#'   ageEnlist = FALSE,
#'   ageNow = FALSE,
#'   yearsServed = FALSE,
#'   dateformat = '%m/%d/%Y',
#'   formats = 'default',
#'   rmDeviates = FALSE,
#'   timeCats = TRUE,
#'   saveData = TRUE,
#'   onlyIDs = FALSE,
#'   oversample = TRUE,
#'   exclude = FALSE,
#'   typos = list()
#' )
#'
#' out <- sampleVADIR(VADIR_fake, params = params, seed = 19)
#' }
sampleVADIR <- function(data, n = 4500, vars = 'all', rankDat = 'rankDat',
                        payRanks = 4, post911 = TRUE, dischargedAfter = FALSE,
                        until = NULL, ageDischarge = TRUE, ageEnlist = FALSE,
                        ageNow = FALSE, yearsServed = FALSE, dateformat = '%m/%d/%Y',
                        params = NULL, formats = 'default', typos = list(),
                        rmDeviates = FALSE, timeCats = FALSE, saveData = TRUE,
                        onlyIDs = FALSE, oversample = FALSE,
                        exclude = FALSE, seed = NULL){

  t1 <- Sys.time()
  if(!is.null(seed)){set.seed(seed)}

  ### STEP 1) *Optional* loading of parameter list
  if(!is.null(params)){if(is(params, 'list') & !is.null(names(params))){
    list2env(params, envir = as.environment(-1))
    if(is.null(rankDat)){rankDat <- 'rankDat'}
  }}
  ### Creating a record of the arguments called with the function
  call <- list(n = n, vars = vars, payRanks = payRanks, post911 = post911,
               dischargedAfter = dischargedAfter, until = until, ageDischarge = ageDischarge,
               ageEnlist = ageEnlist, ageNow = ageNow, yearsServed = yearsServed,
               dateformat = dateformat, formats = formats, typos = typos,
               rmDeviates = rmDeviates, timeCats = timeCats, oversample = oversample,
               exclude = exclude, seed = seed)


  ### STEP 2) Variable names and adding 'ID' to data
  N <- nrow(data)
  if(identical(tolower(vars), 'all')){
    vars <- c('PN_Sex_CD', 'PN_BRTH_DT', 'SVC_CD', 'PNL_CAT_CD',
              'RANK_CD', 'PNL_TERM_DT', 'PNL_BGN_DT', 'OMB_RACE_CD',
              'OMB_ETHNC_NAT_ORIG_CD', 'POST_911_DPLY_IND_CD')
  }
  vars <- toupper(vars)
  dates <- sort(vars[grep('DT$', vars)])
  colnames(data) <- toupper(colnames(data))
  if(all(vars %in% colnames(data))){
    otherinfo <- data.frame(ID = seq_len(N), data[, setdiff(colnames(data), setdiff(vars, dates))], check.names = FALSE)
    if(ncol(otherinfo) == 1){
      otherinfo <- NULL
    } else {
      colnames(otherinfo) <- c('ID', setdiff(colnames(data), setdiff(vars, dates)))
    }
    data <- data.frame(ID = seq_len(N), data[, vars], check.names = FALSE, stringsAsFactors = FALSE)
    if(any(sapply(data, class) == 'factor')){
      fchar <- which(sapply(data, class) == 'factor')
      for(i in seq_along(fchar)){data[, fchar[i]] <- as.character(data[, fchar[i]])}
    }
  } else stop('Variable names do not match with original input!')

  if(ifelse(!is.null(exclude), !identical(exclude, FALSE), FALSE)){
    if(all(c('MA_PR_ZIP_CD', 'MA_LN1_TX') %in% colnames(otherinfo))){
      zips0 <- otherinfo$ID[which(is.na(otherinfo$MA_PR_ZIP_CD))]
      zips0 <- union(zips0, otherinfo$ID[which(otherinfo$MA_PR_ZIP_CD == '')])
      ntc <- otherinfo$ID[which(startsWith(otherinfo$MA_LN1_TX, 'NTC'))]
      if(is.numeric(exclude)){
        exclude <- union(union(zips0, exclude), ntc)
      } else if(isTRUE(exclude)){
        exclude <- union(zips0, ntc)
      }
    }
  }


  ### STEP 3) Response formats
  if(length(formats) < length(vars)){
    if(!identical(tolower(formats), 'default')){
      warning('Format list shorter than # of variables; using default formats')
    }
    formats <- setNames(list(
      c('F', 'M'), 'MM/DD/YYYY 12:00:00 AM', c('N', 'A', 'M', 'F'), c('V', 'A', 'N'),
      c('1LT', '1STLT', '2LT', '2NDLT', 'A1C', 'AB', 'AMN', 'CAPT', 'CDR',
        'COL', 'CPL', 'CPT', 'CW2', 'CW3', 'CWO2', 'ENS', 'GYSGT',
        'LCDR', 'LCPL', 'LT', 'LTJG', 'MAJ', 'LTCOL', 'LTC', 'PFC', 'PO1',
        'PO2', 'PO3', 'PV2', 'PVT', 'SA', 'SGT', 'SN', 'SPC', 'SR',
        'SRA', 'SSG', 'SSGT', 'WO1', 'MIDSHP', 'PV1', 'Sgt', 'SP4', 'SP5'), 'MM/DD/YYYY', 'MM/DD/YYYY',
      c('C', 'N', 'R', 'Z', 'M', 'H'), c('N', 'H'), c('Y', 'N')
    ), vars)
  } else if(length(formats) > length(vars)){
    if(all(vars %in% names(formats))){
      formats <- formats[vars]
    } else stop('Format list longer than variable names')
  } else if(is.null(names(formats))){names(formats) <- vars}
  fixTypos <- function(data, old, new = NULL, var = 'RANK_CD'){
    if(is.null(new) & is(old, 'list') & length(old) == 2){
      if(all(tolower(names(old)) %in% c('old', 'new'))){
        new <- old$new; old <- old$old
      } else {
        new <- old[[2]]; old <- old[[1]]
      }
    }
    stopifnot(length(old) == length(new))
    old <- toupper(old); new <- toupper(new)
    for(i in seq_along(old)){
      ierr <- which(data$RANK_CD == old[i])
      if(length(ierr) > 0){data[ierr, var] <- new[i]}
    }
    return(data)
  }
  data <- fixTypos(data = data, old = c('CW02', 'CW0-2', 'PV1'),
                   new = c('CWO2', 'CWO2', 'PVT'))
  if(length(typos) > 0){
    names(typos) <- rtn <- tolower(names(typos))
    if(is(typos, 'list') & !is.null(rtn)){
      typargs <- switch(2 - all(rtn %in% c('old', 'new')), typos,
                        list(old = rtn, new = unname(unlist(typos))))
      data <- do.call(fixTypos, c(list(data = data), typargs))
    } else {
      message(paste0('"typos" must be a named list'))
    }
  }
  # Check responses with variable formats
  for(v in setdiff(vars, c(dates, 'RANK_CD'))){
    clean <- all(unique(data[[v]]) %in% formats[[v]])
    if(!clean & rmDeviates){
      clean <- which(!data[[v]] %in% formats[[v]])
      message(paste0('   [N = ', length(clean), '] cases removed for deviations on "', v, '"'))
      data <- data[-clean, ]
      N <- nrow(data)
    } else if(!clean){
      stop(paste0('Responses on "', v, '" deviate from formats'))
    }
  }


  ### STEP 4) Check (or import) 'rankDat.RDS' and recode 'RANK_CD'
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


  ### STEP 5) Time/date variable cleaning
  stime <- function(x, asDate = TRUE, dateonly = NULL){
    if(is.null(dateonly)){dateonly <- !grepl(':', x)}
    ff <- switch(2 - dateonly, '%m/%d/%Y', '%m/%d/%Y %H:%M:%S')
    fun <- switch(2 - grepl('/', x), function(x, fo){strptime(x, format = fo)},
                  function(x, fo){as.Date(x)})
    out <- fun(x, ff)
    if(asDate){out <- as.Date(out)}
    return(out)
  }
  countYears <- function(time1, time2 = 'today', units = "years", floor = TRUE){
    if(identical(time2, 'today')){time2 <- lubridate::today()}
    times <- list(time1 = time1, time2 = time2)
    for(i in 1:2){
      if(!any(sapply(c('Date', 'POSIXlt', 'POSIXlt'), function(k) is(times[[i]], k)))){
        times[[i]] <- stime(times[[i]], asDate = TRUE)
      }
    }
    list2env(times, envir = as.environment(-1))
    calcAge <- lubridate::interval(time1, time2)/lubridate::duration(num = 1, units = units)
    if(floor){return(as.integer(floor(calcAge)))}
    return(calcAge)
  }
  dtn <- setNames(dates, c('birth', 'start', 'end'))
  dateopts <- c(ageDischarge = ageDischarge, ageEnlist = ageEnlist,
                ageNow = ageNow, yearsServed = yearsServed)
  for(i in seq_along(dates)){
    if(all(grepl(':|AM$', data[, dates[i]]))){
      data[, dates[i]] <- do.call(rbind, strsplit(data[, dates[i]], ' '))[, 1]
    }
    if(any(sapply(data[, dates[i]], function(z) grepl('/', z)))){
      if(!all(sapply(data[, dates[i]], function(z) grepl('/', z)))){
        stop('More than one date format present in data')
      }
      data[, dates[i]] <- as.Date(strptime(data[, dates[i]], format = dateformat))
    } else {
      data[, dates[i]] <- as.Date(data[, dates[i]])
    }
  }
  #suppressMessages(require(lubridate))
  if(any(dateopts) & !'AGEDISCHARGE' %in% colnames(otherinfo)){ ## WARNING: Can take a LONG time...
    cat('Computing time variable(s)...  ')
    datevars <- setNames(data.frame(sapply(which(dateopts), function(z){
      z1 <- ifelse(z != 4, 'birth', 'start')
      z2 <- switch(z, 'end', 'start', lubridate::today(), 'end')
      t1 <- data[, dtn[[z1]]]
      t2 <- switch(2 - (z == 3), rep(z2, N), data[, dtn[[z2]]])
      sapply(seq_len(N), function(i) countYears(t1[i], t2[i]))
    })), names(dateopts[dateopts]))
    cat('Complete!\n')
    datevars <- datevars[, names(dateopts[dateopts]), drop = FALSE]
    data <- cbind.data.frame(data, datevars)
  } else if('AGEDISCHARGE' %in% colnames(otherinfo)){
    data <- data.frame(data, ageDischarge = otherinfo$AGEDISCHARGE)
  }


  ### STEP 6) Select sample based on start/end date of service
  # First checks "until" argument; i.e., last possible start-service date
  if(is.character(until) | lubridate::is.Date(until)){
    until <- switch(2 - lubridate::is.Date(until), until, switch(
      2 - identical(until, 'today'),
      lubridate::today(), stime(until)))
    keep <- which(data[, dtn[['start']]] < until)
    if(length(keep) == 0){stop('Something is wrong... we must investigate')}
    data <- data[keep, ]
    if(nrow(data) != N){
      message(paste0('   [N = ', N - nrow(data), '] Post ', until, ' cases removed'))
      N <- nrow(data)
    }
  }
  # Next checks if there is an earliest possible discharge date ("dischargedAfter")
  if(is.character(dischargedAfter) | is.numeric(dischargedAfter)){
    if(identical(tolower(dischargedAfter), 'pastyear')){
      now <- as.numeric(strsplit(as.character(lubridate::today()), '-')[[1]])
      now[1] <- now[1] - 1
      n10 <- which(now < 10)
      if(length(n10) > 0){now[n10] <- paste0('0', now[n10])}
      dischargedAfter <- paste0(now[c(2, 3, 1)], collapse = '/')
    } else if(nchar(dischargedAfter) == 4){
      dischargedAfter <- paste0('01/01/', dischargedAfter)
    }
    dischargedAfter <- stime(dischargedAfter)
    keep <- which(data[, dtn[['end']]] >= dischargedAfter)
    if(length(keep) == 0){stop(paste0('No discharges recorded for ', dischargedAfter, ' or later'))}
    data <- data[keep, ]
    if(nrow(data) != N){
      message(paste0('   [N = ', N - nrow(data), '] discharges prior to ', dischargedAfter, ' removed'))
      N <- nrow(data)
    }
  }
  # Then checks if all start-service dates occur after 9/11 (if specified by user)
  n11 <- stime(ifelse(is.character(post911), post911, '09/11/2001'))
  if((isTRUE(post911) | is.character(post911)) & any(data[, dtn[['start']]] <= n11)){
    keep <- which(data[, dtn[['start']]] > n11)
    if(length(keep) == 0){stop('Something is wrong... we must investigate')}
    data <- data[keep, ]
    if(nrow(data) != N){
      message(paste0('   [N = ', N - nrow(data), '] Pre-9/11 cases removed'))
      N <- nrow(data)
    }
  }
  # Finally, assesses whether the variable below is useful anymore
  p911 <- 'POST_911_DPLY_IND_CD'
  if(length(unique(data[, p911])) == 1){
    message(paste0('Removing the variable "', p911, '" due to redundancy'))
    data <- data[, setdiff(colnames(data), p911)]
    vars <- setdiff(vars, p911)
    formats <- formats[vars]
  }
  data <- data[, setdiff(colnames(data), dates), drop = FALSE]
  if(N < n){
    message(paste0('\nSTOP: Total sample is now: [N = ', N, ']'))
    stop(paste0('Sample is smaller than size requested (n = ', n, ')'))
  }


  ### STEP 7) Split the dataset by gender and stratify each group separately
  #suppressMessages(require(splitstackshape))
  gendats <- split(data[, -grep('SEX', colnames(data))], data$PN_SEX_CD)
  genselect <- names(gendats)
  n2 <- floor(n/2)
  logen <- which(sapply(gendats, nrow) < n2)
  gensamps <- setNames(vector('list', 2), names(gendats))
  timeCats <- ifelse(isTRUE(timeCats), 4, ifelse(
    is.numeric(timeCats) & timeCats > 1,
    pmin(timeCats, 4), FALSE)) # Limits to a max of 4, if made categorical
  if(length(logen) == 2){
    warning('Both genders have fewer cases than requested\nSelecting all cases in each category')
    gensamps <- gendats
  } else {
    if(length(logen) == 1){
      genselect <- genselect[-logen]
      gensamps[[genselect[logen]]] <- gendats[[genselect[logen]]]
      logen <- switch(names(gendats)[logen], 'F' = 'Female', 'M' = 'Male')
      warning(paste0('The category "', logen, '" has fewer cases than requested\nSelecting all cases in this category'))
    }
    for(g in genselect){
      leaveout <- 'ID'
      dat <- gendats[[g]]
      vs <- colnames(dat)
      singular <- which(apply(dat, 2, function(z) length(unique(z)) == 1))
      if(length(singular) > 0){dat <- dat[, -singular]}
      agevars <- sapply(c('age', 'years'), function(a) which(startsWith(vs, a)))
      agevars <- unname(unlist(agevars[sapply(agevars, length) > 0]))
      if(length(agevars) > 0 & !identical(timeCats, FALSE)){
        qs <- c(0, seq(1/timeCats, 1, by = 1/timeCats))
        for(j in agevars){
          dat[, j] <- cut(dat[, j], quantile(dat[, j], qs), include.lowest = TRUE)
          levels(dat[, j]) <- 1:4
          dat[, j] <- as.character(dat[, j])
        }
      } else if(length(agevars) > 0){
        leaveout <- c(leaveout, vs[agevars])
      }
      stratvars <- setdiff(vs, leaveout)
      if(!is.null(exclude)){if(is.numeric(exclude)){dat <- dat[which(!dat$ID %in% exclude), ]}}
      dat$cats <- cats <- as.character(interaction(dat[, stratvars]))
      if(length(unique(cats)) > n2){stop('Number of strata exceed requested sample size')}
      sizes <- sizes0 <- c(round(prop.table(table(cats)) * n2))
      if(any(sizes == 0) & !oversample){
        ss <- names(sizes)[which(sizes == 0)]
        dat <- dat[-which(dat$cats %in% ss), ]
        sizes <- sizes[sizes != 0]
      } else if(any(sizes == 0)){
        sizes[sizes == 0] <- 1
      }
      gensamps[[g]] <- as.data.frame(splitstackshape::stratified(dat, group = 'cats', size = sizes))
      attr(gensamps[[g]], 'stratvars') <- stratvars
      attr(gensamps[[g]], 'sizes0') <- sizes0
    }
  }


  ### ---------------- COMPLETE ---------------- ###
  if(onlyIDs){
    output <- list(call = call, Females = gensamps[['F']]$ID, Males = gensamps[['M']]$ID)
  } else {
    output <- list(call = call, Females = gensamps[['F']], Males = gensamps[['M']])
    atts1 <- attributes(output$Females)
    atts2 <- attributes(output$Males)
    if(!is.null(otherinfo)){
      id <- otherinfo$ID
      n1 <- c(colnames(output$Females), setdiff(colnames(otherinfo), 'ID'))
      n2 <- c(colnames(output$Males), setdiff(colnames(otherinfo), 'ID'))
      output$Females <- cbind.data.frame(output$Females, otherinfo[match(output$Females$ID, id), -1])
      output$Males <- cbind.data.frame(output$Males, otherinfo[match(output$Males$ID, id), -1])
      attr(output$Females, 'sizes0') <- atts1$sizes0
      attr(output$Females, 'stratvars') <- atts1$stratvars
      attr(output$Males, 'sizes0') <- atts2$sizes0
      attr(output$Males, 'stratvars') <- atts2$stratvars
      colnames(output$Females) <- n1
      colnames(output$Males) <- n2
    }
  }
  if(saveData){
    output$data <- data
    if(!is.null(otherinfo)){
      id <- otherinfo$ID
      nn <- c(colnames(output$data), setdiff(colnames(otherinfo), 'ID'))
      output$data <- cbind.data.frame(output$data, otherinfo[match(output$data$ID, id), -1])
      colnames(output$data) <- nn
    }
  }
  print(attr(output, 'time') <- Sys.time() - t1)
  return(output)
}
