# Fake VADIR data

library(usethis)

### 1) VARIABLE NAMES
vars <- c('PN_Sex_CD',
          'PN_BRTH_DT',
          'SVC_CD',
          'PNL_CAT_CD',
          'RANK_CD',
          'PNL_TERM_DT',
          'PNL_BGN_DT',
          'OMB_RACE_CD',
          'OMB_ETHNC_NAT_ORIG_CD',
          'POST_911_DPLY_IND_CD')

### 2) RESPONSE OPTIONS
formats <- setNames(list(
  c('F', 'M'), 'MM/DD/YYYY 12:00:00 AM', c('N', 'A', 'M', 'F'), c('V', 'A', 'N'),
  c('1LT', '1STLT', '2LT', '2NDLT', 'A1C', 'AB', 'AMN', 'CAPT', 'CDR',
    'COL', 'CPL', 'CPT', 'CW2', 'CW3', 'CW02', 'CW0-2', 'ENS', 'GYSGT',
    'LCDR', 'LCPL', 'LT', 'LTJG', 'MAJ', 'LTCOL', 'LTC', 'PFC', 'PO1',
    'PO2', 'PO3', 'PV1', 'PV2', 'PVT', 'SA', 'SGT', 'SN', 'SPC', 'SR',
    'SRA', 'SSG', 'SSGT', 'WO1'), 'MM/DD/YYYY', 'MM/DD/YYYY',
  c('C', 'N', 'R', 'Z', 'M'), c('N', 'H'), c('Y', 'N')
), vars)

### 3) dateMaker: Used to simulate date variables
dateMaker <- function(n, ymin = 1950, ymax = 2019, args = NULL){
  mo_args <- list(x = 1:12)
  day_args <- list(x = 'auto')
  ye_args <- list(x = ymin:ymax)
  bigmo <- c(1, 3, 5, 6, 7, 8, 10, 12)
  sample2 <- function(x, prob = NULL){
    out <- sample(x = x, size = 1, replace = FALSE, prob = prob)
    ifelse(out < 10, paste0('0', out), as.character(out))
  }
  if(!is.null(args)){if(is(args, 'list') & !is.null(names(args))){
    names(args) <- tolower(names(args))
    if(sum(startsWith(names(args), 'm')) == 1){
      mo_args <- args[[which(startsWith(names(args), 'm'))]]
      if(!'x' %in% names(mo_args)){mo_args$x <- 1:12}
    }
    if(sum(startsWith(names(args), 'd')) == 1){
      day_args <- args[[which(startsWith(names(args), 'd'))]]
      if(!'x' %in% names(day_args)){day_args$x <- 'auto'}
    }
    if(sum(startsWith(names(args), 'y')) == 1){
      ye_args <- args[[which(startsWith(names(args), 'y'))]]
      if(!'x' %in% names(ye_args)){ye_args$x <- ymin:ymax}
    }
  }}
  if(identical(day_args$x, 'auto')){day_args$x <- list()}
  sapply(1:n, function(z){
    mo <- do.call(sample2, mo_args)
    if(length(day_args$x) == 0){
      day_args$x <- 1:(28 + ifelse(
        identical(mo, '02'), 0, ifelse(
          mo %in% bigmo, 3, 2)))
      day <- do.call(sample2, day_args)
      day_args$x <- list()
    } else {
      day <- do.call(sample2, day_args)
    }
    year <- do.call(sample2, ye_args)
    paste0(c(mo, day, year), collapse = '/')
  })
}

### 4) Specify simulation parameters
N <- 200000
ages <- 18:30
useDates <- TRUE
factorize <- FALSE
year_range <- c(2001, 2019)

### 5) Simulate dataset
probs <- setNames(rep(list(NULL), length(vars)), vars)
VADIR_fake <- setNames(data.frame(sapply(seq_along(vars), function(i){
  dt <- grepl('DT$', vars[i])
  if(useDates & dt){
    dateMaker(N, ymin = min(year_range), ymax = max(year_range))
  } else {
    values <- switch(2 - dt, ages, formats[[i]])
    datvals <- sample(values, N, replace = TRUE, prob = probs[[i]])
    if(!dt & factorize){factor(datvals, levels = values)} else {datvals}
  }
})), vars)

### 6) Write data
write.csv(VADIR_fake, 'data-raw/VADIR_fake.csv', row.names = FALSE)
usethis::use_data(VADIR_fake, overwrite = TRUE)
