# Fake VADIR data

library(lubridate)
library(usethis)

N <- 200000

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

# Population proportions based on previous VADIR data
props <- setNames(list(
  c(0.17794, 0.82206),
  NA,
  c(0.21637, 0.47815, 0.17102, 0.13446),
  c(0.56341, 0.31127, 0.12532),
  NULL,
  NA,
  NA,
  c(0.74463, 0.15319, 0.01331, 0.04588, 0.04299),
  c(0.86207, 0.13793),
  c(0.43643, 0.56357)
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


### 4) Simulate dates
# Proportions of ages at enlistment, starting at 17, from previous VADIR data
bgn_props <- c(0.04310, 0.13179, 0.0942, 0.06336, 0.06049, 0.1015, 0.09909,
               0.08651, 0.07165, 0.05816, 0.04568, 0.03514, 0.02719, 0.02011,
               0.01515, 0.0118, 0.00903, 0.00673, 0.00447, 0.00311, 0.00263,
               0.00199, 0.0017, 0.0011, 0.00114, 0.00057, 0.00057, 0.00039,
               0.00046, 0.00034, 0.00031, 0.00013, 0.00008, 0.00007, 0.00004,
               0.00003, 0.00001, 0.00002, 0.00005, 0.00002, 0.00001, 0.00004,
               0.00001, 0.00001, 0.00001, 0.00001)
bgn_ages <- 17:62

set.seed(1)
BGN <- dateMaker(N, ymin = 2002, ymax = 2019)

BRTH <- sapply(BGN, function(enlist){
  enlist_age <- sample(bgn_ages, 1, prob = bgn_props)
  out <- strptime((strptime(enlist, '%m/%d/%Y') - lubridate::years(enlist_age)) - 1, '%Y-%m-%d')
  out <- paste0(strsplit(as.character(as.Date(out)), '-')[[1]][c(2, 3, 1)], collapse = '/')
  if(startsWith(out, '02/29')){out <- paste0('02/25/', lubridate::year(strptime(out, '%m/%d/%Y')))}
  return(out)
})

term_props <- c(0.001, 0.02467, 0.02317, 0.02057, 0.02217, 0.03028, 0.04071,
                0.03888, 0.1094, 0.14669, 0.10963, 0.08385, 0.06833, 0.05929,
                0.04873, 0.03958, 0.03239, 0.02527, 0.0188, 0.0142, 0.00991,
                0.00686, 0.00535, 0.0039, 0.00326, 0.00268, 0.00183, 0.0014,
                0.00133, 0.00125, 0.00116, 0.00082, 0.00077, 0.00046, 0.00031,
                0.00022, 0.00019, 0.0001, 0.00004, 0.00002, 0.00009, 0.00005,
                0.00004, 0.00019, 0.00002, 0.00005, 0.00001, 0.00003, 0.00001,
                0.00001, 0.00001, 0.00002)
term_ages <- 17:68

TERM <- unname(sapply(BRTH, function(birth){
  birth <- strptime(birth, '%m/%d/%Y')
  discharge_date <- as.Date('2020-01-01')
  limit <- discharge_date - 1
  while(discharge_date > limit){
    upper <- lubridate::year(limit + 1) - lubridate::year(birth)
    t <- ifelse(upper > max(term_ages), length(term_ages), which(term_ages == upper))
    discharge_age <- sample(term_ages[1:t], 1, prob = term_props[1:t])
    discharge_date <- birth + lubridate::years(discharge_age) + lubridate::days(1)
    if(is.na(discharge_date)){discharge_date <- as.Date('2020-01-01')}
  }
  out <- paste0(strsplit(as.character(as.Date(discharge_date)), '-')[[1]][c(2, 3, 1)], collapse = '/')
  return(out)
}))


### 5) Simulate other variables and compile dataset
VADIR_fake <- setNames(data.frame(sapply(seq_along(vars), function(i){
  if(vars[i] == 'PN_BRTH_DT'){
    out <- BRTH
  } else if(vars[i] == 'PNL_BGN_DT'){
    out <- BGN
  } else if(vars[i] == 'PNL_TERM_DT'){
    out <- TERM
  } else {
    out <- sample(formats[[i]], N, replace = TRUE, prob = props[[i]])
  }
  return(out)
})), vars)


### 6) Write data
write.csv(VADIR_fake, 'data-raw/VADIR_fake.csv', row.names = FALSE)
usethis::use_data(VADIR_fake, overwrite = TRUE)
