#' Dose response for Physical Activity
#'
#' Based on our meta-analysis, returns a response value for a specific cause or disease. \cr
#' 1. Disease/Cause: Name of the disease/cause - from a specific list\cr
#' 2. Type: Fatal, non-fatal or fatal-and-non-fatal. \cr
#'
#' At the end, this function returns a response values with or without confidence interval
#'
#'
#' @param cause All-cause-mortality or disease name
#' @param outcome_type Fatal, non-fatal or fatal-and-non-fatal
#' @param dose Dose (all-cause or disease)
#' @param quantile Numeric value between 0 and 1 - default is 0.5
#' @param censor_method Censoring method to use. Options are:
#'   \describe{
#'     \item{`none`}{No censoring.}
#'     \item{`default`}{The default censoring method identified by the meta-analysis.}
#'     \item{`WHO-DRL`}{A fixed WHO-recommended double level of physical activity
#'       (17.5 MMET-hours per week).}
#'     \item{`WHO-QRL`}{A fixed WHO-recommended quadruple level of physical activity
#'       (35 MMET-hours per week).}
#' }
#' @param confidence_intervals Boolean variable to determine whether confidence intervals are returned or not - default is FALSE
#' @return response for a specific dose (in a data frame)
#' @rdname dose_response
#' @importFrom magrittr %>%
#' @export
#'

dose_response <- function (cause, outcome_type, dose, quantile = 0.5, censor_method = "default", confidence_intervals = F){

  if (is.null(dose) || !is.numeric(dose))
    stop ('Please provide dose in numeric')

  if (length(quantile) != 1L ||
      is.na(quantile) ||
      !is.numeric(quantile) ||
      quantile >= 1 ||
      quantile < 0)
    stop('Please provide the quantile value between 0 and 1')

  if (length(censor_method) != 1L ||
      is.na(censor_method) ||
      !is.character(censor_method) ||
      !censor_method %in% c("none", "default", "WHO-DRL", "WHO-QRL"))
    stop('Please specificy `censor_method` by selecting either of four options: `none`, `default`,`WHO-DRL`,`WHO-QRL`')

  pert_75 <- readr::read_csv(system.file("extdata", "default_cutoff.csv",
                                         package = "drpa",
                                         mustWork = TRUE),
                             col_type = readr::cols())

  if (!cause %in% pert_75$disease){
    stop("Unsupported cause/disease. Please select from \n
           all-cause-cancer  \n
           all-cause-cvd  \n
           all-cause-dementia \n
           all-cause-mortality \n
           alzheimer's-disease \n
           bladder-cancer \n
           breast-cancer \n
           colon-cancer \n
           coronary-heart-disease \n
           depression \n
           depressive-symptoms \n
           diabetes \n
           endometrial-cancer \n
           esophageal-cancer \n
           gastric-cardia-cancer \n
           head-and-neck-cancer \n
           heart-failure \n
           kidney-cancer \n
           liver-cancer \n
           lung-cancer \n
           major-depression \n
           myeloid-leukemia \n
           myeloma \n
           parkinson's-disease \n
           prostate-cancer \n
           rectum-cancer \n
           stroke \n
           vascular-dementia \n")
  }

  if (!outcome_type %in% c('fatal', 'non-fatal', 'fatal-and-non-fatal')){
    stop('Unsupported outcome_type. Please select from \n
         fatal \n
         non-fatal \n
         fatal-and-non-fatal')
  }

  fname <- paste(cause, outcome_type, sep = "-")

  if (!file.exists(system.file("extdata", paste0(fname, ".csv"),
                              package = "drpa"))){
    stop(paste('Sorry but for ', cause, ' the outcome type ', outcome_type, ' is not supported by the package'))
  }

  lookup_table <- readr::read_csv(system.file("extdata", paste0(fname, ".csv"),
                                              package = "drpa",
                                              mustWork = TRUE),
                                  col_type = readr::cols())

  if(censor_method == "default"){
    upper_limit <- pert_75 %>% dplyr::filter(disease == cause) %>% dplyr::select(dplyr::all_of(outcome_type)) %>% as.numeric()
    dose[dose > upper_limit] <- upper_limit
  }else if (censor_method == "WHO-DRL"){ # Double of WHO's recommended level of PA for adults - which is 17.5 MMETs hours per week
    dose[dose > 17.5] <- 17.5
  }else if (censor_method == "WHO-QRL"){ # Quadruple of WHO's recommended level of PA for adults - which is 35 MMETs hours per week
    dose[dose > 35] <- 35
  }


  rr <- stats::approx(x = lookup_table$dose, y = lookup_table$RR, xout = dose,yleft = 1, yright = min(lookup_table$RR))$y
  if (confidence_intervals || quantile != 0.5) {
    lb <-
      stats::approx(
        x = lookup_table$dose,
        y = lookup_table$lb,
        xout = dose,
        yleft = 1,
        yright = min(lookup_table$lb)
      )$y
    ub <-
      stats::approx(
        x = lookup_table$dose,
        y = lookup_table$ub,
        xout = dose,
        yleft = 1,
        yright = min(lookup_table$ub)
      )$y
  }
  ## we assume that the columns describe a normal distribution with standard deviation defined by the upper and lower bounds.
  if (quantile != 0.5){
    rr <- stats::qnorm(quantile, mean = rr, sd = (ub-lb)/1.96)
    rr[rr<0] <- 0
  }

  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }

}
