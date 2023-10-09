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
#' @param censor_method Use either no censor method, or the 75th percentile by person years or use a fixed WHO recommended double level of physical activity (17.5 MMet hours per week) - default is 17.5 MMET hours per week (double the WHO recommencded guideline)
#' @param confidence_intervals Boolean variable to determine whether confidence intervals are returned or not - default is FALSE
#' @return response for a specific dose (in a data frame)
#' @rdname dose_response
#' @importFrom magrittr %>%
#' @export
#'

dose_response <- function (cause, outcome_type, dose, quantile = 0.5, censor_method = "WHO-DRL", confidence_intervals = F){

  if (is.null(dose) || class(dose) != "numeric")
    stop ('Please provide dose in numeric')

  if (is.na(quantile) || class(quantile) != 'numeric' || quantile >= 1 || quantile < 0)
    stop('Please provide the quantile value between 0 and 1')

  if (is.na(censor_method) || class(censor_method) != "character" || !censor_method %in% c('none', '75person_years','WHO-DRL'))
    stop('Please specificy `censor_method` by selecting either of three options: `none`, `75person_years`,`WHO-DRL`')

  pert_75 <- readr::read_csv(system.file("extdata", "75p_diseases.csv",
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

  if(censor_method == "75person_years"){
    upper_limit <- pert_75 %>% dplyr::filter(disease == cause) %>% dplyr::select(outcome_type) %>% as.numeric()
    dose[dose > upper_limit] <- upper_limit
  }else if (censor_method == "WHO-DRL"){
    dose[dose > 17.5] <- 17.5
  }

  rr <- approx(x = lookup_table$dose, y = lookup_table$RR, xout = dose,yleft = 1, yright = min(lookup_table$RR))$y
  if (confidence_intervals || quantile != 0.5) {
    lb <-
      approx(
        x = lookup_table$dose,
        y = lookup_table$lb,
        xout = dose,
        yleft = 1,
        yright = min(lookup_table$lb)
      )$y
    ub <-
      approx(
        x = lookup_table$dose,
        y = lookup_table$ub,
        xout = dose,
        yleft = 1,
        yright = min(lookup_table$ub)
      )$y
  }
  ## we assume that the columns describe a normal distribution with standard deviation defined by the upper and lower bounds.
  if (quantile != 0.5){
    rr <- qnorm(quantile, mean = rr, sd = (ub-lb)/1.96)
    rr[rr<0] <- 0
  }

  if (confidence_intervals) {
    return(data.frame (rr = rr, lb = lb, ub = ub))
  }else{
    return(data.frame(rr = rr))
  }

}
