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
#' @param use_75_pert Last quantile determined by person years; beyond which there is less confidence - default is TRUE
#' @param confidence_intervals Boolean variable to determine whether confidence intervals are returned or not - default is FALSE
#' @return response for a specific dose (in a data frame)
#' @rdname dose_response
#' @importFrom magrittr %>%
#' @export
#'

dose_response <- function (cause, outcome_type, dose, quantile = 0.5, use_75_pert = T, confidence_intervals = F){

  if (is.null(dose) || class(dose) != "numeric")
    stop ('Please provide dose in numeric')

  if (is.na(quantile) || class(quantile) != 'numeric' || quantile >= 1 || quantile < 0)
    stop('Please provide quantile between 0 and less than 1')

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

  if(use_75_pert){
    upper_limit <- pert_75 %>% dplyr::filter(disease == cause) %>% dplyr::select(outcome_type) %>% as.numeric()
    dose[dose > upper_limit] <- upper_limit
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
