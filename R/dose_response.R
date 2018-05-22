#' Dose response for Physical Acitivity
#'
#' Based on our meta-analysis, returns a response value for a specific cause or disease. \cr
#' 1. Disease/Cause: Name of the disease/cause - from a specific list\cr
#' 2. Type: Mortality or Incidence. \cr
#'
#' At the end, this function returns a response values with or without confidence interval
#'
#'
#' @param cause All-Cause or disease name
#' @param outcome_type Mortality or Incidence
#' @param dose Dose (all-cause or disease)
#' @param confidence_intervals Boolean variable to determine whether confidence intervals are returned or not - default false
#' @param certainty Certain response (fixed) or uncertain in the range of lower and upper bounds - default is certain (T)
#' @return response for a specific dose (in a data frame)
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname dose_response
#' @export
#'

dose_response <- function (cause, outcome_type, dose, confidence_intervals = F, certainty = T){

  if (is.na(dose) || class(dose) != "numeric")
    stop ('Please provide dose in numeric')

  if (!cause %in% c('all-cause-mortality', 'breast-cancer', 'cardiovascular-disease',
                   'colon-cancer', 'coronary-heart-disease', 'endometrial-cancer',
                   'heart-failure', 'lung-cancer', 'stroke', 'total-cancer')){
    stop('Unsupported cause/disease. Please select from \n
         all-cause-mortality \n
         breast-cancer\n
         cardiovascular-disease \n
         colon-cancer \n
         coronary-heart-disease \n
         endometrial-cancer \n
         heart-failure \n
         lung-cancer \n
         stroke \n
         total-cancer')

  }

  if (!outcome_type %in% c('mortality', 'incidence')){
    stop('Unsupported outcome_type. Please select from \n
         mortality \n
         incidence')
  }

  if (cause == 'all-cause-mortality' && outcome_type == 'incidence')
    stop('Incidence does not exist for all-cause-mortality')

  fname <- paste(cause, outcome_type, sep = "-")

  if (cause == 'all-cause-mortality')
    fname <- cause

  print(fname)

  lookup_table <- readr::read_csv(system.file("extdata", paste0(fname, ".csv"),
                                              package = "drpa",
                                              mustWork = TRUE),
                                  col_type = readr::cols())

  # print(summary(lookup_table))

  if (confidence_intervals){

    if (certainty){

      rr <- lookup_table[which.min(abs(lookup_table$dose - dose)), "RR"] %>% as.numeric()
      lb <- lookup_table[which.min(abs(lookup_table$dose - dose)), "lb"] %>% as.numeric()
      ub <- lookup_table[which.min(abs(lookup_table$dose - dose)), "ub"] %>% as.numeric()

      return (data.frame (rr = rr, lb = lb, ub = ub))

    }else{


      lb <- lookup_table[which.min(abs(lookup_table$dose - dose)), "lb"] %>% as.numeric()
      ub <- lookup_table[which.min(abs(lookup_table$dose - dose)), "ub"] %>% as.numeric()
      rr <- stats::runif(1, min=lb, max=ub)

      return (data.frame (rr = rr, lb = lb, ub = ub))

    }

  }else{


    if (certainty){

      rr = lookup_table[which.min(abs(lookup_table$dose - dose)), "RR"] %>% as.numeric()
      return(data.frame(rr = rr))

    }else{

      lb <- lookup_table[which.min(abs(lookup_table$dose - dose)), "lb"] %>% as.numeric()
      ub <- lookup_table[which.min(abs(lookup_table$dose - dose)), "ub"] %>% as.numeric()

      rr <- stats::runif(1, min=lb, max=ub)
      return(data.frame(rr = rr))

    }

  }

  if (certainty){

    lb <- lookup_table[which.min(abs(lookup_table$dose - dose)), "lb"] %>% as.numeric()
    ub <- lookup_table[which.min(abs(lookup_table$dose - dose)), "ub"] %>% as.numeric()

    return(stats::runif(1, min=lb, max=ub))
  }
  else
    return(lookup_table[which.min(abs(lookup_table$dose - dose)), "RR"] %>% as.numeric())

  # browser()

  # if ((!is.null(dose) && !is.na(dose)) && dose <= 35)
  #   return(lookup_table[which.min(abs(lookup_table$dose - dose)), "RR"])
  # else
  #   return(lookup_table[which.min(abs(lookup_table$dose - 35)), "RR"])

}
