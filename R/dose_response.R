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
#' @param confidence_intervals Boolean variable to determine whether confidence intervals are returned or not - default false
#' @param certainty Certain response (fixed) or uncertain in the range of lower and upper bounds - default is certain (T)
#' @param use_75_pert Last quantile determined by person years; beyond which there is less confidence
#' @return response for a specific dose (in a data frame)
#' @rdname dose_response
#' @importFrom magrittr %>%
#' @export
#'

dose_response <- function (cause, outcome_type, dose, confidence_intervals = F, certainty = T, use_75_pert = T){

  if (is.na(dose) || class(dose) != "numeric")
    stop ('Please provide dose in numeric')

  if (!cause %in% c('all-cause-cancer', 'all-cause-cvd', 'all-cause-mortality', 'bladder-cancer', 'breast-cancer', 'colon-cancer',
                    'coronary-heart-disease', 'diabetes', 'endometrial-cancer', 'esophageal-cancer', 'gastric-cardia-cancer',
                    'head-and-neck-cancer', 'heart-failure', 'kidney-cancer', 'liver-cancer', 'lung-cancer',
                    'myeloma', 'myeloid-leukemia', 'prostate-cancer', 'rectum-cancer', 'stroke')){
    stop('Unsupported cause/disease. Please select from \n
         all-cause-cancer \n
         all-cause-mortality \n
         all-cause-cvd \n
         bladder-cancer \n
         breast-cancer\n
         colon-cancer \n
         coronary-heart-disease \n
         diabetes \n
         endometrial-cancer \n
         esophageal-cancer \n
         gastric-cardia-cancer \n
         head-and-neck-cancer \n
         heart-failure \n
         kidney-cancer \n
         liver-cancer \n
         lung-cancer \n
         myeloma \n
         myeloid-leukemia \n
         prostate-cancer \n
         rectum-cancer \n
         stroke')

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

  pert_75 <- readr::read_csv(system.file("extdata", "75p_diseases.csv",
                                         package = "drpa",
                                         mustWork = TRUE),
                             col_type = readr::cols())

  # print(summary(pert_75))

  cond <- ifelse(use_75_pert,
                 abs(lookup_table$dose - dose),
                 which.min(abs(lookup_table$dose - dose)))



  if (confidence_intervals){

    if (certainty){


      rr <- lookup_table[cond, "RR"] %>% as.numeric()
      lb <- lookup_table[cond, "lb"] %>% as.numeric()
      ub <- lookup_table[cond, "ub"] %>% as.numeric()

      return (data.frame (rr = rr, lb = lb, ub = ub))

    }else{


      lb <- lookup_table[cond, "lb"] %>% as.numeric()
      ub <- lookup_table[cond, "ub"] %>% as.numeric()
      rr <- stats::runif(1, min=lb, max=ub)

      return (data.frame (rr = rr, lb = lb, ub = ub))

    }

  }else{


    if (certainty){

      rr = lookup_table[cond, "RR"] %>% as.numeric()
      return(data.frame(rr = rr))

    }else{


      lb <- lookup_table[cond, "lb"] %>% as.numeric()
      ub <- lookup_table[cond, "ub"] %>% as.numeric()

      rr <- stats::runif(1, min=lb, max=ub)
      return(data.frame(rr = rr))

    }

  }

  if (certainty){


    lb <- lookup_table[cond, "lb"] %>% as.numeric()
    ub <- lookup_table[cond, "ub"] %>% as.numeric()

    return(stats::runif(1, min=lb, max=ub))
  }
  else{

    return(lookup_table[cond, "RR"] %>% as.numeric())
  }

  # browser()

  # if ((!is.null(dose) && !is.na(dose)) && dose <= 35)
  #   return(lookup_table[which.min(abs(lookup_table$dose - dose)), "RR"])
  # else
  #   return(lookup_table[which.min(abs(lookup_table$dose - 35)), "RR"])

}
