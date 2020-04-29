#' chi_and_bin
#'
#' Example data from "https://www.rivm.nl/en/novel-coronavirus-covid-19/children-and-covid-19"

#' @param x A matrix-like object with two columns: success and fail
#'
#' @return Print outs results of chisq.test and binom.test. Returns result of
#' binom.test as a tibble with fields:
#' "estimate", "statistic", "p.value", "parameter", "conf.low", "conf.high",
#' "method", "alternative"
#' @export
#'
#' @examples
#' data <- structure(list(Age = structure(c(2L, 7L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 1L),
#' .Label = c(">=80", "0-4", "13-18", "19-29", "30-39", "40-49", "5-12", "50-59",
#' "60-69", "70-79"), class = "factor"),
#' Infected.contacts = c(0L, 0L, 0L, 5L, 2L, 12L, 22L, 3L, 7L, 4L),
#'  Non.infected.contacts = c(2L, 29L, 12L, 64L, 44L, 152L, 209L, 47L, 67L, 28L)),
#'   class = "data.frame", row.names = c(NA, -10L))
#'
#' chi_and_bin(data[,2:3])
chi_and_bin <- function(x) {
  if(dim(x)[2] != 2)
    stop("Object must have just 2 columns")

  print(x)

  if(requireNamespace("stats",quietly = TRUE)){
    print(stats::chisq.test(x))

    print(stats::chisq.test(x,simulate.p.value = TRUE))

    #print(stats::prop.test(as.matrix(x)))
  }

  if(requireNamespace("purrr",quietly = TRUE)){
    l1 <- x[,1]
    l2 <- x[,2]

    result <- purrr::map2_dfr(l1,l2, ~broom::tidy(stats::binom.test(c(.x,.y))))
  }
  print(result)
}
