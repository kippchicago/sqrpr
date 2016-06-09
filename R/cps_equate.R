#' @title Create vector of imputed prior spring scores
#' @description
#' \code{cps_equate} returns a character vector of prior sprng scores that are imputed
#' from fall scores.  This function uses the regression coefficients that described
#' on page 2 of the \emph{CPS Metric Guide for National School Growth Percentile (NWEA)}.
#'
#' @details
#' From the handbook: "[f]or students who do not have a valid spring pretest
#' score but did test in the fall, thier fall scores will be equated to an
#' equivalent spring score. This equated score will then be incorporated into
#' the school's spring average for the purpose  of calculating the
#' spring-to-spring growth percentile" (SQRP Handbook 12/5/2014, 25).
#'
#' The regression coefficients are taken from page 2  of the \emph{CPS Metric Guide for National
#' School Growth Percentile (NWEA)}.  Equating is only performed for grades 3-8.
#'
#' @param rit_score a vector of fall RIT scores
#' @param subject list a character vector of subjects. The only acceptable values are
#' "Mathematics" or "Reading".
#' @param grade_level A numeric or integer vector of grade levels.  Grade levels
#' below 3rd are ignored and the original RIT score is returned wihtout any
#' imputation/equating performed.
#'
#' @return a character vector of \code{length(rit_score)}.
#' @export
#' @examples
#' cps_equate(175, "Reading", 6)
#' cps_equate(c(190, 175), c("Mathematics", "Reading"), c(6,7))

cps_equate <- function(rit_score, subject, grade_level, cps_constants = cps_constants_2015){
  inner <- function(rit_score, subject, grade_level) {
    assertthat::assert_that(subject %in% c("Mathematics", "Reading"))
    if(!grade_level %in% c(3:8)) return(rit_score)
    alpha <- cps_constants %>%
      dplyr::filter(variable == "alpha",
             measurementscale == subject) %>%
      dplyr::select(value)

    alpha <- alpha +
      cps_constants %>%
      dplyr::filter(variable == "grade_dummy",
             measurementscale == subject,
             grade == grade_level) %>%
      dplyr::select(value)

    beta <- cps_constants %>%
      dplyr::filter(variable == "beta",
             measurementscale == subject) %>%
      dplyr::select(value)

    new_rit <- alpha + beta * rit_score

    #return
    as.numeric(new_rit)
  }
  outer <- Vectorize(inner)

  #return
  outer(rit_score, subject, grade_level)
}
