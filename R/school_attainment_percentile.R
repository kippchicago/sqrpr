#' @title Calculate school growth percentiles
#'
#' @description \code{school_attainment_percentile} is a workhourse function
#' that calculates school attainment percentiles
#'
#' @param .data a long format (i.e., combined) Comprehensive Data File with the
#' second year (post_test) MAP results.
#' @param student_column column that uniquely identifies students with
#' respect to subject and term
#' @param grade_column  column that identifies grade levels
#' @param subject_column which column identifies measurement scale
#' @param rit_column column that identifes RIT score
#' @param term_column identifies term name
#' @param dl_indicator identifies diverse learner status
#' @param ell_indicator identifies English Language Learner status
#' @param race_indicator identifies race and ethnicity data
#' @param school_indicator column that identifies school
#'
#' @return  list with four data frames attached
#'
#' @export
school_attainment_percentile <- function(.data,
                                         student_column="studentid",
                                         grade_column="grade",
                                         subject_column="measurementscale",
                                         rit_column="testritscore",
                                         term_column="termname",
                                         dl_indicator="iep",
                                         ell_indicator="ell",
                                         race_indicator=NULL,
                                         school_indicator="schoolname",
                                         cps_constants = cps_constants_2015
                                         ){
  map_df<-.data %>%
    dplyr::rename_(studentid=student_column,
                   grade=grade_column,
                   measurementscale=subject_column,
                   testritscore=rit_column,
                   termname=term_column,
                   school=school_indicator
    ) %>%
    ensure_subjects %>%
    ensure_one_seasons_only %>%
    filter(grade %in% c(2:8))
    extract_academic_year

  # calculate grade level attainment percentiles
  grade_level_data <- map_df %>%
    dplyr::group_by(school, measurementscale, grade) %>%
    dplyr::summarize(N_students=n(),
                     avg_rit_score=round(mean(testritscore),1)) %>%
    dplyr::inner_join(filter(cps_constants,
                             variable=="mean_attainment") %>%
                        select(grade, measurementscale, typical_attainment=value),
                      by=c("grade", "measurementscale")) %>%
    dplyr::inner_join(filter(cps_constants,
                             variable=="sd_attainment") %>%
                        select(grade, measurementscale, sd_attainment=value),
                      by=c("grade", "measurementscale")) %>%
    dplyr::mutate(attainment_pctl=round(pnorm(q = avg_rit_score,
                                              mean=typical_attainment,
                                              sd=sd_attainment),2))


  #school level
  school_level_data <- collapse_grade_to_school_attainment(grade_level_data)

  #return
  out<-list(student_level=map_df,
            grade_level=grade_level_data,
            school_level=school_level_data
  )

  #set class
  class(out)<- c("sqrp_attainment", class(out))

  #return
  out

}

#' @export
print.sqrp_attainment <-function(x, ...){

  tbl<-x$school_level %>%
    dplyr::select_("School"="school",
                   "Subject"="measurementscale",
                   "Grades"="grades_served",
                   "N",
                   "Attainment Pctl"="attainment_pctl"
                   ) %>%
    as.data.frame

  cat("Test message\n")
  cat("\n")

  print(tbl, row.names=FALSE)

}



#' @title Calculates school-level (i.e, cohort) attainement percentiles a la CPS's SQRP
#'
#' @description \code{collapse_grade_to_school_attainment} calculates school level growth
#' using CPS specific rules (e.g., using a CPS district wide standard deviation for
#' schools that have more than one grade leve).
#'
#' @param .data data passed to function
#'
#' @return data.frame
#'
collapse_grade_to_school_attainment <- function(.data,
                                                cps_constants = cps_constants_2015){
  # create skeleton that properly accounts for number of grades.  If
  # we have a single grade at a school, then we can take the incoming data
  # as is. Otherwise we got do some weighted averaging if the number of grades
  # is greater than 1.

  est_pctls<-.data

  skeleton <- est_pctls %>%
    dplyr::group_by(school, measurementscale) %>%
    dplyr::summarize(grade_low=min(grade),
                     grade_high=max(grade),
                     n_grades=1+grade_high-grade_low)

  # single grade schools
  single_grade_schools <- skeleton %>%
    dplyr::filter(n_grades==1) %>%
    dplyr::select(school, measurementscale) %>%
    dplyr::inner_join(est_pctls,
                      by=c("school", "measurementscale")) %>%
    dplyr::rename(grades_served=grade, N=N_students) %>%
    dplyr::mutate(grades_served=as.character(grades_served))


  #multi grade schools
  multi_grade_schools <- skeleton %>%
    dplyr::filter(n_grades>1) %>%
    dplyr::select(school, measurementscale) %>%
    dplyr::inner_join(est_pctls,
                      by=c("school", "measurementscale"))

  multi_3_8_schools <- multi_grade_schools %>%
    dplyr::filter(grade %in% c(3:8)) %>%
    dplyr::group_by(school, measurementscale) %>%
    dplyr::summarize(
      grades_served=paste(unique(grade), collapse=" "),
      N=sum(N_students),
      avg_rit_score=plyr::round_any(weighted.mean(avg_rit_score, N_students), 0.1, ceiling),
      typical_attainment=round(weighted.mean(typical_attainment, N_students),1)
    ) %>%
    dplyr::inner_join(dplyr::filter(cps_constants,
                                    variable=="sd_attainment",
                                    is.na(grade)
                                    ) %>%
                        dplyr::select(
                          measurementscale,
                          sd_attainment=value),
                      by="measurementscale"
    ) %>%
    dplyr::mutate(attainment_pctl = round(pnorm(avg_rit_score,
                                                typical_attainment,
                                      sd_attainment),2),
                  attainment_pctl = ifelse(attainment_pctl>.99, .99, attainment_pctl),
                  attainment_pctl = ifelse(attainment_pctl<.01, .01, attainment_pctl)
    )



  stacked <- dplyr::rbind_list(multi_3_8_schools,
                               single_grade_schools)

  if(2 %in% multi_grade_schools$grade){
    multi_2_schools <- multi_grade_schools %>%
      dplyr::filter(grade==2) %>%
      dplyr::mutate(grade=as.character(grade)) %>%
      dplyr::rename(grades_served=grade,
                    N=N_students)

    stacked <- dplyr::rbind_list(stacked, multi_2_schools)

  }


  # return
  stacked

}
