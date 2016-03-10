#' @title Create cdf with projected growth from a single season cdf
#'
#' @description \code{project_s2s} accepts a single season, which
#' must be "Spring", cdf and creates  a second season with projected
#' growth, and appends the projected growth to the original cdf.
#'
#' @details This function essentially creates a two season CDF where
#' the second, later season has anticipated results.  The resulting
#' CDF is suitable for passing to \code{\link{school_growth_percentile}}
#' and after filtering to the second season to \code{\link{school_attainment_percentile}}.
#'
#'  The \code{percent_cr} is used to control the minimum pernctage
#'  of students in the CDF that make KIPP Tiered college ready
#'  growth. Students not picked to make CR ready growth have growth that
#'  drawn from \code{\link[stats]{rnorm}} with the mean and sd set by
#'  for each student the 2011 NWEA MAP student-level norms.
#'
#'  Since CR students are identified \code{\link[stats]{rbinom}} and
#'  non CR growth is determeind by \code{\link[stats]{rnorm}} these
#'  project growth of of students is fundametnally stochastic.
#'  Consequently, this function is best suited for simulating
#'  growth.
#'
#'  As with \code{\link{school_growth_percentile}}, you can pass single
#'  season CDF of fall results to impute prior spring scores for students
#'
#' @inheritParams school_growth_percentile
#' @param percent_cr percent of students who make excatly KIPP Tiered
#' college ready growth.  This parameter is a scalar between in (0,1)
#' and is passed to \code{\link[stats]{rbinom}}'s \code{prob} paramter.
#'
#' @export
#'
#' @return  list with four data frames attached
#' showing student-, grade-, and school-level growth percentiles
#' as well as the original data.
#'

project_s2s<-function(.data, student_column = "studentid",
                      grade_column = "grade",
                      subject_column = "measurementscale",
                      rit_column = "testritscore",
                      term_column = "termname",
                      dl_indicator = "iep",
                      ell_indicator = "iep",
                      school_indicator = "schoolname",
                      fall_equate_scores=NULL,
                      percent_cr=.10) {

  #select only necessary columns
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
    extract_academic_year

  # Equating bit
  if(!missing(fall_equate_scores)){

    fall_scores<-fall_equate_scores %>%
      dplyr::rename_(studentid=student_column,
                     grade=grade_column,
                     measurementscale=subject_column,
                     testritscore=rit_column,
                     termname=term_column,
                     school=school_indicator
      ) %>%
      ensure_subjects %>%
      ensure_one_seasons_only %>%
      extract_academic_year

    fall_equated<-fall_scores %>%
      dplyr::anti_join(map_df,
                       by=c("studentid", "measurementscale"))


    unmatched<-nrow(fall_equated)

    if(unmatched>0) {

      season1<-unique(map_df$fallwinterspring)
      sy1<-unique(map_df$map_year_academic)
      tname<-unique(map_df$termname)

      fall_equated<- fall_equated %>%
        mutate(testritscore=cps_equate(testritscore,
                                     measurementscale,
                                     grade) %>% round,
             grade=grade-1,
             equated=TRUE,
             fallwinterspring=season1,
             testpercentile=get_test_percentile(measurementscale,
                                                             fallwinterspring,
                                                             grade,
                                                             testritscore),
             termname=tname,
             map_year_academic=sy1
             )


      map_df <- dplyr::rbind_list(map_df, fall_equated) %>%
        mutate(equated=ifelse(is.na(equated), FALSE, TRUE))
    }
  }
  df_length<-nrow(map_df)
  map_df_s2 <- map_df %>%
    dplyr::inner_join(norms_students_2015 %>%
                        dplyr::select(measurementscale=MeasurementScale,
                                      grade=StartGrade,
                                      testritscore=StartRIT,
                                      typical_growth=T22,
                                      reported_growth=R22,
                                      sd_growth=S22
                        ) %>%
                        mutate(grade=as.numeric(grade)),
                      by=c("measurementscale",
                           "grade",
                           "testritscore")
                      ) %>%
    dplyr::mutate(map_year_academic=map_year_academic+1,
                  termname=paste0(fallwinterspring,
                                  " ",
                                  map_year_academic,
                                  "-",
                                  map_year_academic+1),
                  testquartile=mapvizieR::kipp_quartile(testpercentile),
                  tiered_factor=mapvizieR::tiered_growth_factors(testquartile, grade),
                  tiered_growth=round(reported_growth*tiered_factor),
                  simulated_growth=round(rnorm(n(), typical_growth, sd_growth)),
                  testritscore_tiered=testritscore+tiered_growth,
                  testritscore_nontiered=testritscore+simulated_growth
                  ) %>%
    dplyr:: group_by(school, grade) %>%
    dplyr:: mutate(cr_projected=as.logical(rbinom(n(),1,prob = percent_cr)),
           testritscore=ifelse(cr_projected, testritscore_tiered, testritscore_nontiered)
    ) %>%
    ungroup %>%
    dplyr::mutate(grade=grade+1,
           testpercentile=ifelse(cr_projected, sqrpr::get_test_percentile(measurementscale,
                                                                     fallwinterspring,
                                                                     grade,
                                                                     testritscore),
                                 testpercentile)
           )  %>%
    select(-testquartile,
         -tiered_factor,
         -tiered_growth,
         -simulated_growth,
         -testritscore_tiered,
         -testritscore_nontiered,
         -typical_growth,
         -reported_growth,
         -sd_growth
         )

  out<-dplyr::rbind_list(map_df, map_df_s2) %>%
    rename(schoolname=school)

  out
}


#' @title Produce one iteration of SQRP simulation
#'
#' @description \code{simulate_once_sqrp_data} produces a single iteration
#' of \code{\link{simulate_sqrp}}.  Essentially it is a building block function.
#' @param spring_pretest_cdf single season pret-test, spring cdf
#' @param fall_equate_cdf single season fall cdf used to equate missing scores
#' @param school character providing a part of the name of the school in
#' \code{spring_test_cdf} for which to provide simulations,
#' @param pct_cr percent of students that should make exactly KIPP
#' Tiered college ready growth
#' @param aa a list with two elements: \code{aa_col} and \code{aa_ind},
#' which are use to id columns for priority group calculations
#' @param iep a list with two elements: \code{iep_col} and \code{iep_ind},
#' which are use to id columns for priority group calculations
#' @param ... arguments passed to \code{\link{sqrp_level}}
#' @export
#'
#' @return  a data.frame with SQRP points and levels
#'

simulate_once_sqrp_data <- function(spring_pretest_cdf,
                          fall_equate_cdf=NULL,
                          school="Ascend",
                          pct_cr,
                          aa= list(aa_col="studentethnicgroup",
                                   aa_ind="Black or African American"),
                          iep = list(iep_col="sped",
                                     iep_ind=TRUE),
                          ...) {


  # build filter argument

  spring_cdf<-spring_pretest_cdf %>%
    dplyr::filter(grepl(school, schoolname))

  if(!missing(fall_equate_cdf)&!is.null(fall_equate_cdf)){

    fall_cdf<-fall_equate_cdf %>%
      dplyr::filter(grepl(school, schoolname))

    projected<-project_s2s(.data = spring_cdf,
                           fall_equate_scores = fall_cdf,
                           percent_cr = pct_cr )
  } else {
    projected<-project_s2s(.data = spring_cdf,
                           percent_cr = pct_cr )
  }

  growth<-school_growth_percentile(projected)

  attain <- projected %>%
    dplyr::filter(fallwinterspring=="Spring",
                  map_year_academic==max(map_year_academic)) %>%
    school_attainment_percentile

 if(!missing(aa)){
   growth_aa<-priority_group(growth,
                             group_column = "studentethnicgroup",
                             group_id = "Black or African American")
 }  else {
   growth_aa<-NULL
 }

 if(!missing(aa)){
   growth_iep<-priority_group(growth,
                              group_column = iep$iep_col,
                              group_id = iep$iep_ind)

 }  else {
   growth_iep<-NULL
 }


  pct_me<-calc_pct_me(growth = growth)

  args_list <- list(school_name=school,
                    growth=growth,
                    attain=attain,
                    growth_pg_aa=growth_aa,
                    growth_pg_iep=growth_iep,
                    pct_me=pct_me,
                    ...)

  level<-do.call(sqrp_level,
                 args_list)

  # return
  level

}



#' @title Simulations of projected SQRP points and levels
#'
#' @description \code{simulate_sqrp} runs simulation for SQRP proejctions.
#'
#' @inheritParams simulate_once_sqrp_data
#' @param n_sims the number of simulations to run

#' @export
#'
#' @return  a data.frame with SQRP points and levels with
#' \code{nrow(n_sims)}
#'

simulate_sqrp<-function(spring_pretest_cdf,
                        fall_equate_cdf=NULL,
                        school="Asend",
                        pct_cr,
                        aa= list(aa_col="studentethnicgroup",
                                 aa_ind="Black or African American"),
                        iep = list(iep_col="sped",
                                   iep_ind=TRUE),
                        ...,
                        n_sims=10){

  args_list <- c(as.list(environment()), list(...))

  args_list[["n_sims"]]<-NULL

  out<-data.frame

  for(i in 1:n_sims){
    sims<-do.call(simulate_once_sqrp_data,args_list)

    out<-rbind_list(out, sims)
  }

  out
}
