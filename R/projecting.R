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
             testpercentile=sqrpr::get_test_percentile(measurementscale,
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
    dplyr::inner_join(norms_students_2011 %>% 
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
    rename(schoolname=school) %>%
    select(-schoolname.y)
  
  out
}

