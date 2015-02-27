#' @title Calculate gorwth percenitle
#'
#' @description \code{school_growth_percentile} is the workhourse function
#' that calculates school growth percentiles
#'
#' @param .data a long format (i.e., combined) Comprehensive Data File
#' @param student_column column that uniquely identifies students with
#' respect to subject and term
#' @param grade_column  column that identifies grade levels
#' @param subject_column which column identifies measurement scale
#' @param rit_column column that identifes RIT score
#' @param term_column identifies term name
#' @param dl_indicator identifies diverse learner status 
#' @param ell_indicator identifies English Language Learner status
#' @param school_indicator column that identifies school
#' @param growth_season currenlty not used,
#' @param truncate_growth whether or not to truncate student growth at 1st and 99th percentiles
#' Default is `TRUE`
#' @param fall_equate_scores a data.frame of fall MAP results with columns `studentid`, 
#' `measurementscale`, `testritscore`, and `grade`.  These data are used to 
#' equate missing pre-test (i.e., prior-spring) RIT scores per CPS 
#' guidelines.
#' 
#' @return  list with four data frames attached
#' showing student-, grade-, and school-level growth percentiles 
#' as well as the original data. 
#' 

school_growth_percentile <- function(.data, 
                                     student_column="studentid",
                                     grade_column="grade", 
                                     subject_column="measurementscale", 
                                     rit_column="testritscore",
                                     term_column="termname",
                                     dl_indicator="iep", 
                                     ell_indicator="iep", 
                                     school_indicator="schoolname",
                                     growth_season="SS",
                                     truncate_growth=TRUE,
                                     fall_equate_scores=NULL){
  
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
    ensure_two_seasons_only %>%
    extract_academic_year
  
  # set begin and end seasons
  season1<-season2<-"Spring"
  if("Fall" %in% unique(map_df$fallwinterspring)) season1<-"Fall"
  map_start <- map_df %>% dplyr::filter(fallwinterspring==season1, 
                              map_year_academic==min(map_year_academic))
  
  map_end <- map_df %>% dplyr::filter(fallwinterspring==season2, 
                              map_year_academic==max(map_year_academic))
  
  map_matched<-dplyr::left_join(map_end, 
                         map_start, 
                         by=c("studentid", "measurementscale"))
  
  #change .x to _end
  col_names <- names(map_matched)
  col_names<-stringr::str_replace(col_names, "\\.x", "_end")
  col_names<-stringr::str_replace(col_names, "\\.y", "_start")
  
  # change names
  names(map_matched) <- col_names 
  
  
  
  # reorder columns
  start_cols <- col_names[grepl("_start", col_names)]
  end_cols <- col_names[grepl("_end", col_names)]
  non_seasons_cols <- col_names[!grepl("(_end)|(_start)", col_names)]
  
  cols_reorderd<-c(non_seasons_cols, start_cols, end_cols)
  
  map_matched <- map_matched[,cols_reorderd]
  
  # Equating bit
  if(missing(fall_equate_scores)){
    unmatched<-nrow(filter(map_matched, is.na(testritscore_start)))
    
    if(unmatched>0) {
      warning(paste0("Your data currently has ", unmatched, 
                     " student-subject pairs that are missing ",
                     "pre-test scores from the prior spring.\n\n",
                     "These students will be dropped now\n\n",
                     "You can equate fall scores to prior spring scores ",
                     "by passing a data frame with fall scores to ",
                     "the fall_equate_scores argument."))
    }
    
  } else {
    map_matched <- equate_fall_to_spring(map_matched,
                                         fall_equate_scores %>%
                                           ensure_fall_data)
  }
  
  map_matched <- map_matched %>%
    dplyr::inner_join(norms_students_2011 %>%
                        dplyr::select(measurementscale=MeasurementScale,
                                      grade_start=StartGrade,
                                      testritscore_start=StartRIT,
                                      typical_growth=T22,
                                      reported_growth=R22,
                                      std_dev_growth=S22
                                      ), 
                      by=c("measurementscale",
                           "grade_start",
                           "testritscore_start")
    ) %>%
    dplyr::mutate(growth=testritscore_end-testritscore_start,
                  met_typical=growth>=typical_growth,
                  cgi=(growth-typical_growth)/std_dev_growth,
                  growth_pctl=pnorm(cgi)
                  )
  
  
  if(truncate_growth){
    map_matched <- map_matched %>%
      mutate(truncated=ifelse(growth_pctl>.99, .99, NA),
             truncated=ifelse(growth_pctl<.01,.01,truncated),
             testritscore_end_truncated=ifelse(!is.na(truncated), 
                                           truncated_growth(truncated, 
                                                            typical_growth,
                                                            std_dev_growth,
                                                            testritscore_start),
                                           testritscore_end
                                           )
               )
  }
  
  #return
  map_matched_final <- map_matched %>%
    dplyr::mutate(testritscore_final=testritscore_end)
  if("testritscore_end_truncated" %in% names(map_matched_final)) {
    map_matched_final <- map_matched_final %>% 
      dplyr::mutate(testritscore_final=testritscore_end_truncated)
  }
  map_matched_final<-map_matched_final %>%
    dplyr::group_by(school_end, 
                    grade_end, 
                    measurementscale, 
                    fallwinterspring_start,
                    fallwinterspring_end) %>%
    dplyr::summarize(N_students=n(),
                     avg_rit_start = round(mean(testritscore_start),1),
                     avg_rit_end = round(mean(testritscore_final),1)
                     ) %>%
    dplyr::inner_join(nwea_cps_school_level_norms %>%
                 select(measurementscale,
                        grade_end,
                        avg_rit_start=rit_start,
                        typical_growth_mean,
                        sd_growth,
                        fallwinterspring_start=season_start,
                        fallwinterspring_end=season_end), 
                 by=c("measurementscale",
                      "grade_end",
                      "avg_rit_start",
                      "fallwinterspring_start",
                      "fallwinterspring_end")) %>%
    dplyr::mutate(growth=avg_rit_end-avg_rit_start,
                  z_score=(growth-typical_growth_mean)/sd_growth,
                  growth_pctl = round(pnorm(z_score),2),
                  growth_pctl = ifelse(growth_pctl>.99, .99, growth_pctl),
                  growth_pctl = ifelse(growth_pctl<.01, .01, growth_pctl))
    
  
  # get school_level data
  
  school_level_growth_pctls<-collapse_grade_to_school(map_matched_final)
  
  # package up data
  out<-list(original_data=map_df,
            student_level=map_matched, 
            grade_level=map_matched_final,
            school_level = school_level_growth_pctls
            )
  
  #set_class
  
  class(out)<-c("sqrp_growth", class(out))
  #return 
  out
}


#' @title Calculate truncated growth
#'
#' @description \code{truncated_growth} calculates rit score associated 
#' with a given student growth percentile 
#'
#' @param truncation_percentile percentile to truncate to. 
#' @param typical_growth the typical growth (expected growth) from NWEA
#' norms table
#' @param sd_growth the standard deviation growth (expected growth) from NWEA
#' norms table
#' @param start_rit the starting rit score to which truncated growth will be added.
#' 
#' @return vector of RIT scores equivilent to specified growth percentile
#' 
truncated_growth <- function(truncation_percentile=.99, 
                            typical_growth,
                            sd_growth,
                            start_rit=0
                            ){
  growth<-qnorm(p=truncation_percentile,
             mean=typical_growth,
             sd=sd_growth)
  round(growth + start_rit)
}

#' @title Calculates school-level (i.e, cohort) growth percentiles a la CPS's SQRP
#'
#' @description \code{collapse_grade_to_school} calculates school level growth
#' using CPS specific rules (e.g., using a CPS district wide standard deviation for
#' schools that have more than one grade leve).
#'
#' @param .data data passed to function 
#' 
#' @return data.frame 
#' 
collapse_grade_to_school <- function(.data){
  # create skeleton that properly accounts for number of grades.  If 
  # we have a single grade at a school, then we can take the incoming data 
  # as is. Otherwise we got do some weighted averaging if the number of grades
  # is greater than 1.
  
  est_pctls<-.data
  
  skeleton <- est_pctls %>% 
    dplyr::group_by(school_end, measurementscale) %>%
    dplyr::summarize(grade_low=min(grade_end),
              grade_high=max(grade_end),
              n_grades=1+grade_high-grade_low)

  # single grade schools
  single_grade_schools <- skeleton %>% 
    dplyr::filter(n_grades==1) %>%
    dplyr::select(school_end, measurementscale) %>%
    dplyr::inner_join(est_pctls,
               by=c("school_end", "measurementscale")) %>%
    dplyr::rename(school=school_end, grades_served=grade_end, N=N_students) %>%
    dplyr::mutate(grades_served=as.character(grades_served))  %>%
    dplyr::select(-c(fallwinterspring_start:fallwinterspring_end))
  
  
  #multi grade schools 
  multi_grade_schools <- skeleton %>% 
    dplyr::filter(n_grades>1) %>%
    dplyr::select(school_end, measurementscale) %>%
    dplyr::inner_join(est_pctls,
               by=c("school_end", "measurementscale")) %>%
    dplyr::rename(school=school_end) %>% 
    dplyr::group_by(school, measurementscale) %>%
    dplyr::summarize(
              grades_served=paste(unique(grade_end), collapse=" "),
              N=sum(N_students), 
              avg_rit_start=weighted.mean(avg_rit_start, N_students),
              avg_rit_end=plyr::round_any(weighted.mean(avg_rit_end, N_students), 0.1, ceiling),
              typical_growth_mean=round(weighted.mean(typical_growth_mean, N_students), 1)
              ) %>% 
    dplyr::inner_join(dplyr::filter(cps_constants, 
                                    variable=="sd_growth") %>%
                        dplyr::select(
                          measurementscale,
                          sd_growth=value),
                      by="measurementscale"
      ) %>%
    dplyr::mutate(growth=avg_rit_end-avg_rit_start,
           z_score=round(growth-typical_growth_mean,1)/sd_growth,
           growth_pctl = round(pnorm(z_score),2),
           growth_pctl = ifelse(growth_pctl>.99, .99, growth_pctl),
           growth_pctl = ifelse(growth_pctl<.01, .01, growth_pctl)
    )
  
  stacked <- dplyr::rbind_list(multi_grade_schools,
                      single_grade_schools)
  
  # return
  stacked
  
}


#' @title Applies CPS fall-to-prior-spring equating
#'
#' @description \code{equate_fall_to_spring} applies the function 
#' \code{cps_equate} on fall data that must be passed in and returns
#' a data frame with missing prior_spring test scores updated with
#' equated scores.  An `equated` indcator is added to the 
#' resulting data frame.
#'
#' @param growth_data season matched growth data frame
#' @param fall_data a data frame of fall data with columns  
#' `studentid`, `measurmentscale`, `grade`, and `testritscore`
#' 
#' @return data.frame with `nrow(growth_data)` rows and 
#' `ncol(growth_data) + 1` columns
#' 
equate_fall_to_spring <- function(growth_data=map_matched, 
                                  fall_data=NA){
  
  # id students with missing prior spring scores
  missing_prior_spring <- growth_data %>%
    dplyr::filter(is.na(testritscore_start)) %>%
    dplyr::select(studentid,
                  grade_start,
                  grade_end,
                  measurementscale, 
                  testritscore_end, 
                  testritscore_start)
  
  # match missing scores with existing fall scores and
  # impute prior spring
  matched<-missing_prior_spring %>%
    dplyr::inner_join(fall_data %>%
                        dplyr::rename(testritscore_fall=testritscore,
                       grade_fall=grade),
              by=c("studentid",
                   "measurementscale")) %>%
    dplyr::mutate(test_rit_score_equated=cps_equate(rit_score = testritscore_fall,
                                             subject=measurementscale,
                                              grade_level=grade_end),
                  testritscore_equated=round(test_rit_score_equated))
  
  
  out<-growth_data %>%
    dplyr::left_join(matched %>% 
                       select(studentid, 
                              measurementscale,
                              testritscore_equated),
                     by=c("studentid", 
                          "measurementscale")) %>%
    dplyr::mutate(equated=!is.na(testritscore_equated),
           testritscore_start=ifelse(equated,
                                     testritscore_equated,
                                     testritscore_start)
           ) %>%
    dplyr::select(-testritscore_equated)
    
  
  # return
  out
  
  
}



