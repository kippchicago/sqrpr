#' @title Calculate growth percenitle
#' 
#'
#' @description \code{school_growth_percentile} is the workhourse function
#' that calculates school growth percentiles.
#' 
#' @details Currently this function (indeed, this package) only supports spring-to-spring calculations.  Since
#' CPS will equate your fall scores anyway, this function does support equating fall to prior spring scores. see the 
#' `fall_equate_score` parameter below for more details. 
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
#' @param truncate_growth whether or not to truncate student growth at 1st and 99th percentiles
#' Default is `TRUE`
#' @param fall_equate_scores either a data.frame of fall MAP results with columns `studentid`, 
#' `measurementscale`, `testritscore`, and `grade`, or `TRUE` if your equateding all fall scores
#' from fall-to-spring map data.  If a data.frame is passed, then these data are used to equate 
#' missing pre-test (i.e., prior-spring) RIT scores per CPS guidelines. IF `TRUE` is passed, then all fall scores
#' are equated to the prior spring
#' 
#' 
#' @return  list with four data frames attached
#' showing student-, grade-, and school-level growth percentiles 
#' as well as the original data. 
#'
#' @export  
school_growth_percentile <-  function(.data, 
                                     student_column="studentid",
                                     grade_column="grade", 
                                     subject_column="measurementscale", 
                                     rit_column="testritscore",
                                     term_column="termname",
                                     dl_indicator="iep", 
                                     ell_indicator="iep", 
                                     school_indicator="schoolname",
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
    
    season1<-"Spring"
  }

  
  if(season1=="Fall"){
    map_matched <- map_matched %>%
      dplyr::inner_join(norms_students_2011 %>% 
                          dplyr::select(measurementscale=MeasurementScale,
                                        grade_start=StartGrade,
                                        testritscore_start=StartRIT,
                                        typical_growth=T42,
                                        reported_growth=R42,
                                        std_dev_growth=S42
                          ) %>%
                          mutate(grade_start=as.numeric(grade_start)), 
                        by=c("measurementscale",
                             "grade_start",
                             "testritscore_start")
      ) %>%
      dplyr::mutate(growth=testritscore_end-testritscore_start,
                    met_typical=growth>=typical_growth,
                    cgi=(growth-typical_growth)/std_dev_growth,
                    growth_pctl=pnorm(cgi)
      ) 
  } else {
    map_matched <- map_matched %>%
      dplyr::inner_join(norms_students_2011 %>% 
                          dplyr::select(measurementscale=MeasurementScale,
                                        grade_start=StartGrade,
                                        testritscore_start=StartRIT,
                                        typical_growth=T22,
                                        reported_growth=R22,
                                        std_dev_growth=S22
                          ) %>%
                          mutate(grade_start=as.numeric(grade_start)), 
                        by=c("measurementscale",
                             "grade_start",
                             "testritscore_start")
      ) %>%
      dplyr::mutate(growth=testritscore_end-testritscore_start,
                    met_typical=growth>=typical_growth,
                    cgi=(growth-typical_growth)/std_dev_growth,
                    growth_pctl=pnorm(cgi)
      ) %>%
      dplyr::filter(grade_start!=grade_end)  
  }
  
  
  
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
                     avg_rit_end = round(mean(testritscore_final),1),
                     N_met=sum(met_typical),
                     pct_met=round(N_met/N_students,2)) %>%
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


#' @export
print.sqrp_growth <-function(x, ...){
  
  tbl<-x$school_level %>%
    dplyr::select_("School"="school",
           "Subject"="measurementscale",
           "Grades"="grades_served",
           "N",
           "Growth Pctl"="growth_pctl",
           "Pct M/E Typcial Growth" = "pct_met") %>%
    as.data.frame
  
  cat("Test message\n")
  cat("\n")
  
  print(tbl, row.names=FALSE)
  
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
#' @export
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
#' @export
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
    dplyr::filter(n_grades>1) 
  if(nrow(multi_grade_schools)>0) {
    
    multi_grade_schools <- multi_grade_schools %>%
      dplyr::select(school_end, measurementscale) %>%
      dplyr::inner_join(est_pctls,
                        by=c("school_end", "measurementscale")) %>%
      dplyr::rename(school=school_end) %>% 
      dplyr::group_by(school, measurementscale) %>%
      dplyr::summarize(
        grades_served=paste(unique(grade_end), collapse=" "),
        avg_rit_start=round(weighted.mean(avg_rit_start, N_students),1),
        avg_rit_end=plyr::round_any(weighted.mean(avg_rit_end, N_students), 0.1, ceiling),
        typical_growth_mean=round(weighted.mean(typical_growth_mean, N_students), 1),
        N=sum(N_students), 
        N_met=sum(N_met),
        pct_met=round(N_met/N,2)
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
  }  else {
    stacked <- single_grade_schools
  }
  
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
#' @export
equate_fall_to_spring <- function(growth_data=map_matched, 
                                  fall_data=NA){
  
  
  fall_first_season <- any(growth_data$fallwinterspring_start=="Fall", na.rm = TRUE)
  
  if(fall_first_season){
    
    message(paste("Detected first season as fall and equate_all_fall==TRUE.\n",
                  "Equating ALL fall scores to prior spring scores")
            )
    
    out <- growth_data %>%
      dplyr::mutate(test_rit_score_equated=cps_equate(rit_score = testritscore_start,
                                                      subject=measurementscale,
                                                      grade_level=grade_end
                                                      ),
                    testritscore_equated=round(test_rit_score_equated),
                    season="Spring",
                    testpercentile_equated=get_test_percentile(measurementscale=measurementscale,
                                                               season=season,
                                                               grade_level=grade_start-1,
                                                               ritscore=testritscore_equated
                                                               ),
                    equated=!is.na(testritscore_equated),
                    testritscore_start=ifelse(equated,
                                              testritscore_equated,
                                              testritscore_start),
                    testpercentile_start=ifelse(equated,
                                                testpercentile_equated,
                                                testpercentile_start),
                    grade_start=ifelse(equated, grade_end-1, grade_start),
                    fallwinterspring_start="Spring",
                    map_year_academic_start=map_year_academic_end-1,
                    termname_start=sprintf("%s %s-%s", 
                                           fallwinterspring_start,
                                           map_year_academic_start,
                                           map_year_academic_start+1)
                    
      ) %>%
      dplyr::select(-testritscore_equated, -testpercentile_equated)
    
    # return
    out
      
    
  } else {
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
                                                      grade_level=grade_end 
      ),
      testritscore_equated=round(test_rit_score_equated),
      season="Spring",
      testpercentile_equated=get_test_percentile(measurementscale=measurementscale,
                                                 season=season,
                                                 grade_level=grade_fall-1,
                                                 ritscore=testritscore_equated
      )
      )
    
    
    out<-growth_data %>%
      dplyr::left_join(matched %>% 
                         select(studentid, 
                                measurementscale,
                                testritscore_equated,
                                testpercentile_equated),
                       by=c("studentid", 
                            "measurementscale")) %>%
      dplyr::mutate(equated=!is.na(testritscore_equated),
                    testritscore_start=ifelse(equated,
                                              testritscore_equated,
                                              testritscore_start),
                    testpercentile_start=ifelse(equated,
                                                testpercentile_equated,
                                                testpercentile_start),
                    grade_start=ifelse(equated, grade_end-1, grade_start),
                    fallwinterspring_start="Spring",
                    map_year_academic_start=map_year_academic_end-1,
                    termname_start=sprintf("%s %s-%s", 
                                           fallwinterspring_start,
                                           map_year_academic_start,
                                           map_year_academic_start+1)
                    
      ) %>%
      dplyr::select(-testritscore_equated, -testpercentile_equated)
    
    # return
    out
    
  }
    
  
  
}


#' @title Finds student national percentile rank 
#'
#' @description \code{get_test_percentile} looks up a student's national 
#' percentile rank from \code{student_status_norms_2011_dense_extended}.
#'
#' @param measurementscale the test subject.
#' @param season the test season, which is one of "Fall", "Winter", or "Spring".
#' @param grade_level the grade level at the time of the assessment
#' @param ritscore the test RIT score for which you want to know the test percentile. 
#' 
#' @return an integer vector
#' 
#' @export
#' 
get_test_percentile <- function(measurementscale, season, grade_level, ritscore){
  x<-data.frame(measurementscale=measurementscale, 
                fallwinterspring=season,
                grade=grade_level,
                RIT=ritscore)
  joined <- dplyr::left_join(x, 
                             student_status_norms_2011,
                             by=c("measurementscale",
                                  "fallwinterspring",
                                  "grade",
                                  "RIT")
  ) %>%
    dplyr::group_by(measurementscale, fallwinterspring, grade, RIT) %>%
    ungroup %>% 
    dplyr::select(percentile) %>% 
    unlist %>%
    as.integer
  
  # return
  joined
}


