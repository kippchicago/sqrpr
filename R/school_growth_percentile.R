#' @title Calculate gorwth percenitle
#'
#' @description \code{school_growth_percentile} is the workhourse function
#' that calculates school growth percentiles
#'
#' @param student_column column that uniquely identifies students with
#' respect to subject and term
#' @param .data a long format (i.e., combined) Comprehensive Data File
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
#' 
#' @return data.frame with school level growth percentiles
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
                                     truncate_growth=TRUE){
  
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
  
  map_matched <- map_matched[,cols_reorderd] %>%
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
    
  
  
  # package up data
  out<-list(student_data=map_matched, 
            grade_level_data=map_matched_final)
  
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
