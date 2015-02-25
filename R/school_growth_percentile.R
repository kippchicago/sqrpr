school_growth_percentile <- function(.data, 
                                     student_column="studentid",
                                     grade_column="grade", 
                                     subject_column="measurementscale", 
                                     rit_column="testritscore",
                                     term_column="termname",
                                     dl_indicator="iep", 
                                     ell_indicator="iep", 
                                     school_indicator="schoolname",
                                     growth_season="SS"){
  
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
    )
  
  #return
  map_matched
}
