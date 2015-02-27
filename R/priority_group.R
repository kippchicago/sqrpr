
priority_group<-function(sqrp_obj, 
                         group_column, 
                         group_id, 
                         sqrp_fun,
                         ...) {
  
  # check prvenacne of object
  assertthat::assert_that(inherits(sqrp_obj, 
                                   c("sqrp_attainment", "sqrp_growth")
                                   )
                          )
                          
  #create _end tag
  end_tag<-""
  if(inherits(sqrp_obj, "sqrp_growth")) end_tag<-"_end"
  
  #filter data on group_column and group_id
  filter_arg<-paste0("grepl('",group_id,"', ",group_column, end_tag,")")
  
  filtered <- sqrp_obj$student_level %>%
    dplyr::filter_(filter_arg)
  
  # calculate number of students at school in each measurement scale
  group_by_arg<-paste0("school", end_tag)
  
  mask<-filtered %>%
    dplyr::group_by_(group_by_arg,
                     "measurementscale") %>%
    dplyr::summarize(N=n()) %>%
    dplyr::filter(N>=30) %>%
    dplyr::select(-N)
  
  # drop students measurement scales with too few students (or keep
  # groups with enough students are kept) 
  filter_arg2<-paste0("grepl('",group_id,"', ",group_column,")")
  masked_data<-sqrp_obj$original_data %>%
    dplyr::inner_join(mask, 
                      by=c("measurementscale"="measurementscale",
                           "school"=paste0("school",end_tag
                                           )
                           )
                      ) %>%
    dplyr::filter_(filter_arg2)
  
  
  #build function arguments
  args_list <- list(.data=masked_data,
                    school_indicator="school",
                   ...
                    )
  
  #run data through function 
  out<-do.call(sqrp_fun, args_list)
                
  
  class(out)<-c("priority_group", class(out))
  
  out
  
 
  
  #return
  
}
