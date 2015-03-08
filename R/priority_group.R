#' @title Recalculate school growth or attainment precentiles for 
#' priority groups.
#'
#' @description \code{priority_group} recalculates SQRP school growth
#' or attainment percentiles for priority groups for an \code{sqrp_growth}
#' or \code{sqrp_attainment} attainment objects.  The function subsets 
#' the original data by the priority group and then reruns 
#' \code{\link{school_growth_percentile}} or 
#' \code{\link{school_growth_percentile}}, depending on the class of 
#' \code{sqrp_obj}
#' 
#'
#' @param sqrp_obj either an \code{sqrp_growth} or \code{sqrp_attainment}
#' object create by running #' \code{\link{school_growth_percentile}} or 
#' \code{\link{school_growth_percentile}}, respectivley, on NWEA MAP data.
#' @param group_column  a single element character vector with the 
#' name of the column in the original data that identifies priority 
#' group membership.
#' @param group_id a single element character vector of the code in 
#' \code{grou_column} that identifites whether ro not a student is 
#' a member of the priority group of interest
#' @param ...  other arguments passed to either 
#' \code{\link{school_growth_percentile}} or 
#' \code{\link{school_growth_percentile}} 
#' 
#' @return  an \code{sqrp_growth} and \code{priority_group_growth} 
#' or \code{sqrp_attainment} and \code{priority_group_attain}  
#'
#' @export 
priority_group <- function(sqrp_obj, 
                           group_column, 
                           group_id, 
                           ...) {
  UseMethod("priority_group")
}

#' @export 
priority_group.sqrp_growth<-function(sqrp_obj, 
                         group_column, 
                         group_id,
                         ...) {

  if(group_column %>% is.null) {
    out <- NULL
    return(out)
  }
  #create _ends tag
  end_tag<-"_end"
  
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
  
  if(nrow(masked_data)>0) {
    #build function arguments
    args_list <- list(.data=masked_data,
                      school_indicator="school",
                      ...
    )
    
    #run data through function 
    out<-do.call(school_growth_percentile, args_list)
    
    
    class(out)<-c("priority_group_growth", class(out))

  } else {
    out<-NULL
  }
    #return
  out
  
  }

#' @export 
priority_group.sqrp_attainment<-function(sqrp_obj, 
                                     group_column, 
                                     group_id,
                                     ...) {
  
  
  #filter data on group_column and group_id
  filter_arg<-paste0("grepl('",group_id,"', ",group_column,")")
  
  filtered <- sqrp_obj$student_level %>%
    dplyr::filter_(filter_arg)
  
  # calculate number of students at school in each measurement scale  
  mask<-filtered %>%
    dplyr::group_by_("school",
                     "measurementscale") %>%
    dplyr::summarize(N=n()) %>%
    dplyr::filter(N>=30) %>%
    dplyr::select(-N)
  
  # drop students measurement scales with too few students (or keep
  # groups with enough students are kept) 
  filter_arg2<-paste0("grepl('",group_id,"', ",group_column,")")
  masked_data<-sqrp_obj$student_level %>%
    dplyr::inner_join(mask, 
                      by=c("measurementscale",
                           "school")
    ) %>%
    dplyr::filter_(filter_arg2)
  
  
  #build function arguments
  args_list <- list(.data=masked_data,
                    school_indicator="school",
                    ...
  )
  
  #run data through function 
  out<-do.call(school_attainment_percentile, args_list)
  
  
  class(out)<-c("priority_group_attain", class(out))
  
  out
  
  
  
  #return
  
}
