



get_sqrp_level <- function(sqrp_points){
  cut(sqrp_points, breaks = c(0,
                              2,
                              3,
                              3.5,
                              4,
                              5),
      labels = c("3", 
                 "2", 
                 "2+", 
                 "1", 
                 "1+"),
      right=FALSE,
      ordered_result=TRUE
      )
}

calc_sqrp_points <- function(school_growth_pctl_reading=NULL,
                 school_growth_pctl_mathematics=NULL,
                 school_growth_pctl_aa_reading=NULL,
                 school_growth_pctl_hisp_reading=NULL,
                 school_growth_pctl_iep_reading=NULL,
                 school_growth_pctl_ell_reading=NULL,
                 school_growth_pctl_aa_mathematics=NULL,
                 school_growth_pctl_hisp_mathematics=NULL,
                 school_growth_pctl_iep_mathematics=NULL,
                 school_growth_pctl_ell_mathematics=NULL,
                 pct_exceed_typical_growth=NULL,
                 school_attaninment_pct_2_reading=NULL,
                 school_attaninment_pct_2_mathematics=NULL,
                 school_attaninment_pct_38_reading=NULL,
                 school_attaninment_pct_38_mathematics=NULL,
                 pct_sufficient_access_progress=NULL,
                 ada=NULL,
                 mvms_5essentials=NULL,
                 dqi=NULL
                 ){
  
  args_used<- c(as.list(environment()))
  
  #args_used_names<-names(args_used)
  
  args_df<-as.data.frame(unlist(args_used))
  names(args_df)<-"sqrp_points"
  args_df <- args_df %>%
    mutate(category=rownames(args_df)) %>%
    inner_join(sqrp_weights %>%
                 select(category, weight), 
               by="category")
  
  #get missing ready in order to reweight
  
  missing_cats<-anti_join(sqrp_weights, 
                          args_df,
                          by="category")
  
  
  
  if("no_rating" %in% missing_cats$reallocate_to){
    necessary_cats<-missing_cats %>%  
      filter(reallocate_to=="no_rating")
    
    necessary_cats<- paste(necessary_cats$category, collapse="\n\n")
    
    return(message(paste("Can't calculate SQRP without the following categories:\n \n",
                         necessary_cats
                         )
                   )
    )
  } 
  
  add_weights <- missing_cats %>%
    dplyr::select(category=reallocate_to, weight) %>%
    dplyr::group_by(category) %>% 
    dplyr::summarize(addl_weight=sum(weight))
  
  reweighted <- args_df %>%
    dplyr::left_join(add_weights, 
                      by="category")
    
  sqrp_points<-reweighted %>% 
    dplyr::mutate(new_weight=ifelse(is.na(addl_weight), weight, weight+addl_weight),
           weighted_points=sqrp_points*new_weight) %>%
    dplyr::summarize(sqrp_points=sum(weighted_points))
  
  sqrp_points
}

calc_ada_points <- function(ada){
  out<-cut(ada, breaks = c(0,
                              .92,
                              .94,
                              .95,
                              .96,
                              1),
      labels = c(1,2,3,4,5),
      right=FALSE,
      ordered_result=TRUE
  )
  
  as.integer(out)
}

calc_growth_points <- function(growth_pctl){
  out<-cut(growth_pctl, breaks = c(0,
                           .10,
                           .40,
                           .70,
                           .9,
                           1),
           labels = c(1,2,3,4,5),
           right=FALSE,
           ordered_result=TRUE
  )
  
  as.integer(out)
}

calc_priority_growth_points <- function(growth_pctl){
  out<-cut(growth_pctl, breaks = c(0,
                                   .10,
                                   .30,
                                   .50,
                                   .7,
                                   1),
           labels = c(1,2,3,4,5),
           right=FALSE,
           ordered_result=TRUE
  )
  
  as.integer(out)
}

calc_me_growth_norms_points <- function(pct){
  out<-cut(pct, breaks = c(0,
                                   .40,
                                   .50,
                                   .60,
                                   .70,
                                   1),
           labels = c(1,2,3,4,5),
           right=FALSE,
           ordered_result=TRUE
  )
  
  as.integer(out)
}

calc_attainment_points <- function(pct){
  out<-cut(pct, breaks = c(0,
                           .10,
                           .40,
                           .70,
                           .90,
                           1),
           labels = c(1,2,3,4,5),
           right=FALSE,
           ordered_result=TRUE
  )
  
  as.integer(out)
}


calc_access_points <- function(pct){
  out<-cut(pct, breaks = c(0,
                           .25,
                           .35,
                           .45,
                           .55,
                           1),
           labels = c(1,2,3,4,5),
           right=FALSE,
           ordered_result=TRUE
  )
  
  as.integer(out)
}



calc_5essentials_points <- function(pct){
  out<-switch(pct,
              "WO"="5",
              "O"="4",
              "MO"="3",
              "PO"="2",
              "NYO"="1"
              )
  
  as.integer(out)
}

calc_dqi_points <- function(pct){
  out<-cut(pct, breaks = c(0,
                           .85,
                           .90,
                           .95,
                           .99,
                           1),
           labels = c(1,2,3,4,5),
           right=FALSE,
           ordered_result=TRUE
  )
  
  as.integer(out)
}
