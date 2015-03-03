#' @title Calculate sqrp level given sqrp points
#'
#' @description \code{get_sqrp_level} calculates SQRP level given SQRP points.
#' 
#' @param sqrp_points sqpr points
#' 
#' @export
#' 
#' @return  a single element factor vector


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


#' @title Calculate sqrp points given given scores in each categor.
#'
#' @description \code{calc_sqrp_points} calculates SQRP given performance in each
#' category.  Reweighting is automagically performed.
#' 
#' @param school_growth_pctl_reading school growth percentile in reading
#' @param school_growth_pctl_mathematics growth percentile in math
#' @param school_growth_pctl_aa_reading growth percentile in reading for African
#' American students
#' @param school_growth_pctl_hisp_reading growth percentile in reading for Latino students
#' @param school_growth_pctl_iep_reading growth percentile in reading for students with an IEP
#' @param school_growth_pctl_ell_reading growth percentile in reading for English language learners.
#' @param school_growth_pctl_aa_mathematics growth percentile in mathematics for African American
#' students
#' @param school_growth_pctl_hisp_mathematics growth percentile in mathematics for 
#' Latino students 
#' @param school_growth_pctl_iep_mathematics growth percentile in mathematics for students with an IEP
#' @param school_growth_pctl_ell_mathematics growth percentile in mathematics for English language learners
#' @param pct_exceed_typical_growth Percent of students meeting or exceeding typical growth
#' @param school_attaninment_pct_2_reading school level reading attainment percentile for grade 2
#' @param school_attaninment_pct_2_mathematics school level math attainment percentile for grade 2
#' @param school_attaninment_pct_38_reading school level reading attainment percentile for grades 3-8
#' @param school_attaninment_pct_38_mathematics school level math attainment percentile for grades 3-8
#' @param pct_sufficient_access_progress Percent of Access-taking students making sufficient progress
#' @param ada Average daily attendance rate for the school year
#' @param mvms_5essentials My Voice, My School 5Essentials rating
#' @param dqi Data Quality Index score
#' 
#' @export
#' 
#' @return  SQPR Points
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
  
  round(sqrp_points,1)
}


#' @title Calculate ADA points
#'
#' @description \code{calc_ada_points} calculates ADA points
#' 
#' @param ada ADA points
#' 
#' @export
#' 
#' @return  a single element numeric vector
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



#' @title Calculate growth percentile points
#'
#' @description \code{calc_growth_points} calculates growht points
#' 
#' @param growth_pctl growth percentile
#' 
#' @export
#' 
#' @return  a single element numeric vector

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



#' @title Calculate priority group growth percentile points
#'
#' @description \code{calc_priority_growth_points} calculates growht points
#' 
#' @param growth_pctl growth percentile
#' 
#' @export
#' 
#' @return  a single element numeric vector
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



#' @title Calculate priority typical growth percentage points
#'
#' @description \code{calc_me_growth_norms_points} calculates  points for meeting 
#' exceding typical groth 
#' 
#' @param pct percent meeting/exceeding growth
#' 
#' @export
#' 
#' @return  a single element numeric vector
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


#' @title Calculate priority attainment percentile points
#'
#' @description \code{calc_attainment_points} calculates  points for meeting 
#' exceding typical groth 
#' 
#' @param pct attainment percentile
#' 
#' @export
#' 
#' @return  a single element numeric vector
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



#' @title Calculate Access percentage points
#'
#' @description \code{calc_access_points} calculates  points for sufficient 
#' growth on Access
#' 
#' @param pct percentage of students making sufficient progress on Access
#' 
#' @export
#' 
#' @return  a single element numeric vector
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



#' @title Calculate 5Essentials points
#'
#' @description \code{calc_5essentials_points} calculates  points given 
#' 5Essentials Rating
#' @details `rating` must be a characteer vector with one of the following:
#' "WO" = Well Organized
#' "O" = Organized
#' "MO" = Mostly Organized
#' "PO" = Partially Organized
#' "NYO" = Not Yet Organized
#' 
#' @param rating 5Esstianls rating as character vectors:
#' 
#' @export
#' 
#' @return  a single element numeric vector

calc_5essentials_points <- function(rating){
  out<-switch(rating,
              "WO"="5",
              "O"="4",
              "MO"="3",
              "PO"="2",
              "NYO"="1"
              )
  
  as.integer(out)
}


#' @title Calculate DQI  points
#'
#' @description \code{calc_dqi_points} calculates  points given DQI score
#' 
#' @param pct percentage DQI score
#' 
#' @export
#' 
#' @return  a single element numeric vector
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

#' @title Calculate percent meets/exceeds typical growth 
#'
#' @description \code{calc_pct_me} calculates  typical growth percentage from 
#' an `sqrp_growth` object
#' 
#' @param growth an `sqrp_growth` object
#' 
#' @export
#' 
#' @return  a data frame
#' 
calc_pct_me<-function(growth){
  out<-growth$school_level %>%
    group_by(school, grades_served) %>%
    summarize(N=sum(N), 
              N_met=sum(N_met), 
              pct_met=round(N_met/N,2)
              )
  
  #return out
}


#' @title Wrapper around \code{\link{get_sqrp_level}} and \code{\link{calc_sqrp_points}}
#' 
#'
#' @description \code{sqrp_level} is a convenience wrapper that takes `sqrp_growth`
#' and `sqrp_attainment` objects, as well as raw scores for non-MAP categores and
#' a school name and returns a data frame containing that school's SQRP points 
#' and level. 
#' 
#' @param school_name character vector passed to `grepl` and `filter`
#' @param growth an `sqrp_growth` object
#' @param attain an `sqrp_attainment` object
#' @param growth_pg_aa a `pg_sqrp_attainment` object
#' @param growth_pg_iep a `pg_sqrp_attainment` object 
#' @param pct_me Percent meets/exceeds typical growth
#' @param access Percent making sufficient Access progress
#' @param ada Average daily attendence
#' @param mvms 5Essentials rating
#' @param dqi Data quality index
#' 
#' @export
#' 
#' @return  a data frame with SQRP points and Level

sqrp_level<-function(school_name="Ascend", 
                          growth,
                          attain,
                          growth_pg_aa,
                          growth_pg_iep,
                          pct_me,
                          access,
                          ada,
                          mvms,
                          dqi
){
  
  
  # create filter call
  filter_arg=sprintf("grepl('%s', school)", school_name)
  
  # get MAP related measures
  growth<-growth$school_level %>% filter_(filter_arg)
  attain<-attain$school_level %>% filter_(filter_arg)
  growth_pg_aa<-growth_pg_aa$school_level %>% filter_(filter_arg)
  growth_pg_iep<-growth_pg_iep$school_level %>% filter_(filter_arg)
  pct_me<-pct_me %>% filter_(filter_arg)
  
  # calculate pionts 
  gp_r<-growth %>%
    filter(measurementscale=="Reading") %>% 
    select(growth_pctl) %>%
    as.numeric %>%
    calc_growth_points
  
  gp_m<-growth %>% 
    filter(measurementscale=="Mathematics") %>% 
    select(growth_pctl) %>%
    as.numeric %>%
    calc_growth_points
  
  gp_aa_r<-growth_pg_aa %>% 
    filter(measurementscale=="Reading") %>% 
    select(growth_pctl) %>% 
    as.numeric %>%
    calc_priority_growth_points
  
  if(is.na(gp_aa_r)) gp_aa_r<-NULL
  
  gp_dl_r <- growth_pg_iep %>% 
    filter(measurementscale=="Reading") %>% 
    select(growth_pctl) %>%
    as.numeric %>%
    calc_priority_growth_points
  
  if(is.na(gp_dl_r)) gp_dl_r<-NULL
  
  gp_aa_m<-growth_pg_aa %>% 
    filter(measurementscale=="Mathematics") %>% 
    select(growth_pctl) %>%
    as.numeric %>%
    calc_priority_growth_points
  
  if(is.na(gp_aa_m)) gp_aa_m<-NULL
  
  gp_dl_m<-growth_pg_iep %>% 
    filter(measurementscale=="Mathematics") %>% 
    select(growth_pctl) %>%
    as.numeric %>%
    calc_priority_growth_points
  
  if(is.na(gp_dl_m)) gp_dl_m<-NULL
  
  pct_me <- pct_me$pct_met %>%
    as.numeric %>%
    calc_me_growth_norms_points
  
  attain_2_r <- attain  %>% 
    filter(measurementscale=="Reading",
           grades_served=="2") %>% 
    select(attainment_pctl) %>%
    as.numeric %>%
    calc_attainment_points
  
  if(is.na(attain_2_r)) attain_2_r<-NULL
  
  attain_2_m <- attain  %>% 
    filter(measurementscale=="Mathematics",
           grades_served=="2") %>% 
    select(attainment_pctl) %>%
    as.numeric %>%
    calc_attainment_points
  
  if(is.na(attain_2_m)) attain_2_m<-NULL
  
  attain_38_r <- attain  %>% 
    filter(measurementscale=="Reading",
           grades_served!="2") %>% 
    select(attainment_pctl) %>%
    as.numeric %>%
    calc_attainment_points
  
  attain_38_m <- attain  %>% 
    filter(measurementscale=="Mathematics",
           grades_served!="2") %>% 
    select(attainment_pctl) %>%
    as.numeric %>%
    calc_attainment_points
  
  if(!missing(access)){
    access<-calc_access_points(access)
  } else {
    access<-NULL
  }
  
  if(!missing(ada)){
    ada<-calc_ada_points(ada)
  } else {
    ada<-NULL
  }
  
  if(!missing(mvms)){
    mvms<-calc_5essentials_points(mvms)
  } else {
    mvms<-NULL
  }
  
  if(!missing(dqi)){
    dqi<-calc_dqi_points(dqi)
  } else {
    dqi <- NULL
  }
  
  args_list<-
    list(school_growth_pctl_reading=gp_r, 
         school_growth_pctl_mathematics=gp_m,
         school_growth_pctl_aa_reading=gp_aa_r, 
         school_growth_pctl_aa_mathematics=gp_aa_m,
         school_growth_pctl_iep_reading=gp_dl_r, 
         school_growth_pctl_iep_mathematics=gp_dl_m,
         pct_exceed_typical_growth=pct_me,
         school_attaninment_pct_2_reading=attain_2_r,
         school_attaninment_pct_2_mathematics=attain_2_m,
         school_attaninment_pct_38_reading=attain_38_r,
         school_attaninment_pct_38_mathematics=attain_38_m,
         pct_sufficient_access_progress=access,
         ada=ada,
         mvms_5essentials=mvms,
         dqi=dqi
    )
  
  sqrp_points<-do.call(calc_sqrp_points, args_list) %>%
    as.numeric
  
  sqrp_level<-get_sqrp_level(round(sqrp_points,1))
  
  out<-data.frame(points=sqrp_points,
                  level=sqrp_level)
  
  # return
  out
  
}

