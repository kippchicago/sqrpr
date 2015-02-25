#' @title Ensure subjects only reading and mathematics
#' 
#' @description checks if `measurementscale` field only contains "Mathematics" or
#' "Reading"
#' 
ensure_subjects <- 
  ensurer::ensures_that(all(.$measurementscale %in% c("Reading", "Mathematics"))~
                          paste0("Your data contains subjects besides Reading and Mathematics. ",  
                                 "Try filtering it."
                                 )
  )

#' @title Ensure only two test terms in data
#' 
#' @description checks if `testterm` field has exactly two terms
#' 
ensure_two_seasons_only <- 
  ensurer::ensures_that(length(unique(.$termname))==2 ~
                          paste0("You must have exactly two test terms in your data.",
                                 "No more, no less."
                          )
  )
