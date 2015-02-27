#' @title Ensure subjects only reading and mathematics
#' 
#' @description checks if `measurementscale` field only contains "Mathematics" or
#' "Reading"
#' 
#' @param . the thing to be ensured
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
#' @param . the thing to be ensured
#' 
ensure_two_seasons_only <- 
  ensurer::ensures_that(length(unique(.$termname))==2 ~
                          paste0("You must have exactly two test terms in your data.",
                                 "No more, no less."
                          )
  )

#' @title Ensure only one test term in data
#' 
#' @description checks if `testterm` field has exactly two terms
#' 
#' @param . the thing to be ensured
#' 
ensure_one_seasons_only <- 
  ensurer::ensures_that(length(unique(.$termname))==1 ~
                          paste0("You must have exactly one term in your data.",
                                 "No more, no less."
                          )
  )


#' @title Ensure fall data has proper columns 
#' 
#' @description checks `names(.)` for correct columns
#' 
#' @param . the thing to be ensured
#' 
ensure_fall_data <- 
  ensurer::ensures_that(all(names(.)==c("studentid", 
                                    "measurementscale",
                                    "testritscore",     
                                    "grade")) ~
                          paste0("Fall data for equating must have columns:",
                                 " studentid, measurementscale, testritscore, and grade."
                          )
  )





