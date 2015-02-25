context("CPS Adjustments")

#
data(ex_CombinedAssessmentResults)
data(ex_CombinedStudentsBySchool)
cdf<-ex_CombinedAssessmentResults %>%
  inner_join(ex_CombinedStudentsBySchool, by=c("StudentID", 
                                               "TermName"))
names(cdf)<-tolower(names(cdf))

cdf_fall <- cdf %>% filter(grepl("Fall", termname))

#cdf_fall %>% mutate(equated_rit=cps_equate(testritscore, measurementscale, grade))

cdf_fall_filtered <- cdf_fall %>% filter(measurementscale %in% 
                                           c("Reading", "Mathematics"))

cdf_fall_filtered_equate<-cdf_fall_filtered %>%
  mutate(equated_rit=cps_equate(testritscore, 
                                measurementscale, 
                                grade
                           )
         )


test_that("cps equate performs properly", {
  expect_equal(round(cps_equate(175, "Reading", 6),4), 183.5925)
  expect_equal(round(cps_equate(c(190, 175), c("Mathematics", "Reading"), c(6,7)),4),
               c(195.4007, 183.0604))
  
  expect_error(mutate(cdf_fall, 
                      equated_rit=cps_equate(testritscore, 
                                             measurementscale, 
                                             grade
                                             )
                      )
               )
  
  expect_equal(nrow(cdf_fall_filtered),
               nrow(cdf_fall_filtered_equate)
               )
  
  expect_equal(mean(cdf_fall_filtered$testritscore),
               mean(cdf_fall_filtered_equate$testritscore))
  
  expect_equal(round(mean(cdf_fall_filtered_equate$equated_rit),4),
               219.4473)
  
})
