context("School Growth Percentile")

#
data(ex_CombinedAssessmentResults)
data(ex_CombinedStudentsBySchool)
cdf<-ex_CombinedAssessmentResults %>%
  inner_join(ex_CombinedStudentsBySchool, by=c("StudentID", 
                                               "TermName")) %>%
  rename(schoolname=SchoolName.x)

names(cdf)<-tolower(names(cdf))

cdf_fall <- cdf %>% filter(grepl("Fall", termname))


#cdf_fall %>% mutate(equated_rit=cps_equate(testritscore, measurementscale, grade))

cdf_fall_filtered <- cdf_fall %>% filter(measurementscale %in% 
                                           c("Reading", "Mathematics"))

cdf_filtered <-  cdf %>% filter(measurementscale %in% 
                                       c("Reading", "Mathematics"),
                                     termname %in% c("Spring 2012-2013", "Spring 2013-2014"))

cdf_fall_filtered_equate<-cdf_fall_filtered %>%
  mutate(equated_rit=cps_equate(testritscore, 
                                measurementscale, 
                                grade
  )
  )


test_that("school_growth_percentiile fails gracefully", {

  expect_error(school_growth_percentile(cdf_fall_filtered),
                "You must have exactly two test terms")
  expect_error(school_growth_percentile(cdf),
               "Your data contains subjects besides Reading and Mathematics.")
    
})

test_that("school_growth_percentiile produces proper output", {

})
