context("School Attainment Percentile")


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

cdf_spring14 <-  cdf %>% filter(measurementscale %in% 
                                  c("Reading", "Mathematics"),
                                termname %in% c("Spring 2013-2014"))

test_that("school_attainment_percentile fails gracefully", {
  
  expect_error(school_attainment_percentile(cdf %>% 
                                              filter(measurementscale %in% 
                                                       c("Reading", "Mathematics"))),
               "You must have exactly one term")
  expect_error(school_attainment_percentile(cdf),
               "Your data contains subjects besides Reading and Mathematics.")
  
})
