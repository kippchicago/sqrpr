context("Priority Group Metrics")


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

cdf_filtered <-  cdf %>% filter(measurementscale %in% 
                                  c("Reading", "Mathematics"),
                                termname %in% c("Spring 2012-2013", "Spring 2013-2014"))

cdf_fall_filtered_equate<-cdf_fall_filtered %>%
  select(studentid, measurementscale, testritscore, grade)


school_growth <- school_growth_percentile(cdf_filtered) 



school_attainment <- school_attainment_percentile(cdf_spring14)


test_that("priority_group works with growth data", {
  
  pg_growth<-priority_group(school_growth, 
                            "studentethnicgroup", 
                            "Hispanic or Latino")
  
  expect_equal(names(pg_growth), names(school_growth))
  expect_equal(length(pg_growth), length(school_growth))
  expect_equal(round(mean(pg_growth$school_level$growth_pctl),2),
               0.55)  
  

  
})

test_that("NULLS passed as arguments or too few students return NULL", {
  expect_null(priority_group(school_growth,  
                             group_column = NULL,
                             group_id = "Hispanic or Latino"))
  
  expect_null(priority_group(school_growth,  
                             group_column = "studentethnicgroup",
                             group_id = "Black or African American"))
})


test_that("priority_group works with attainment data", {
  
   pg_attain<-priority_group(school_attainment, 
                             "studentethnicgroup", 
                             "Hispanic or Latino")
  
   expect_equal(names(pg_attain), names(school_attainment))
   expect_equal(length(pg_attain), length(school_attainment))
   expect_equal(round(mean(pg_attain$school_level$attainment_pctl),3),
                0.715)  

   
  
})
