context("School Growth Percentile")

#seed
seed <- 1896
set.seed(seed)

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

school_growth <- school_growth_percentile(cdf_filtered) 

test_that("school_growth_percentiile fails gracefully", {

  expect_error(school_growth_percentile(cdf_fall_filtered),
                "You must have exactly two test terms")
  expect_error(school_growth_percentile(cdf),
               "Your data contains subjects besides Reading and Mathematics.")
    
})

test_that("school_growth_percentiile produces proper output", {
  expect_equal(length(school_growth), 3)
  expect_equal(names(school_growth), c("student_level", "grade_level", "school_level"))
  expect_equal(nrow(school_growth$student_level), 1345)
  expect_equal(nrow(school_growth$grade_level), 12)
  expect_equal(nrow(school_growth$school_level), 6)
  expect_equal(round(mean(school_growth$grade_level$growth_pctl),4), 
               0.5725)
  
  expect_equal(round(mean(school_growth$school_level$growth_pctl),3), 
               0.615)
})

test_that("school_growth_percentile() figures proper school level perentiles" ,{
  est_pctls<-school_growth$grade_level %>% 
    ungroup %>%
    select(measurementscale, 
           grade_end, 
           avg_rit_start, 
           avg_rit_end, 
           growth_pctl) %>%
    arrange(desc(measurementscale),grade_end, avg_rit_start)
  
  # reading
  expect_equal(as.numeric(est_pctls[1,"growth_pctl"]), 0.99)
  expect_equal(as.numeric(est_pctls[2,"growth_pctl"]), 0.01)
  expect_equal(as.numeric(est_pctls[3,"growth_pctl"]), 0.01)
  expect_equal(as.numeric(est_pctls[4,"growth_pctl"]), 0.02)
  expect_equal(as.numeric(est_pctls[5,"growth_pctl"]), 0.31)
  expect_equal(as.numeric(est_pctls[6,"growth_pctl"]), 0.01)
  
  # math
  expect_equal(as.numeric(est_pctls[7,"growth_pctl"]), 0.96)
  expect_equal(as.numeric(est_pctls[8,"growth_pctl"]), 0.86)
  expect_equal(as.numeric(est_pctls[9,"growth_pctl"]), 0.99)
  expect_equal(as.numeric(est_pctls[10,"growth_pctl"]), 0.73)
  expect_equal(as.numeric(est_pctls[11,"growth_pctl"]), 0.99)
  expect_equal(as.numeric(est_pctls[12,"growth_pctl"]), 0.99)
})

test_that("collapse_grade_to_school() collapses everthing just fine" ,{
  est_pctls<-school_growth$grade_level
  collapsed <- collapse_grade_to_school(est_pctls) %>%
    arrange(grades_served, school, measurementscale)
  
  expect_equal(nrow(collapsed), 6)
  expect_equal(ncol(collapsed), 11)
  # reading
  expect_equal(as.numeric(collapsed[1,"growth_pctl"]), 0.93)
  expect_equal(as.numeric(collapsed[2,"growth_pctl"]), 0.86)
  expect_equal(as.numeric(collapsed[3,"growth_pctl"]), 0.86)
  expect_equal(as.numeric(collapsed[4,"growth_pctl"]), 0.01)
  expect_equal(as.numeric(collapsed[5,"growth_pctl"]), 0.97)
  expect_equal(as.numeric(collapsed[6,"growth_pctl"]), 0.06)
  
  
  
})

