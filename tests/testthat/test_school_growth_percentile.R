context("School Growth Percentile")

#seed

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
  select(studentid, measurementscale, testritscore, grade)

school_growth <- school_growth_percentile(cdf_filtered) 


school_growth_equated <- school_growth_percentile(cdf_filtered, 
                                                  fall_equate_scores = cdf_fall_filtered_equate)

test_that("school_growth_percentile fails gracefully", {

  expect_error(school_growth_percentile(cdf_fall_filtered),
                "You must have exactly two test terms")
  expect_error(school_growth_percentile(cdf),
               "Your data contains subjects besides Reading and Mathematics.")
  
  expect_warning(school_growth_percentile(cdf_filtered),
               "our data currently has 202 student-subject pairs")
  
})

test_that("school_growth_percentiile produces proper output", {
  expect_equal(length(school_growth), 4)
  expect_equal(names(school_growth), c("original_data", "student_level", "grade_level", "school_level"))
  expect_equal(nrow(school_growth$student_level), 1345)
  expect_equal(nrow(school_growth$grade_level), 12)
  expect_equal(nrow(school_growth$school_level), 6)
  expect_equal(round(mean(school_growth$grade_level$growth_pctl),4), 
               0.5725)
  
  expect_equal(round(mean(school_growth$school_level$growth_pctl),3), 
               0.615)
})

test_that("school_growth_percentile() figures proper school level percentiles" ,{
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
    
  #multi grade schools
  est_pctls<-school_growth$grade_level
  collapsed <- collapse_grade_to_school(est_pctls) %>%
    arrange(grades_served, school, measurementscale)
  
  # single grade schools
  est_pctls_single_grade<-school_growth$grade_level %>% filter(grade_end==5)
  collapsed_single_grade <- collapse_grade_to_school(est_pctls_single_grade) %>%
    arrange(grades_served, school, measurementscale)
  
  
  expect_equal(nrow(collapsed), 6)
  expect_equal(ncol(collapsed), 13)
  # reading
  expect_equal(as.numeric(collapsed[1,"growth_pctl"]), 0.93)
  expect_equal(as.numeric(collapsed[2,"growth_pctl"]), 0.86)
  expect_equal(as.numeric(collapsed[3,"growth_pctl"]), 0.86)
  expect_equal(as.numeric(collapsed[4,"growth_pctl"]), 0.01)
  expect_equal(as.numeric(collapsed[5,"growth_pctl"]), 0.97)
  expect_equal(as.numeric(collapsed[6,"growth_pctl"]), 0.06)
  
  
  # single grade schools 
  expect_equal(as.numeric(collapsed_single_grade[[1,"growth_pctl"]]), 0.86)
  expect_equal(as.numeric(collapsed_single_grade[[2,"growth_pctl"]]), 0.01)
  expect_equal(as.numeric(collapsed_single_grade[[3,"growth_pctl"]]), 0.99)
  expect_equal(as.numeric(collapsed_single_grade[[4,"growth_pctl"]]), 0.01)
  
})

test_that("school_growth_equated works when passing fall scores", {
  est_pctls<-school_growth_equated$grade_level %>% 
    ungroup %>%
    select(measurementscale, 
           grade_end, 
           avg_rit_start, 
           avg_rit_end, 
           growth_pctl) %>%
    arrange(desc(measurementscale),grade_end, avg_rit_start)
  
  expect_equal(sum(school_growth_equated$student_level$equated), 204)
  
  # reading
  expect_equal(as.numeric(est_pctls[1,"growth_pctl"]), 0.99)
  expect_equal(as.numeric(est_pctls[2,"growth_pctl"]), 0.01)
  expect_equal(as.numeric(est_pctls[3,"growth_pctl"]), 0.01)
  expect_equal(as.numeric(est_pctls[4,"growth_pctl"]), 0.01)
  expect_equal(as.numeric(est_pctls[5,"growth_pctl"]), 0.31)
  expect_equal(as.numeric(est_pctls[6,"growth_pctl"]), 0.01)
  
  # math
  expect_equal(as.numeric(est_pctls[7,"growth_pctl"]), 0.96)
  expect_equal(as.numeric(est_pctls[8,"growth_pctl"]), 0.86)
  expect_equal(as.numeric(est_pctls[9,"growth_pctl"]), 0.99)
  expect_equal(as.numeric(est_pctls[10,"growth_pctl"]), 0.78)
  expect_equal(as.numeric(est_pctls[11,"growth_pctl"]), 0.99)
  expect_equal(as.numeric(est_pctls[12,"growth_pctl"]), 0.99)
  
})




