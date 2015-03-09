context("SQRP levels")

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


test_that("get_sqrp_level calculations returms correct values.", {
  expect_equal(as.character(get_sqrp_level(1)), "3")
  expect_equal(as.character(get_sqrp_level(2)), "2")
  expect_equal(as.character(get_sqrp_level(3)), "2+")
  expect_equal(as.character(get_sqrp_level(3.5)), "1")
  expect_equal(as.character(get_sqrp_level(4)), "1+")
})

test_that("get_sqrp_level calculations returms correct values.", {
  recalced_points<-calc_sqrp_points(school_growth_pctl_reading=2,
                                    school_growth_pctl_mathematics=3,
                                    school_growth_pctl_aa_reading=2,
                                    school_growth_pctl_hisp_reading=NULL,
                                    school_growth_pctl_iep_reading=NULL,
                                    school_growth_pctl_ell_reading=NULL,
                                    school_growth_pctl_aa_mathematics=4,
                                    school_growth_pctl_hisp_mathematics=NULL,
                                    school_growth_pctl_iep_mathematics=NULL,
                                    school_growth_pctl_ell_mathematics=NULL,
                                    pct_exceed_typical_growth=3,
                                    school_attaninment_pct_2_reading=2,
                                    school_attaninment_pct_2_mathematics=3,
                                    school_attaninment_pct_38_reading=2,
                                    school_attaninment_pct_38_mathematics=3,
                                    pct_sufficient_access_progress=NULL,
                                    ada=3,
                                    mvms_5essentials=4,
                                    dqi=5)
  
  expect_equal(as.numeric(recalced_points), 2.9)
  
  
})

