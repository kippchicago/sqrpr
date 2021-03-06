% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sqrp_levels.R
\name{calc_sqrp_points}
\alias{calc_sqrp_points}
\title{Calculate sqrp points given given scores in each categor.}
\usage{
calc_sqrp_points(school_growth_pctl_reading = NULL,
  school_growth_pctl_mathematics = NULL,
  school_growth_pctl_aa_reading = NULL,
  school_growth_pctl_hisp_reading = NULL,
  school_growth_pctl_iep_reading = NULL,
  school_growth_pctl_ell_reading = NULL,
  school_growth_pctl_aa_mathematics = NULL,
  school_growth_pctl_hisp_mathematics = NULL,
  school_growth_pctl_iep_mathematics = NULL,
  school_growth_pctl_ell_mathematics = NULL,
  pct_exceed_typical_growth = NULL, school_attaninment_pct_2_reading = NULL,
  school_attaninment_pct_2_mathematics = NULL,
  school_attaninment_pct_38_reading = NULL,
  school_attaninment_pct_38_mathematics = NULL,
  pct_sufficient_access_progress = NULL, ada = NULL,
  mvms_5essentials = NULL, dqi = NULL)
}
\arguments{
\item{school_growth_pctl_reading}{school growth percentile in reading}

\item{school_growth_pctl_mathematics}{growth percentile in math}

\item{school_growth_pctl_aa_reading}{growth percentile in reading for African
American students}

\item{school_growth_pctl_hisp_reading}{growth percentile in reading for Latino students}

\item{school_growth_pctl_iep_reading}{growth percentile in reading for students with an IEP}

\item{school_growth_pctl_ell_reading}{growth percentile in reading for English language learners.}

\item{school_growth_pctl_aa_mathematics}{growth percentile in mathematics for African American
students}

\item{school_growth_pctl_hisp_mathematics}{growth percentile in mathematics for
Latino students}

\item{school_growth_pctl_iep_mathematics}{growth percentile in mathematics for students with an IEP}

\item{school_growth_pctl_ell_mathematics}{growth percentile in mathematics for English language learners}

\item{pct_exceed_typical_growth}{Percent of students meeting or exceeding typical growth}

\item{school_attaninment_pct_2_reading}{school level reading attainment percentile for grade 2}

\item{school_attaninment_pct_2_mathematics}{school level math attainment percentile for grade 2}

\item{school_attaninment_pct_38_reading}{school level reading attainment percentile for grades 3-8}

\item{school_attaninment_pct_38_mathematics}{school level math attainment percentile for grades 3-8}

\item{pct_sufficient_access_progress}{Percent of Access-taking students making sufficient progress}

\item{ada}{Average daily attendance rate for the school year}

\item{mvms_5essentials}{My Voice, My School 5Essentials rating}

\item{dqi}{Data Quality Index score}
}
\value{
SQPR Points
}
\description{
\code{calc_sqrp_points} calculates SQRP given performance in each
category.  Reweighting is automagically performed.
}
