pacman::p_load(tidyverse,tidylog, shiny, flextable, magrittr,tmap,sf,DT)
#load Ghana school attendance data 3 years and above from the Ghana 2021 Census Literacy reports
gh_school_attend=read_csv("Step_up_education_in_Ghana/data/GHA_2021_Census_Pop_3yrs_school_attendance.csv")

gh_school_attend%<>%
  mutate(Percent_dropout=round(((gh_school_attend$Dropped_out_of_School/
                                           gh_school_attend$`Sum_of_Respondents_(3_years_and_above)`)
                                        *100),2),
         Percent_Currently_in_School=round(((gh_school_attend$Currently_Attending_School/
                                   gh_school_attend$`Sum_of_Respondents_(3_years_and_above)`)
                                *100),2),
         Percent_Never_Attended_School=round(((gh_school_attend$Never_Attended_School/
                                               gh_school_attend$`Sum_of_Respondents_(3_years_and_above)`)
                                            *100),2))

#rename Region to ADM1_EN so can share common functions
gh_school_attend%<>%
  mutate(across(c(ADM1_EN,Gender,Locality),factor))

