##Brian Redline 
# LML person-level survey data management 

library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(haven)

version_one_bl_filepath <- "/Users/brianredline/University of Southern California/LogMyLife Project - Documents/Data/Person Level/_Raw Data/lml baseline v1.sav"

generate_version_one <- function(filepath){
  return_data <- read_sav(filepath) %>%
    rename_all(funs(tolower(.))) %>%
    filter(responseid != "R_1myHhk3o8mAi0GY" &
             responseid != "R_eaPtOykJQhKLnOh")
  return(return_data)
}
test <- generate_version_one(version_one_bl_filepath)
  
processing_function_version_one <- function() {
  #drop observations:
    #test response(s) 
      drop if RESPONSEID=="R_1myHhk3o8mAi0GY"
    #original 1014 response ***(need to decide if we want to keep the one from the second time he did it... will keep for now)***
      drop if RESPONSEID=="R_eaPtOykJQhKLnOh"
  #combine 1004's two partial responses into one obs. (use 6/9 response for beginning-MHC_NEEDCURR and 6/19 response for DERS through end of BL)  
      (originally I did this w/ copy/paste in raw csv file. Eldin: please go ahead and do this a more automated way if it makes sense)
        #drop if RESPONSEID=="R_72GUjPfXIWKYOlP" <- this is one of the two responeses for 1004
  }  

adapter_function_version_one <- function() {
  #drop variables not in version two (currently includes ones in all versions)
    recipientlastname
    recipientfirstname
    recipientemail 
    externalreference 
    site 
    literacyscreen 
    consentcomp 
    consent 
    desrim_exps_0-discrim_reasons_12 
    lifesex_online-mosex_online_prop 
    exchsex_ever exchsex_ever_forwhat 
    exchsex_ever_forced 
    exchsex_3mo_app 
    q525
  #create variables in version two
    site_unhoused site_housed
  #replace values for those variables not in version one
    replace site_housed=1 if pid==1010 | pid==1011
    replace site_housed=2 if pid==1001 | pid==1002 | pid==1003 | pid==1004 | pid==1005 | pid==1013 | pid==1014 
    replace site_unhoused=1 if pid==2001 | pid==2002 | pid==2003 | pid==2004 | pid==2005 | pid==2006
  #rename vars 
    rename pid to pid_bl
    gen baseline_version = 1
    rename STI_POS_0 STI_POS_88
    rename STI_POS_9 STI_POS_0
    rename CELL_CHARGE_ACCESS_10 CELL_CHARGE_ACCESS_77
  #*CHRONICDX_* CHANGES FROM ORIGINAL VERSION TO 7/11 VERSION: wording of respiratory item (same content); thyroid was there as 24 insead of 22; "something else not listed" was in as 22 not 23*/
    rename CHRONICDX_22 CHRONICDX_23
    rename CHRONICDX_24 CHRONICDX_22
  #/* ORIGINAL VERSION DOESN'T HAVE MH_CURRENT ACROSS ALL CONDITIONS. INSTEAD HAS THE FOLLOWING FOLLOW-UP'S FOR EACH ENDORSED MH CONDITION: lab var MH_CURRENT "Past 30day extent of issues w/ mental health"*/
    figure out what to do here
  #NEED TO DECIDE WHAT TO DO W/ STRESS ON THE STREETS FOR THIS VERSION: participants got a check all for the following statements then marked the extent of stress caused by those checked items afterwards
    foreach var of varlist STRESS_STREETS_1-STRESS_STREETS_15 {
      replace `var' = 888 if `var'==1
      replace `var' = 1 if `var'==. 
    }
      replace STRESS_STREETS_1=STRESS_SCALE_1 if STRESS_STREETS_1==888
      replace STRESS_STREETS_2=STRESS_SCALE_2 if STRESS_STREETS_2==888
      replace STRESS_STREETS_3=STRESS_SCALE_3 if STRESS_STREETS_3==888
      replace STRESS_STREETS_4=STRESS_SCALE_4 if STRESS_STREETS_4==888
      replace STRESS_STREETS_5=STRESS_SCALE_5 if STRESS_STREETS_5==888
      replace STRESS_STREETS_6=STRESS_SCALE_6 if STRESS_STREETS_6==888
      replace STRESS_STREETS_7=STRESS_SCALE_7 if STRESS_STREETS_7==888
      replace STRESS_STREETS_8=STRESS_SCALE_8 if STRESS_STREETS_8==888
      replace STRESS_STREETS_9=STRESS_SCALE_9 if STRESS_STREETS_9==888
      replace STRESS_STREETS_10=STRESS_SCALE_10 if STRESS_STREETS_10==888
      replace STRESS_STREETS_11=STRESS_SCALE_11 if STRESS_STREETS_11==888
      replace STRESS_STREETS_12=STRESS_SCALE_12 if STRESS_STREETS_12==888
      replace STRESS_STREETS_13=STRESS_SCALE_13 if STRESS_STREETS_13==888
      replace STRESS_STREETS_14=STRESS_SCALE_14 if STRESS_STREETS_14==888
      replace STRESS_STREETS_15=STRESS_SCALE_15 if STRESS_STREETS_15==888
      drop STRESS_STREETS_16
      lab var STRESS_STREETS_1 "How much stress did you feel about the following in the past month: FINDING ENOUGH FOOD TO EAT"
      lab var STRESS_STREETS_2 "How much stress did you feel about the following in the past month: GETTING ALONG WITH FRIENDS"
      lab var STRESS_STREETS_3 "How much stress did you feel about the following in the past month: BEING ARRESTED"
      lab var STRESS_STREETS_4 "How much stress did you feel about the following in the past month: BEING UNABLE TO FIND WORK"
      lab var STRESS_STREETS_5 "How much stress did you feel about the following in the past month: BEING HIT/KICKED/PUNCHED"
      lab var STRESS_STREETS_6 "How much stress did you feel about the following in the past month: FINDING A PLACE TO SLEEP"
      lab var STRESS_STREETS_7 "How much stress did you feel about the following in the past month: GETTING PROFESSIONAL HELP FOR A HEALTH PROBLEM"
      lab var STRESS_STREETS_8 "How much stress did you feel about the following in the past month: BEING TREATED BADLY BY THE REST OF SOCIETY"
      lab var STRESS_STREETS_9 "How much stress did you feel about the following in the past month: HAVING A PURPOSE FOR MY LIFE"
      lab var STRESS_STREETS_10 "How much stress did you feel about the following in the past month: GETTING MORE EDUCATION"
      lab var STRESS_STREETS_11 "How much stress did you feel about the following in the past month: FINDING A PLACE TO TAKE A BATH OR SHOWER"
      lab var STRESS_STREETS_12 "How much stress did you feel about the following in the past month: FINDING A PLACE TO WASH MY CLOTHES"
      lab var STRESS_STREETS_13 "How much stress did you feel about the following in the past month: FINDING OTHER PEOPLE TO HANG OUT WITH"
      lab var STRESS_STREETS_14 "How much stress did you feel about the following in the past month: BEING SEXUALLY ASSAULTED"
      lab var STRESS_STREETS_15 "How much stress did you feel about the following in the past month: EARNING MONEY"
      lab def SOTS_LAB 1"None at all" 2"A little" 3"More than a little" 4"A lot" -99"Seen but not answered"
      lab val STRESS_STREETS_* SOTS_LAB
      drop STRESS_SCALE_1-STRESS_SCALE_15
    
    
  #Come back to this: was for recoding values but don't need if we're using strings
    recode firsthomeless (43=1) (44=2) (45=3) (46=4) (47=5) (48=6) (49=7) (50=8) (51=9) (51=10) (53=11) (54=12) (55=13) (56=14) (57=15) (58=16) (59=17) (60=18) (61=19) (62=20) (63=21) (64=22) (65=23) (66=24) (67=25)
    recode EMPLOY_TTLPAIDHRS (2=1) (3=2) (4=3) (5=4) (6=5) (7=6) (8=7) (9=8) (10=9) (11=10) (12=11) (13=12) (14=13) (15=14) (16=15) (17=16) (18=17) (19=18) (20=19) (21=20) (22=21)
    recode check-all categories for romrel_sex items
      rename ROMREL_SEX_7 ROMREL_SEX_G
      rename ROMREL_SEX_3 ROMREL_SEX_C
      rename ROMREL_SEX_4 ROMREL_SEX_D
      rename ROMREL_SEX_5 ROMREL_SEX_E
      rename ROMREL_SEX_6 ROMREL_SEX_F
      rename ROMREL_SEX_G ROMREL_SEX_3
      rename ROMREL_SEX_C ROMREL_SEX_4
      rename ROMREL_SEX_D ROMREL_SEX_5
      rename ROMREL_SEX_E ROMREL_SEX_6
      rename ROMREL_SEX_F ROMREL_SEX_7
      order ROMREL_SEX_3, after(ROMREL_SEX_2)
      lab var ROMREL_SEX_0 "Sex behavior w/ partner: no sexual relatnioship selected"
      lab var ROMREL_SEX_1 "Sex behavior w/ partner: monogamous throughout relationship selected"
      lab var ROMREL_SEX_2 "Sex behavior w/ partner: have had period(s) of monogamy selected"
      lab var ROMREL_SEX_3 "Sex behavior w/ partner: have had sex with other people TOGETHER (e.g., 3-ways) selected"
      lab var ROMREL_SEX_4 "Sex behavior w/ partner: I have sex with other people selected"
      lab var ROMREL_SEX_5 "Sex behavior w/ partner: Partner(s) have sex with other people selected"
      lab var ROMREL_SEX_6 "Sex behavior w/ partner: Don't ask-don't tell about sex with others selected"
      lab var ROMREL_SEX_7 "Sex behavior w/ partner: I've played around/cheated on partner selected"
      lab var ROMREL_SEX_8 "Sex behavior w/ partner: Partner has played around/cheated on me selected"
    #fix ttlincome_employ and ttlimcome_informal to match future versions:  
      #TTLINCOME_EMPLOY in this version skipped "$2,251-$2,500" so need to recode for those after. (no participants reported incomes near this range so not a big deal*/
        lab var TTLINCOME_EMPLOY "Total monthly income from all paid jobs"
        recode TTLINCOME_EMPLOY (10=11) (11=12) (12=13)
        lab def INCOME_LAB 1"Less than $250" 2"$251-$500" 3"$501-$750" 4"$751-$1000" 5"$1001-$1250" 6"$1251-$1500" 7"$1501-$1750" 8"$1751-$2000" 9"$2001-$2250" 10"$2251-$2500" 11"$2501-$2750" 12"$2751-$3000" 13"More than $3000"
        lab val TTLINCOME_EMPLOY INCOME_LAB
      #same change as ttlincome_employ for ttlincome_informal (NEED TO DOUBLE CHECK IF WE NEED TO DO ANYTHING WITH 1013'S RESPONSES SINCE HE RESPONDED W/ RECODED VALUE*/)
        lab var TTLINCOME_INFORMAL "Total monthly informal income"
        recode TTLINCOME_INFORMAL (10=11) (11=12) (12=13)
        lab val TTLINCOME_INFORMAL INCOME_LAB
    #/*needed to reverse score these items to match w/ instrument and 7/11 version*/
      recode SD_SF2 (1=5) (2=4) (4=2) (5=1)
      recode ISI7SRI (1=0) (2=1) (3=2) (4=3) (5=4)
      recode ISI7SRI_SINCE (3=1) (1=3)
      recode PHQ9_1-PHQ9_9 (1=0) (2=1) (3=2) (4=3)
      recode GAD7* (1=0) (2=1) (3=2) (4=3)
      recode PSS_1 (1=0) (2=1) (3=2) (4=3) (5=4)
      recode PSS_2-PSS_3 (1=4) (2=3) (3=2) (4=1) (5=0)
      recode PSS_4 (1=0) (2=1) (3=2) (4=3) (5=4)
      recode LIFESEX_PARTNUM (1=0) (2=1) (3=2) (4=3) (5=4) (6=5) (7=6)//Recode LIFESEX_PARTNUM to match final version
      rename DESCRIBE_3MOPARTNER_1 DESCRIBE_3MOPARTNER_3
      rename DESCRIBE_3MOPARTNER_5 DESCRIBE_3MOPARTNER_1
      recode SEX3MO_PARTNUM (0=.) (1=.) (2=1) (3=2) (4=3) (5=4) (6=5) (7=6)
      recode SEX3MO_CONDOMFREQ (1=0) (2=1) (3=2) (4=3) (5=4)
      //NOTE: OLD VERSION had option for *CTRCPTV_0 for have not had sex in last 3 months...leave for now. 
      recode SEX3MO_SUIFREQ (1=0) (2=1) (3=2) (4=3) (5=4)
      recode HIVP_TESTLOC (5=3) (3=4) (4=5)
      recode HIVN_LASTTEST (5=4)
      recode HCV_STATUS (0=88)
      lab def HCVSTATUS_LAB 1"Yes" 0"No" 88"I did not get my results" -99"Seen but not answered"
      recode PREP_KNOW (1=0) (2=1) (3=2) (4=3)
      recode PREP_INTEREST (1=0) (2=1) (3=2) (4=3) (5=4)
      recode PREG_FREQ (1=0) (2=1) (3=2) (4=3) (5=4)
      recode PREG_UNPLAN (1=0) (2=1) (4=2) (5=3) (6=4)
      recode PREG_CHILD (1=0) (2=1) (3=2) (4=3) (5=4)
      recode PREG_LIVE (1=0) (2=1) (3=2) (4=3) (5=4)
      recode TOBACCO_LAST (3=1) (1=3)
      recode TOBACCO_SMOKEFREQ (3=2) (2=1) (1=0)
      recode TOBACCO_SMOKELESSFRE (3=2) (2=1) (1=0)
      recode TOBACCO_VEPFREQ (3=2) (2=1) (1=0)
      recode ALC_LAST (3=1) (1=3)
      recode ALC_30 (4=1) (5=2) (6=3) (7=4) (8=5) (9=6) (10=7) (11=8) (12=9) (13=10) (14=11) (15=12) (16=13) (17=14) (18=15) (19=16) (20=17) (21=18) (22=19) (23=20) (24=21) (25=22) (26=23) (27=24) (28=25) (29=26) (30=27) (31=28) (32=29) (33=30)
      recode ALC_30_BINGE_F (31=0)
      recode ALC_30_BINGE_M (31=0)
      recode MARJ_LAST (3=1) (1=3)
      recode MARJ_FREQ_CURR (1=5) (2=4) (4=2) (5=1)
      recode METH_LAST (3=1) (1=3)
      recode METH_30 (5=1) (6=2) (7=3) (8=4) (9=5) (10=6) (11=7) (12=8) (13=9) (14=10) (15=11) (16=12) (17=13) (18=14) (19=15) (20=16) (21=17) (22=18) (23=19) (24=20) (25=21) (26=22) (27=23) (28=24) (29=25) (30=26) (31=27) (32=28) (33=29) (34=30)
      ###### stopped at METH_30 b/c just realized we probs don't need any of these recodes if we're importing strings...whoops
}



# read a csv
# process csv
# write csv as rdata file
# }
# generate_version2(version1,filenamev2)
# function for v2{
#   adapter from v1 to v2
#   read csv for v2
#   process csv for v2
#   merge v1 v2
#   write csv as rdata file
# }

raw_baseline <- read_csv()

filtered_baseline <- raw_baseline %>%
  filter(id != "abc") %>% # equivalent to keep in stata
  filter(id != "def") %>%
  select(abc = A,B,def = C) %>% # use = to rename on the fly, select keeps
  select(-B) %>% # equivalent to keep or drop for columns, but can also rename
  rename(xyz = abc) # difference between rename and select is taht rename keeps all vars

big <- read_csv(datadictionary.csv)
varlabels <- big$colname
names(filtered_baseline) <- varlabels
