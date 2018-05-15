# libraries
library(stringr)
library(tidyverse)
library(caret)

print_05 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/amps-2005-newspaper-magazine-readership-v1.1.csv")
electr_05 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/amps-2005-electronic-media-v1.1.csv")
internet_05 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/amps-2005-internet-v1.1.csv")
demogrs_05 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/amps-2005-demographics-v1.1.csv")
personal_05 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/amps-2005-personal-v1.1.csv")
lsm_05 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/amps-2005-lsm-v1.1.csv")
lifestage_05 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/amps-2005-lifestage-v1.1.csv")
# attitudes_05 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/amps-2005-attitudes-v1.1.csv") # dont have this and lifestyles yet
# 
save(print_05, electr_05, internet_05, demogrs_05, personal_05, lsm_05, lifestage_05, file = "input_05.RData")

load("input_05.RData")
# # 
print_05_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/amps-2005-newspaper-magazine-readership-v1.1_variable_labels.txt")
electr_05_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/amps-2005-electronic-media-v1.1_variable_labels.txt")
# internet_05_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/metadata/variable_labels/amps-2005-internet-v1.1_variable_labels.txt")
# demogrs_05_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/metadata/variable_labels/amps-2005-demographics-v1.1_variable_labels.txt")
# personal_05_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/metadata/variable_labels/amps-2005-personal-v1.1_variable_labels.txt")
# lsm_05_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/metadata/variable_labels/amps-2005-lsm-v1.1_variable_labels.txt")
# lifestage_05_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/metadata/variable_labels/amps-2005-lifestage-v1.1_variable_labels.txt")
# # attitudes_05_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/csv/metadata/variable_labels/amps-2005-attitudes-v1.1_variable_labels.txt")
# 
# 
# # 
save(print_05_labels, electr_05_labels, file = "labels_05.RData" ) #, internet_05_labels, demogrs_05_labels, personal_05_labels, lsm_05_labels, lifestage_05_labels, file = "labels_05.RData")
# 
load("labels_05.RData")

## 1st Print (newspapers and magazines) Media Set

## ISSUES
names_issues_print_05 <- str_subset(print_05_labels, 'Number of different issues usually read or page through') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()
vars_issues_print_05 <- str_subset(print_05_labels, 'Number of different issues usually read or page through') %>%
        str_replace('Number\\sof\\sdifferent.+', '') %>%
        str_trim()

##Newspapers
# # fix names and get rid of some and save
# names_newspapers_05_issues <- names_issues_print_05[c(1:42)]
# fix(names_newspapers_05_issues)
# saveRDS(names_newspapers_05_issues, "names_newspapers_05_issues.rds")
names_newspapers_05_issues <- readRDS("names_newspapers_05_issues.rds")

# vector of variables
vars_newspapers_05_issues <- vars_issues_print_05[c(1:42)]
issues_newspapers_05 <- print_05[,vars_newspapers_05_issues]

# Magazines
# # fix names and get rid of some (including MNet guides and save
# names_magazines_05_issues <- names_issues_print_05[c(43:49,52:56,58:65,68:71,73:89,91,93:95,97:130,132)]
# fix(names_magazines_05_issues)
# saveRDS(names_magazines_05_issues, "names_magazines_05_issues.rds")
names_magazines_05_issues <- readRDS("names_magazines_05_issues.rds")

# vector of variables
vars_magazines_05_issues <- vars_issues_print_05[c(43:49,52:56,58:65,68:71,73:89,91,93:95,97:130,132)]
issues_magazines_05 <- print_05[,vars_magazines_05_issues]

# create datasets ...for newspapers and magazines:SAME FOR SIMPLE ...
newspapers_engagement_05_all <- issues_newspapers_05
names(newspapers_engagement_05_all) <- names_newspapers_05_issues
magazines_engagement_05_all <- issues_magazines_05
names(magazines_engagement_05_all) <- names_magazines_05_issues

newspapers_engagement_05_simple_all <- issues_newspapers_05
names(newspapers_engagement_05_simple_all) <- names_newspapers_05_issues
magazines_engagement_05_simple_all <- issues_magazines_05
names(magazines_engagement_05_simple_all) <- names_magazines_05_issues

# # # replace NAs with zeros
newspapers_engagement_05_all[is.na(newspapers_engagement_05_all)] <- 0
magazines_engagement_05_all[is.na(magazines_engagement_05_all)] <- 0

newspapers_engagement_05_simple_all[is.na(newspapers_engagement_05_simple_all)] <- 0
magazines_engagement_05_simple_all[is.na(magazines_engagement_05_simple_all)] <- 0

# save (alls)
saveRDS(newspapers_engagement_05_all, "newspapers_engagement_05_all.rds")
saveRDS(magazines_engagement_05_all, "magazines_engagement_05_all.rds")
saveRDS(newspapers_engagement_05_simple_all, "newspapers_engagement_05_simple_all.rds")
saveRDS(magazines_engagement_05_simple_all, "magazines_engagement_05_simple_all.rds")

# CLEAN UP (for now not "simple" versions)
# for newspapers: nothing
newspapers_engagement_05 <- newspapers_engagement_05_all
newspapers_engagement_05_simple <- newspapers_engagement_05_simple_all

# for magazines - deal with it in vehicle_cleaning project
magazines_engagement_05 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_05.rds")
magazines_engagement_05_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_05_simple.rds")

# save them in this project
saveRDS(newspapers_engagement_05, "newspapers_engagement_05.rds")
saveRDS(magazines_engagement_05, "magazines_engagement_05.rds")
saveRDS(newspapers_engagement_05_simple, "newspapers_engagement_05_simple.rds")
saveRDS(magazines_engagement_05_simple, "magazines_engagement_05_simple.rds")

magazines_engagement_05 <- readRDS("magazines_engagement_05.rds")
newspapers_engagement_05 <- readRDS("newspapers_engagement_05.rds")
magazines_engagement_05_simple <- readRDS("magazines_engagement_05_simple.rds")
newspapers_engagement_05_simple <- readRDS("newspapers_engagement_05_simple.rds")

## 2nd Electronic Media Set
# # RADIO
# 
# names_radio_05_4w <- electr_05_labels %>%
#         str_subset('Listened to this radio station in the past 4 weeks') %>%
#         str_replace('.+-','') %>%
#         str_trim()
# 
# names_radio_05_4w <- names_radio_05_4w[c(1:31,34)] # get rid of "unsure" and "none" and summaries
# 
# 
# # most radio stations in 4 weeks, so use that to create names list
# names_radio_05 <- names_radio_05_4w
# 
# check_radio <- readRDS("names_radio_10.rds")
# fix(names_radio_05)
# 
# saveRDS(names_radio_05, "names_radio_05.rds")
names_radio_05 <- readRDS('names_radio_05.rds')

names_radio_05_7 <- electr_05_labels %>%
        str_subset('Listened to this radio station in the past 7 days') %>%
        str_replace('.+-','') %>%
        str_trim()

names_radio_05_7 <- names_radio_05_7[c(1:31,34)] # get rid of "unsure" and "none" and summaries


names_radio_05_y <- electr_05_labels %>%
        str_subset('Listened to this radio station yesterday') %>%
        str_replace('.+-','') %>%
        str_trim()

names_radio_05_y <- names_radio_05_y[c(1:31,34)] # get rid of "unsure" and "none" and summaries

# get data...
# 4 weeks:
colnames_4weeks <- electr_05_labels %>%
        str_subset('Listened to this radio station in the past 4 weeks') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_4weeks <- colnames_4weeks[c(1:31,34)]
radio4weeks_05 <- electr_05[,names(electr_05) %in% colnames_4weeks]

# 7 days
colnames_7days <- electr_05_labels %>%
        str_subset('Listened to this radio station in the past 7 days') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_7days <- colnames_7days[c(1:31,34)]
radio7days_05 <- electr_05[,names(electr_05) %in% colnames_7days]

# yesterday
colnames_yesterday <- electr_05_labels %>%
        str_subset('Listened to this radio station yesterday') %>%
        str_replace('Listened.+','') %>%
        str_trim()
colnames_yesterday <- colnames_yesterday[c(1:31,34)]
radioYesterday_05 <- electr_05[,names(electr_05) %in% colnames_yesterday]

# # want to create two sparser sets for 7 days & yesterday to add up with 4week set
# 

#         
radio_engagement_05_all <- radio4weeks_05 + radio7days_05 + radioYesterday_05
names(radio_engagement_05_all) <- names_radio_05

saveRDS(radio_engagement_05_all, "radio_engagement_05_all.rds")
radio_engagement_05_all <- readRDS("radio_engagement_05_all.rds")

# AFTER CLEANING (see vehicle cleaning project)
radio_engagement_05 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/radio_engagement_05.rds")

# save in this space
saveRDS(radio_engagement_05, "radio_engagement_05.rds")

## TV

# # check with 2012 names:
# check_tv_names_12 <- readRDS("names_tv_10.rds")
# names_tv_05 <- electr_05_labels %>%
#         str_subset('Watched.+4\\sweeks') %>%
#         str_replace('.+Watched\\s','') %>%
#         str_replace('in\\sthe\\sPAST\\s4\\sWEEKS','') %>%
#         str_trim()
# 
# saveRDS(names_tv_05, "names_tv_05.rds")
# names_tv_05 <- readRDS("names_tv_05.rds")

# want to isolate only past 4 weeks (no DSTV yet)
tv4weeks_05 <- electr_05[,c('ca39co9_1', # "e TV"
                            'ca39co9_4', # "SABC 1"
                            'ca39co9_5', # "SABC 2"
                            'ca39co9_6', # "SABC 3"
                            # 'ca50co24_2', # "Cape Town TV" 
                            # 'ca50co16_2', # "Soweto TV"
                            'ca39co18_7' # other TV
                            )] 

# want to isolate only past 7 days...(no DSTV yet)
tv7days_05 <- electr_05[,c('ca39co19_1', # "e TV"
                           'ca39co19_4', # "SABC 1"
                           'ca39co19_5', # "SABC 2"
                           'ca39co19_6', # "SABC 3"
                           # 'ca50co24_2', # "Cape Town TV" 
                           # 'ca50co16_2', # "Soweto TV"
                           'ca39co28_7' # other TV
                           )] 

# want to isolate only yesterday...(no DSTV yet)
tvYesterday_05 <- electr_05[,c('ca39co29_1', # "e TV"
                               'ca39co29_4', # "SABC 1"
                               'ca39co29_5', # "SABC 2"
                               'ca39co29_6', # "SABC 3"
                               # 'ca50co24_2', # "Cape Town TV" 
                               # 'ca50co16_2', # "Soweto TV"
                               'ca39co38_7' # other TV
                               )]


# dealing with complicated DSTV issue:
# will identify list of variables for each time period that will serve as
# proxy or indication of dstv viewing for each period.

# 4 weeks:
dstv_05_4w <- electr_05[,c(#'ca50co09_1', # Africa Magicx
                'ca39co10_9', # Cartoon Network
                'ca39co11_1', # Channel O
                'ca39co11_5', # Discovery Channel
                'ca39co12_0', # ESPN
                'ca39co12_1', # Fashion TV
                'ca39co12_5', # History Channel
                'ca39co12_9', # KykNET
                'ca39co13_4', # Mnet Movies 1
                'ca39co13_5', # Mnet Movies 2
                'ca39co13_3', # MTV
                'ca39co13_6', #National Geographic Channel
                # 'ca50co14_9', # News24x
                # 'ca50co16_0', # Sony Entertainment Televisionx
                'ca39co15_8', # Supersports1
                'ca39co15_9', # Supersports2
                'ca39co16_0', # Supersports3
                'ca39co16_1', # Supersports4
                'ca39co16_7' # Travel Channel
                )]
# create single vector:

dstv_05_4w_vec <- rowSums(dstv_05_4w)
dstv_05_4w_vec <- ifelse(dstv_05_4w_vec != 0, 1, 0)

# 7 days:
dstv_05_7d <- electr_05[,c(#'ca50co09_1', # Africa Magicx
        'ca39co20_9', # Cartoon Network
        'ca39co21_1', # Channel O
        'ca39co21_5', # Discovery Channel
        'ca39co22_0', # ESPN
        'ca39co22_1', # Fashion TV
        'ca39co22_5', # History Channel
        'ca39co22_9', # KykNET
        'ca39co23_4', # Mnet Movies 1
        'ca39co23_5', # Mnet Movies 2
        'ca39co23_3', # MTV
        'ca39co23_6', #National Geographic Channel
        # 'ca50co14_9', # News24x
        # 'ca50co16_0', # Sony Entertainment Televisionx
        'ca39co25_8', # Supersports1
        'ca39co25_9', # Supersports2
        'ca39co26_0', # Supersports3
        'ca39co26_1', # Supersports4
        'ca39co26_7' # Travel Channel
)]
dstv_05_7d_vec <- rowSums(dstv_05_7d)
dstv_05_7d_vec <- ifelse(dstv_05_7d_vec != 0, 1, 0)

# yesterday:
dstv_05_y <- electr_05[,c(#'ca50co09_1', # Africa Magicx
        'ca39co30_9', # Cartoon Network
        'ca39co31_1', # Channel O
        'ca39co31_5', # Discovery Channel
        'ca39co32_0', # ESPN
        'ca39co32_1', # Fashion TV
        'ca39co32_5', # History Channel
        'ca39co32_9', # KykNET
        'ca39co33_4', # Mnet Movies 1
        'ca39co33_5', # Mnet Movies 2
        'ca39co33_3', # MTV
        'ca39co33_6', #National Geographic Channel
        # 'ca50co14_9', # News24x
        # 'ca50co16_0', # Sony Entertainment Televisionx
        'ca39co35_8', # Supersports1
        'ca39co35_9', # Supersports2
        'ca39co36_0', # Supersports3
        'ca39co36_1', # Supersports4
        'ca39co36_7' # Travel Channel
)]
dstv_05_y_vec <- rowSums(dstv_05_y)
dstv_05_y_vec <- ifelse(dstv_05_y_vec != 0, 1, 0)

dstv_05 <- dstv_05_4w_vec + dstv_05_7d_vec + dstv_05_y_vec

# combining into a tv engagement dataset first not including dstv:
tv_engagement_05 <- tv4weeks_05 + tv7days_05 + tvYesterday_05

tv_engagement_05 <- tv_engagement_05 %>%
        mutate(DSTV = dstv_05)

names(tv_engagement_05) <- c("e tv",
                             "SABC 1",
                             "SABC 2",
                             "SABC 3",
                             "Other TV",
                             "DSTV")
                             
                             # "Cape Town TV",
                             # "Soweto TV",

saveRDS(tv_engagement_05, "tv_engagement_05.rds")

tv_engagement_05 <- readRDS("tv_engagement_05.rds")

## 3rd Internet Media Set

## accessed: sum of 4weeks, 7days and yesterday
internet_level1 <- internet_05[,str_detect(names(internet_05), 'ca41co(41)|(47)|(54)')]

#change all 2 = "No" and NA's' to 0
internet_level1 <- data.frame(ifelse(is.na(internet_level1) | internet_level1 == 2, 0, 1))

internet_level1 <- rowSums(internet_level1)

# # what internet was accessed for...NOT IN 2005 yet
# ##  (maybe could use similar to vehicles?? as well as add up and multiply with first eng):
# 
# internet_level2 <- internet_05[,str_detect(names(internet_05), 'ca47co(50_1)|(50_6)|(51_6)|(51_7)|(51_8)|(51_9)')]
# 
# names(internet_level2) <- c('int_search',
#                           'int_social',
#                           'int_radio',
#                           'int_tv',
#                           'int_print',
#                           'int_news')

## create single dataframe for internet multiplying internet_level1 with sum of internet_level2:
internet_engagement_05 <- internet_level1 #  * internet_level2

saveRDS(internet_engagement_05, "internet_engagement_05.rds")

internet_engagement_05 <- readRDS("internet_engagement_05.rds")

## create single dataframe for media05, including total_engagement columns (consider using media groupings .. follow up on this!)

# Level 1: Type
media_type_05 <- data.frame(cbind(qn = print_05$qn,
                                  rowSums(scale(newspapers_engagement_05)),
                                  rowSums(scale(magazines_engagement_05)),
                                  rowSums(scale(radio_engagement_05)),
                                  rowSums(scale(tv_engagement_05)),
                                  scale(internet_engagement_05)))
names(media_type_05) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_05 <- media_type_05 %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet)) 


media_type_05_simple <- data.frame(cbind(qn = print_05$qn,
                                  rowSums(scale(newspapers_engagement_05_simple)),
                                  rowSums(scale(magazines_engagement_05_simple)),
                                  rowSums(scale(radio_engagement_05)),
                                  rowSums(scale(tv_engagement_05)),
                                  scale(internet_engagement_05)))
names(media_type_05_simple) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_05_simple <- media_type_05_simple %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet)) 


# Level 2: Vehicles
media_vehicles_05 <- data.frame(cbind(qn = print_05$qn,
                                      newspapers_engagement_05,
                                      magazines_engagement_05,
                                      radio_engagement_05,
                                      tv_engagement_05,
                                      internet_eng = internet_engagement_05))

media_vehicles_05_simple <- data.frame(cbind(qn = print_05$qn,
                                      newspapers_engagement_05_simple,
                                      magazines_engagement_05_simple,
                                      radio_engagement_05,
                                      tv_engagement_05,
                                      internet_eng = internet_engagement_05))

saveRDS(media_type_05, 'media_type_05.rds')
saveRDS(media_vehicles_05, 'media_vehicles_05.rds')
saveRDS(media_type_05_simple, 'media_type_05_simple.rds')
saveRDS(media_vehicles_05_simple, 'media_vehicles_05_simple.rds')

media_type_05.rds <- readRDS('media_type_05.rds')
media_vehicles_05 <- readRDS('media_vehicles_05.rds')
media_type_05_simple <- readRDS('media_type_05_simple.rds')
media_vehicles_05_simple <- readRDS('media_vehicles_05_simple.rds')

## 4th Demographics Set (see notes for descriptions)
age <- personal_05[,'ca47co38']
sex <- demogrs_05[,'ca91co51a']
edu <- demogrs_05[,'ca91co48']
for(i in 1: length(edu)) {
        if(edu[i] %in% c(6,7)) {
                edu[i] <- edu[i] + 1
        }
        else if(edu[i] == 8) {
                edu[i] <- 6
        }
}
hh_inc <- demogrs_05[,'ca91co50'] # up to 12 000+
race <- demogrs_05[,'ca91co51b'] # here need to align coding with later sets
race <- ifelse(race == 1, 9, race)
race <- ifelse(race == 2, 6, race)
race <- ifelse(race == 3, 7, race)
race <- ifelse(race == 4, 8, race)
race <- race - 5

province <- demogrs_05[,'ca91co56']


metro1 <- demogrs_05[,'ca91co57'] # NB... reconsider all to greater jhb..ie including soweto..
metro2 <- demogrs_05[,'ca91co58'] + 9
metro <- rowSums(cbind(metro1, # NB...use the combination to determine non metropolitan respondents
                       metro2), na.rm = TRUE)

# collect and code into single metro set:
#0 = no metro
#1 Cape Town
#2 Cape Town Fringe Area
#3 Port Elizabeth/Uitenhage
#4 East London
#5 Durban
#6 Bloemfontein
#7 Greater Johannesburg
#8 Reef
#9 Pretoria
#10 Kimberley
##11 Pietermaritzburg
##12 Vaal
##13 Welkom

metro <- ifelse(metro == 7 | metro == 19, 7, metro) # Soweto back into greater jhb
metro <- ifelse(metro %in% c(13), 12,metro)
metro <- ifelse(metro %in% c(14), 13,metro)

lang <- demogrs_05[,'ca91co75'] + 1 # change 0 to 1, so add one to all
lifestages <- demogrs_05[,'ca91co77'] # NB check coding ... differs between years
mar_status <- personal_05[,'ca47co09']

lsm <- lsm_05[,'ca91co64']
lsm <- ifelse(lsm == 0,10,lsm)

# lifestyles and attitudes not yet in 2005

demographics_05 <- data.frame(qn = electr_05$qn,
                              pwgt = electr_05$pwgt,
                              age,
                              sex,
                              edu,
                              hh_inc,
                              race,
                              province,
                              metro,
                              lang,
                              lifestages,
                              mar_status,
                              lsm)


#reducing levels of categorical variables and setting factor types for demographics:

# age:
demographics_05$age <- ifelse(demographics_05$age %in% c(1,2), 1, demographics_05$age)
demographics_05$age <- ifelse(demographics_05$age %in% c(3,4), 2, demographics_05$age)
demographics_05$age <- ifelse(demographics_05$age %in% c(5,6), 3, demographics_05$age)
demographics_05$age <- ifelse(demographics_05$age %in% c(7,8), 4, demographics_05$age)
demographics_05$age <- factor(demographics_05$age, ordered = TRUE)

# sex:
demographics_05$sex <- factor(demographics_05$sex, ordered = FALSE)

#edu:
demographics_05$edu <- ifelse(demographics_05$edu %in% c(1,2,3,4), 1, demographics_05$edu)
demographics_05$edu <- ifelse(demographics_05$edu %in% c(5), 2, demographics_05$edu)
demographics_05$edu <- ifelse(demographics_05$edu %in% c(6,7,8), 3, demographics_05$edu)
demographics_05$edu <- factor(demographics_05$edu, ordered = TRUE)

#hh_inc
demographics_05$hh_inc <- ifelse(demographics_05$hh_inc %in% c(1,2,3,4), 1, demographics_05$hh_inc)
demographics_05$hh_inc <- ifelse(demographics_05$hh_inc %in% c(5,6), 2, demographics_05$hh_inc)
demographics_05$hh_inc <- ifelse(demographics_05$hh_inc %in% c(7), 3, demographics_05$hh_inc)
demographics_05$hh_inc <- ifelse(demographics_05$hh_inc %in% c(8), 4, demographics_05$hh_inc)
demographics_05$hh_inc <- factor(demographics_05$hh_inc, ordered = TRUE)

demographics_05$race <- factor(demographics_05$race, ordered = FALSE)
demographics_05$province <- factor(demographics_05$province, ordered = FALSE)
demographics_05$metro <- factor(demographics_05$metro, ordered = FALSE)
demographics_05$lang <- factor(demographics_05$lang, ordered = FALSE)
demographics_05$lifestages <- factor(demographics_05$lifestages, ordered = FALSE)
demographics_05$mar_status <- factor(demographics_05$mar_status, ordered = FALSE)

# lsm
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(1,2), 1, demographics_05$lsm)
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(3,4), 2, demographics_05$lsm)
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(5,6), 3, demographics_05$lsm)
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(7,8), 4, demographics_05$lsm)
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(9,10), 5, demographics_05$lsm)
demographics_05$lsm <- factor(demographics_05$lsm, ordered = TRUE)

saveRDS(demographics_05, "demographics_05.rds")
demographics_05 <- readRDS("demographics_05.rds")

# read datafiles again if necessary
magazines_engagement_05 <- readRDS("magazines_engagement_05.rds")
magazines_engagement_05_simple <- readRDS("magazines_engagement_05_simple.rds")

newspapers_engagement_05 <- readRDS("newspapers_engagement_05.rds")
newspapers_engagement_05_simple <- readRDS("newspapers_engagement_05_simple.rds")

radio_engagement_05 <- readRDS("radio_engagement_05.rds")

tv_engagement_05 <- readRDS("tv_engagement_05.rds")

internet_engagement_05 <- readRDS("internet_engagement_05.rds")
# internet_engagement_05_simple <- readRDS("internet_engagement_05_simple.rds") #no simple

media_type_05 <- readRDS("media_type_05.rds")
media_type_05_simple <- readRDS("media_type_05_simple.rds")
media_vehicles_05 <- readRDS("media_vehicles_05.rds")
media_vehicles_05_simple <- readRDS("media_vehicles_05_simple.rds")

demographics_05 <- readRDS("demographics_05.rds")

# #create single dataset minus non metropolitans
set05 <- demographics_05 %>%
        left_join(media_type_05) %>%
        left_join(media_vehicles_05)
#%>%filter(metro != 0)
set05_simple <- demographics_05 %>%
        left_join(media_type_05_simple) %>%
        left_join(media_vehicles_05_simple)
#%>%filter(metro != 0)

# get rid of zero variances:
ind_05 <- nearZeroVar(set05[,14:ncol(set05)], saveMetrics = TRUE)
good_set <- set05[,14:ncol(set05)][,!ind_05$zeroVar]
set05 <- data.frame(cbind(set05[,1:13], good_set))

ind_05_simple <- nearZeroVar(set05_simple[,14:ncol(set05_simple)], saveMetrics = TRUE)
good_set_simple <- set05_simple[,14:ncol(set05_simple)][,!ind_05_simple$zeroVar]
set05_simple <- data.frame(cbind(set05_simple[,1:13], good_set_simple))

# scale media type and media vehicles
set05[,14:ncol(set05)] <- scale(set05[,14:ncol(set05)])
set05_simple[,14:ncol(set05_simple)] <- scale(set05_simple[,14:ncol(set05_simple)])

# save it:
saveRDS(set05, "set05.rds")
saveRDS(set05_simple, "set05_simple.rds")

