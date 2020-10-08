library("haven")
library("tidyverse")

# caliendo <- read_sas("MAN_Data/caliendo_merged.sas7bdat")
# duration <- read_sas("MAN_Data/duration_all_final.sas7bdat")
# master_id <- read_sas("MAN_Data/master_id_list_rev_final.sas7bdat")
# networks <- read_sas("MAN_Data/networks.sas7bdat")
# dyad_summary <- read_sas("MAN_Data/participant_dyad_summary.sas7bdat")
# partner_pairs <- read_sas("MAN_Data/partner_pairs.sas7bdat")
# partners_analysis <- read_sas("MAN_Data/partners_analysis_final.sas7bdat")
# partners_concurrency <- read_sas("MAN_Data/partners_concurrency.sas7bdat")
# partners_concurrency_eid_dedup <- read_sas("MAN_Data/partners_concurrency_eid_dedup.sas7bdat")
# sex_connections_dedup <- read_sas("MAN_Data/sex_connections_dedup.sas7bdat")
# status_within_network <- read_sas("MAN_Data/status_within_network.sas7bdat")
# survey_concurrency <- read_sas("MAN_Data/survey_concurrency.sas7bdat")

status <- read_sas("MAN_Data/status.sas7bdat")
participants_survey <- read_sas("MAN_Data/participants_survey.sas7bdat")
partners_survey <- read_sas("MAN_Data/partners_survey.sas7bdat")

HIV.self_report <- participants_survey %>% select(final_id, HIVtest_ever, HIVtest_result)
demo.self_report <- status %>% select(final_id, part_hispanic, starts_with("part_race"), age_baseline)


HIV.ego_report <- partners_survey %>% 
        filter(final_id %in% status$final_id) %>% 
        select(final_id, enrolled_final_id, age, hispanic, race, morethanonce, main, pasdiscusstopicsn3, discussstatusfs, statusfs, ls_knowstatus, hivstatls)

# pasdiscusstopicsn3: have you discussed HIV status with ___?

# discussstatusfs did you and your partner share both of your HIV statuses before you had sex?
# statusfs: what was his status at that time? 1 = HIV-; 2 = HIV+; 3 = Don't know

# ls_knowstatus: the last time you had sex with ___ did you know his/her HIV status?
# hivstatls: what was his status at that time? 1 = HIV-; 2 = HIV+; 3 = Don't know

# Number of alters per ego
N.alters <- HIV.ego_report %>% count(enrolled_final_id)
HIV.ego_report <- left_join(HIV.ego_report, N.alters, by = "enrolled_final_id") %>%
                        rename(N.alters.per.ego = n)


### p_hiv
HIV.ego_report$p_hiv <- NA

# only had sex once
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 0 & HIV.ego_report$statusfs == 1] <- 0 # negative
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 0 & HIV.ego_report$statusfs == 2] <- 1 # positive
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 0 & HIV.ego_report$discussstatusfs == 0] <- 2 # didn't discuss
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 0 & HIV.ego_report$discussstatusfs == 1 & HIV.ego_report$statusfs == 9] <- 2 # discussed but don't know status
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 0 & HIV.ego_report$discussstatusfs == 9] <- 2 # don't know if discussed

# sex more than once
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 1 & HIV.ego_report$hivstatls == 1] <- 0 # negative
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 1 & HIV.ego_report$hivstatls == 2] <- 1 # positive
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 1 & HIV.ego_report$ls_knowstatus == 0] <- 2 # don't know
HIV.ego_report$p_hiv[HIV.ego_report$morethanonce == 1 & HIV.ego_report$ls_knowstatus == 9] <- 2 # don't know if status was known

### partner type
HIV.ego_report$ptype <- NA
HIV.ego_report$ptype[HIV.ego_report$main == 1] <- "Main"
HIV.ego_report$ptype[HIV.ego_report$main == 0] <- "Casual"
HIV.ego_report$ptype[HIV.ego_report$main == 9] <- "Casual"
HIV.ego_report$ptype[HIV.ego_report$morethanonce == 0] <- "Once"


HIV.ego_report %>% count(ptype)

# there's some guys with missing ptype so this is potentially some useful info
# need to see how this was handled in ARTnet
# check <- HIV.ego_report %>% filter(is.na(ptype))
# 
# check2 <- HIV.ego_report %>% 
#         filter(final_id == 543 | final_id == 241 | final_id == 1515 | final_id == 299 | final_id == 969 | final_id == 1025 | final_id == 1472 | final_id == 1390 | final_id == 827) %>%
#         select(final_id, enrolled_final_id, main, morethanonce)


# Self-reported HIV for egos
HIV.self_report$hiv3 <- NA
HIV.self_report$hiv3[HIV.self_report$HIVtest_result == 0] <- 0 #negative
HIV.self_report$hiv3[HIV.self_report$HIVtest_result == 1] <- 1 #positive
HIV.self_report$hiv3[HIV.self_report$HIVtest_result %in% c(2, 3)] <- 2
HIV.self_report$hiv3[HIV.self_report$HIVtest_ever == 0] <- 2

hiv3 <- HIV.self_report %>% select(final_id, hiv3) %>% rename(enrolled_final_id = final_id)
p_hiv.validation <- HIV.self_report %>% select(final_id, hiv3) %>% rename(p_hiv.validation = hiv3)

# Merging with the HIV.ego_report
MAN.long <- left_join(HIV.ego_report, hiv3, by = "enrolled_final_id")
MAN.long <- left_join(MAN.long, p_hiv.validation, by = "final_id")

# p_hiv.match: 1 = correct report; 0 = incorrect
MAN.long$p_hiv.match <- NA
MAN.long$p_hiv.match[MAN.long$p_hiv %in% c(0,2) & MAN.long$p_hiv.validation %in% c(0,2)] <- 1
MAN.long$p_hiv.match[MAN.long$p_hiv == 1 & MAN.long$p_hiv.validation == 1] <- 1
MAN.long$p_hiv.match[MAN.long$p_hiv == 1 & MAN.long$p_hiv.validation %in% c(0,2)] <- 0
MAN.long$p_hiv.match[MAN.long$p_hiv %in% c(0,2) & MAN.long$p_hiv.validation == 1] <- 0


table(MAN.long$p_hiv, MAN.long$p_hiv.validation, useNA = "ifany")
table(MAN.long$p_hiv, MAN.long$p_hiv.match)

# Nondifferential
sens.nd <- 24/37   #0.649
spec.nd <- 144/146 #0.986

# By partner type
table(MAN.long$p_hiv, MAN.long$p_hiv.validation, MAN.long$ptype)

sens.main <- 13/18 #0.722
spec.main <- 74/75 #0.987

sens.casu <- 5/8   #0.625
spec.casu <- 24/24 #1.0

sens.once <- 6/10  #0.6
spec.once <- 40/41 #0.976

# By HIV3
table(MAN.long$p_hiv, MAN.long$p_hiv.validation, MAN.long$hiv3)

sens.neg <- 2/9     #0.222
spec.neg <- 126/126 #1.0

sens.pos <- 21/26   #0.808
spec.pos <- 11/12   #0.917

sens.unk <- 1/1     #1.0
spec.unk <- 7/7     #1.0

# Specificity based on reported as unknown or negative
table(MAN.long$p_hiv, MAN.long$p_hiv.validation)

spec.p.neg <- 99/133 #0.744 P(p_hiv = 0|p_hiv.validation = 0)
spec.p.unk <- 3/13   #0.23  P(p_hiv = 2|p_hiv.validation = 2)
