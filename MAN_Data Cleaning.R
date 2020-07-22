library(haven)


caliendo <- read_sas("MAN_Data/caliendo_merged.sas7bdat")
duration <- read_sas("MAN_Data/duration_all_final.sas7bdat")
master_id <- read_sas("MAN_Data/master_id_list_rev_final.sas7bdat")
networks <- read_sas("MAN_Data/networks.sas7bdat")
dyad_summary <- read_sas("MAN_Data/participant_dyad_summary.sas7bdat")
participants_survey <- read_sas("MAN_Data/participants_survey.sas7bdat")
partner_pairs <- read_sas("MAN_Data/partner_pairs.sas7bdat")
partners_analysis <- read_sas("MAN_Data/partners_analysis_final.sas7bdat")
partners_concurrency <- read_sas("MAN_Data/partners_concurrency.sas7bdat")
partners_concurrency_eid_dedup <- read_sas("MAN_Data/partners_concurrency_eid_dedup.sas7bdat")
partners_survey <- read_sas("MAN_Data/partners_survey.sas7bdat")
sex_connections_dedup <- read_sas("MAN_Data/sex_connections_dedup.sas7bdat")
status <- read_sas("MAN_Data/status.sas7bdat")
status_within_network <- read_sas("MAN_Data/status_within_network.sas7bdat")
survey_concurrency <- read_sas("MAN_Data/survey_concurrency.sas7bdat")

