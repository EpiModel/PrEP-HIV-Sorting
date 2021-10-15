# Table 1: Ego and dyad level characteristics

rm(list = ls())
source('1 Data cleaning.R')

IDs <- artnetSort %>% select(AMIS_ID) %>% unique()
t1_egos <- left_join(IDs, artnet, by = "AMIS_ID")

# Titles
t1.title <- vector("character", 7)
t1.title[1] <- "Table 1. Sample Characteristics"

t1.groupname <- vector("character", 7) 
t1.groupname[2] <- "Respondent-level"
t1.groupname[4] <- "Partnership-level"

t1.colname <- c("Category", "Respondents N", "Respondents %", "Respondents N", "Respondents %", "Partners N", "Partners %")

# Total Sample
t1.total <- cbind("Total",
               nrow(t1_egos), nrow(t1_egos) / nrow(t1_egos),
               nrow(artnetLong), nrow(artnetLong) / nrow(artnetLong),
               nrow(artnetLong), nrow(artnetLong) / nrow(artnetLong)
               )

# Age Category
a <- t1_egos %>% count(age.cat) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% count(age.cat) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% count(p_age.cat_imp) %>% mutate(perc = round(100*n/sum(n), 1))

t1.age <- rbind(
                cbind("Age category", "", "", "", "", "", ""),
                cbind("15-24", a$n[1], a$perc[1], b$n[1], b$perc[1], c$n[1], c$perc[1]),
                cbind("25-34", a$n[2], a$perc[2], b$n[2], b$perc[2], c$n[2], c$perc[2]),
                cbind("35-44", a$n[3], a$perc[3], b$n[3], b$perc[3], c$n[3], c$perc[3]),
                cbind("45-54", a$n[4], a$perc[4], b$n[4], b$perc[4], c$n[4], c$perc[4]),
                cbind("55-65", a$n[5], a$perc[5], b$n[5], b$perc[5], c$n[5], c$perc[5]),
                cbind("66+", "0", "---", "0", "---", c$n[6], c$perc[6])
                )

# Race/ethnicity
a <- t1_egos %>% count(race.cat) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% count(race.cat) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% count(p_race.cat) %>% mutate(perc = round(100*n/sum(n), 1))

t1.race <- rbind(
        cbind("Race/ ethnicity", "", "", "", "", "", ""),
        cbind("Non-Hispanic black", a$n[1], a$perc[1], b$n[1], b$perc[1], c$n[1], c$perc[1]),
        cbind("Non-Hispanic white", a$n[4], a$perc[4], b$n[4], b$perc[4], c$n[4], c$perc[4]),
        cbind("Hispanic/ Latinx", a$n[2], a$perc[2], b$n[2], b$perc[2], c$n[2], c$perc[2]),
        cbind("Other", a$n[3], a$perc[3], b$n[3], b$perc[3], c$n[3], c$perc[3])
        )

# HIV
a <- t1_egos %>% count(hiv3) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% count(hiv3) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))

t1.hiv <- rbind(
        cbind("HIV status", "", "", "", "", "", ""),
        cbind("Negative", a$n[1], a$perc[1], b$n[1], b$perc[1], c$n[1], c$perc[1]),
        cbind("Diagnosed HIV", a$n[2], a$perc[2], b$n[2], b$perc[2], c$n[2], c$perc[2]),
        cbind("Unknown HIV", a$n[3], a$perc[3], b$n[3], b$perc[3], c$n[3], c$perc[3])
        )

# PrEP
b <- artnetSort %>% filter(!hiv3 == "Pos") %>% count(prep.during.ego2) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(!p_hiv == "Pos") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))

t1.prep <- rbind(
        cbind("PrEP use during partnership", "", "", "", "", "", ""),
        cbind("Never", "---", "---", b$n[1], b$perc[1], c$n[1], c$perc[1]),
        cbind("Ever", "---", "---", b$n[2], b$perc[2], c$n[2], c$perc[2]),
        cbind("Unknown", "---", "---", "---", "---", c$n[3], c$perc[3])
        )

# Partnership type
c <- artnetSort %>% count(ptype) %>% mutate(perc = round(100*n/sum(n), 1))

t1.ptype <- rbind(
        cbind("Partnership type", "", "", "", "", "", ""),
        cbind("Main", "---", "---", "---", "---", c$n[1], c$perc[1]),
        cbind("Casual", "---", "---", "---", "---", c$n[2], c$perc[2]),
        cbind("One-time", "---", "---", "---", "---", c$n[3], c$perc[3])
        )

table1 <- rbind(t1.title, t1.groupname, t1.colname, t1.total, t1.age, t1.race, t1.hiv, t1.prep, t1.ptype)

write.csv(table1, file = "aim1_table1.csv")
