source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

# Lists the column names
# names(artnet)

###Table 1 - Demographic Characteristics

# Total Sample
total <- cbind("Total",
               nrow(artnet), nrow(artnet) / nrow(artnet),
               nrow(artnetLong), nrow(artnetLong) / nrow(artnetLong)
               )

# Age Category
a <- artnet %>% count(age.cat)
age.ego <- cbind(cbind(c(a$age.cat[1], a$age.cat[2], a$age.cat[3],
                        a$age.cat[4], a$age.cat[5], "66+")),
                cbind(c(a$n[1], a$n[2], a$n[3], a$n[4], a$n[5],
                        ifelse(length(which(artnet$age.cat == "66+")) > 0, a$n[6], 0))),
                cbind(rbind((100 * a$n[1] / sum(a$n)),
                            (100 * a$n[2] / sum(a$n)),
                            (100 * a$n[3] / sum(a$n)),
                            (100 * a$n[4] / sum(a$n)),
                            (100 * a$n[5] / sum(a$n)),
                            (100 * a$n[6] / sum(a$n)))))

# Using imputed age
b <- artnetLong %>% count(p_age.cat_imp)
age.part <- cbind(cbind(c(b$n[1], b$n[2], b$n[3], b$n[4], b$n[5],
                         ifelse(length(which(artnetLong$p_age.cat_imp == "66+")) > 0, b$n[6], 0))),
                 cbind(rbind((100 * b$n[1] / sum(b$n)),
                             (100 * b$n[2] / sum(b$n)),
                             (100 * b$n[3] / sum(b$n)),
                             (100 * b$n[4] / sum(b$n)),
                             (100 * b$n[5] / sum(b$n)),
                             (100 * b$n[6] / sum(b$n)))))

# Race/ethnicity
a <- artnet %>% count(race.cat)
race.ego <- cbind(cbind(c(a$race.cat[1], a$race.cat[4], a$race.cat[2], a$race.cat[3])),
                 cbind(c(a$n[1], a$n[4], a$n[2], a$n[3])),
                 cbind(rbind((100 * a$n[1] / sum(a$n)),
                             (100 * a$n[4] / sum(a$n)),
                             (100 * a$n[2] / sum(a$n)),
                             (100 * a$n[3] / sum(a$n)))))

b <- artnetLong %>% count(p_race.cat)
race.part <- cbind(cbind(c(b$n[1], b$n[4], b$n[2], b$n[3])),
                 cbind(rbind((100 * b$n[1] / sum(b$n)),
                             (100 * b$n[4] / sum(b$n)),
                             (100 * b$n[2] / sum(b$n)),
                             (100 * b$n[3] / sum(b$n)))))

# HIV
a <- artnet %>% count(hiv3)
hiv.ego <- cbind(cbind(c("Negative", "Positive", "Unknown", NA)),
                 cbind(c(a$n[1], a$n[2], a$n[3], NA)), #a$n[4]),
                 cbind(rbind((100 * a$n[1] / sum(a$n[1:3])),
                             (100 * a$n[2] / sum(a$n[1:3])),
                             (100 * a$n[3] / sum(a$n[1:3])),
                             NA)))

b <- artnetLong %>% count(p_hiv)
hiv.part <- cbind(cbind(c(b$n[1], b$n[2], b$n[3], b$n[4])), #b$n[4]),
                 cbind(rbind((100 * b$n[1] / sum(b$n[1:3])),
                             (100 * b$n[2] / sum(b$n[1:3])),
                             (100 * b$n[3] / sum(b$n[1:3])),
                             NA)))

table1 <- rbind(total,
                cbind(age.ego, age.part),
                cbind(race.ego, race.part),
                cbind(hiv.ego, hiv.part)
                )
colnames(table1) <- c("Category", "Egos N", "Egos %", "Partners N", "Partners %")
write.csv(table1, file = "aim1_table1.csv")