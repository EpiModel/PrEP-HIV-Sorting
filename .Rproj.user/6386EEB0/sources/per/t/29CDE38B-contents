source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

#### Table 2a - HIV mixing with unknown values ####

a <- artnetLong %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% count(p_hiv) #HIV among partners

# HIV distribution among egos
totalHIV.ego <- cbind(cbind(c("Negative", "Positive", "Unknown", "Total")),
                   cbind(c(a$n[1], a$n[2], a$n[3], sum(a$n))),
                   cbind(rbind((100 * a$n[1] / sum(a$n)),
                               (100 * a$n[2] / sum(a$n)),
                               (100 * a$n[3] / sum(a$n)),
                               (100 * sum(a$n) / sum(a$n)))))

# Negative partners
d.neg.part <- cbind(cbind(c(b$n[1], b$n[4], b$n[7], c$n[1])),
                    cbind(rbind((100 * b$n[1] / a$n[1]),
                                (100 * b$n[4] / a$n[2]),
                                (100 * b$n[7] / a$n[3]),
                                (100 * c$n[1] / sum(c$n)))))

# Positive partners
d.pos.part <- cbind(cbind(c(b$n[2], b$n[5], b$n[8], c$n[2])),
                    cbind(rbind((100 * b$n[2] / a$n[1]),
                                (100 * b$n[5] / a$n[2]),
                                (100 * b$n[8] / a$n[3]),
                                (100 * c$n[2] / sum(c$n)))))

# HIV unknown partners
d.unk.part <- cbind(cbind(c(b$n[3], b$n[6], b$n[9], c$n[3])),
                    cbind(rbind((100 * b$n[3] / a$n[1]),
                                (100 * b$n[6] / a$n[2]),
                                (100 * b$n[9] / a$n[3]),
                                (100 * c$n[3] / sum(c$n)))))

table2a.title <- c("All Partners", "", "", "Alters", "", "", "", "", "")
table2a.colnames <- c("Egos", "Total N", "%", "Negative n", "%", "Positive n", "%", "Unknown n", "%")

table2a <- rbind(table2a.title, table2a.colnames,
                cbind(totalHIV.ego, d.neg.part, d.pos.part, d.unk.part))
write.csv(table2a, file = "aim1_table2a.csv")


#### Table 2b - Partnership pairings with unknown values, stratified by partnership type ####

#### Main partnerships ####

a <- artnetLong %>% filter(ptype == 1) %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% filter(ptype == 1) %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% filter(ptype == 1) %>% count(p_hiv) #HIV among partners

# HIV distribution among egos
totalHIV.ego.main <- cbind(cbind(c("Negative", "Positive", "Unknown", "Total")),
                      cbind(c(a$n[1], a$n[2], a$n[3], sum(a$n))),
                      cbind(rbind((100 * a$n[1] / sum(a$n)),
                                  (100 * a$n[2] / sum(a$n)),
                                  (100 * a$n[3] / sum(a$n)),
                                  (100 * sum(a$n) / sum(a$n)))))

# Negative partners
d.neg.part.main <- cbind(cbind(c(b$n[1], b$n[4], b$n[7], c$n[1])),
                    cbind(rbind((100 * b$n[1] / a$n[1]),
                                (100 * b$n[4] / a$n[2]),
                                (100 * b$n[7] / a$n[3]),
                                (100 * c$n[1] / sum(c$n)))))

# Positive partners
d.pos.part.main <- cbind(cbind(c(b$n[2], b$n[5], b$n[8], c$n[2])),
                    cbind(rbind((100 * b$n[2] / a$n[1]),
                                (100 * b$n[5] / a$n[2]),
                                (100 * b$n[8] / a$n[3]),
                                (100 * c$n[2] / sum(c$n)))))

# HIV unknown partners
d.unk.part.main <- cbind(cbind(c(b$n[3], b$n[6], b$n[9], c$n[3])),
                    cbind(rbind((100 * b$n[3] / a$n[1]),
                                (100 * b$n[6] / a$n[2]),
                                (100 * b$n[9] / a$n[3]),
                                (100 * c$n[3] / sum(c$n)))))

#### Casual partnerships ####

a <- artnetLong %>% filter(ptype == 2) %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% filter(ptype == 2) %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% filter(ptype == 2) %>% count(p_hiv) #HIV among partners

# HIV distribution among egos
totalHIV.ego.casual <- cbind(cbind(c("Negative", "Positive", "Unknown", "Total")),
                           cbind(c(a$n[1], a$n[2], a$n[3], sum(a$n))),
                           cbind(rbind((100 * a$n[1] / sum(a$n)),
                                       (100 * a$n[2] / sum(a$n)),
                                       (100 * a$n[3] / sum(a$n)),
                                       (100 * sum(a$n) / sum(a$n)))))

# Negative partners
d.neg.part.casual <- cbind(cbind(c(b$n[1], b$n[4], b$n[7], c$n[1])),
                         cbind(rbind((100 * b$n[1] / a$n[1]),
                                     (100 * b$n[4] / a$n[2]),
                                     (100 * b$n[7] / a$n[3]),
                                     (100 * c$n[1] / sum(c$n)))))

# Positive partners
d.pos.part.casual <- cbind(cbind(c(b$n[2], b$n[5], b$n[8], c$n[2])),
                         cbind(rbind((100 * b$n[2] / a$n[1]),
                                     (100 * b$n[5] / a$n[2]),
                                     (100 * b$n[8] / a$n[3]),
                                     (100 * c$n[2] / sum(c$n)))))

# HIV unknown partners
d.unk.part.casual <- cbind(cbind(c(b$n[3], b$n[6], b$n[9], c$n[3])),
                         cbind(rbind((100 * b$n[3] / a$n[1]),
                                     (100 * b$n[6] / a$n[2]),
                                     (100 * b$n[9] / a$n[3]),
                                     (100 * c$n[3] / sum(c$n)))))

#### One-off partnerships ####

a <- artnetLong %>% filter(ptype == 3) %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% filter(ptype == 3) %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% filter(ptype == 3) %>% count(p_hiv) #HIV among partners

# HIV distribution among egos
totalHIV.ego.oneoff <- cbind(cbind(c("Negative", "Positive", "Unknown", "Total")),
                           cbind(c(a$n[1], a$n[2], a$n[3], sum(a$n))),
                           cbind(rbind((100 * a$n[1] / sum(a$n)),
                                       (100 * a$n[2] / sum(a$n)),
                                       (100 * a$n[3] / sum(a$n)),
                                       (100 * sum(a$n) / sum(a$n)))))

# Negative partners
d.neg.part.oneoff <- cbind(cbind(c(b$n[1], b$n[4], b$n[7], c$n[1])),
                         cbind(rbind((100 * b$n[1] / a$n[1]),
                                     (100 * b$n[4] / a$n[2]),
                                     (100 * b$n[7] / a$n[3]),
                                     (100 * c$n[1] / sum(c$n)))))

# Positive partners
d.pos.part.oneoff <- cbind(cbind(c(b$n[2], b$n[5], b$n[8], c$n[2])),
                         cbind(rbind((100 * b$n[2] / a$n[1]),
                                     (100 * b$n[5] / a$n[2]),
                                     (100 * b$n[8] / a$n[3]),
                                     (100 * c$n[2] / sum(c$n)))))

# HIV unknown partners
d.unk.part.oneoff <- cbind(cbind(c(b$n[3], b$n[6], b$n[9], c$n[3])),
                         cbind(rbind((100 * b$n[3] / a$n[1]),
                                     (100 * b$n[6] / a$n[2]),
                                     (100 * b$n[9] / a$n[3]),
                                     (100 * c$n[3] / sum(c$n)))))



#### Creating Table 2b ####
table2b.title <- c("HIV Serosorting, stratified by partnership type, crude ARTnet data", "", "", "", "", "", "", "", "")
table2b.title.main <- c("Main Partnerships", "", "", "Alters", "", "", "", "", "")
table2b.title.casual <- c("Casual Partnerships", "", "", "Alters", "", "", "", "", "")
table2b.title.oneoff <- c("One-off Partnerships", "", "", "Alters", "", "", "", "", "")
table2b.colnames <- c("Egos", "Total N", "%", "Negative n", "%", "Positive n", "%", "Unknown n", "%")


table2b <- rbind(table2b.title, 
                 table2b.title.main,
                 table2b.colnames,
                 cbind(totalHIV.ego.main, d.neg.part.main, d.pos.part.main, d.unk.part.main),
                 table2b.title.casual,
                 table2b.colnames,
                 cbind(totalHIV.ego.casual, d.neg.part.casual, d.pos.part.casual, d.unk.part.casual),
                 table2b.title.oneoff,
                 table2b.colnames,
                 cbind(totalHIV.ego.oneoff, d.neg.part.oneoff, d.pos.part.oneoff, d.unk.part.oneoff))
write.csv(table2b, file = "aim1_table2b.csv")
