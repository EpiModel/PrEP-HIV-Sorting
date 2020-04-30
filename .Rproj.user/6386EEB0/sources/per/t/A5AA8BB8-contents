source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

#### Table 3a - Partnership pairings with NO UNKNOWN ####

a <- artnetLong %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% count(p_hiv) #HIV among partners

# HIV distribution among egos
totalHIV.ego.known <- cbind(cbind(c("Negative", "Positive", "Total")),
                            cbind(c((b$n[1] + b$n[2]), (b$n[4] + b$n[5]), (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                  cbind(rbind((100 * (b$n[1] + b$n[2]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                              (100 * (b$n[4] + b$n[5]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                              (100 * (b$n[1] + b$n[2] + b$n[4] + b$n[5]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5]))))))

# Negative partners
d.neg.part.known <- cbind(cbind(c(b$n[1], b$n[4], (b$n[1] + b$n[4]))),
                          cbind(rbind((100 * b$n[1] / (b$n[1] + b$n[2])),
                                      (100 * b$n[4] / (b$n[4] + b$n[5])),
                                      (100 * (b$n[1] + b$n[4]) / (b$n[1] + b$n[4] + b$n[2] + b$n[5])))))

# Positive partners
d.pos.part.known <- cbind(cbind(c(b$n[2], b$n[5], (b$n[2] + b$n[5]))),
                          cbind(rbind((100 * b$n[2] / (b$n[1] + b$n[2])),
                                      (100 * b$n[5] / (b$n[4] + b$n[5])),
                                      (100 * (b$n[2] + b$n[5]) / (b$n[1] + b$n[4] + b$n[2] + b$n[5])))))


table3a.title <- c("HIV serosorting, conditional on HIV reported Negative or Positive", "", "", "Alters", "", "", "")
table3a.colnames <- c("Egos", "Total N", "%", "Negative n", "%", "Positive n", "%")

table3a <- rbind(table3a.title, table3a.colnames,
                 cbind(totalHIV.ego.known, d.neg.part.known, d.pos.part.known))

write.csv(table3a, file = "aim1_table3a .csv")

#### Table 3b - Partnership pairings with NO UNKNOWN, stratified by partnership type ####

#### Main partnerships ####

a <- artnetLong %>% filter(ptype == 1) %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% filter(ptype == 1) %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% filter(ptype == 1) %>% count(p_hiv) #HIV among partners

# HIV distribution among egos
totalHIV.ego.known.main <- cbind(cbind(c("Negative", "Positive", "Total")),
                            cbind(c((b$n[1] + b$n[2]), (b$n[4] + b$n[5]), (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                  cbind(rbind((100 * (b$n[1] + b$n[2]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                              (100 * (b$n[4] + b$n[5]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                              (100 * (b$n[1] + b$n[2] + b$n[4] + b$n[5]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5]))))))

# Negative partners
d.neg.part.known.main <- cbind(cbind(c(b$n[1], b$n[4], (b$n[1] + b$n[4]))),
                          cbind(rbind((100 * b$n[1] / (b$n[1] + b$n[2])),
                                      (100 * b$n[4] / (b$n[4] + b$n[5])),
                                      (100 * (b$n[1] + b$n[4]) / (b$n[1] + b$n[4] + b$n[2] + b$n[5])))))

# Positive partners
d.pos.part.known.main <- cbind(cbind(c(b$n[2], b$n[5], (b$n[2] + b$n[5]))),
                          cbind(rbind((100 * b$n[2] / (b$n[1] + b$n[2])),
                                      (100 * b$n[5] / (b$n[4] + b$n[5])),
                                      (100 * (b$n[2] + b$n[5]) / (b$n[1] + b$n[4] + b$n[2] + b$n[5])))))

#### Casual partnerships ####

a <- artnetLong %>% filter(ptype == 2) %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% filter(ptype == 2) %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% filter(ptype == 2) %>% count(p_hiv) #HIV among partners

# HIV distribution among egos
totalHIV.ego.known.casual <- cbind(cbind(c("Negative", "Positive", "Total")),
                                 cbind(c((b$n[1] + b$n[2]), (b$n[4] + b$n[5]), (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                       cbind(rbind((100 * (b$n[1] + b$n[2]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                                   (100 * (b$n[4] + b$n[5]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                                   (100 * (b$n[1] + b$n[2] + b$n[4] + b$n[5]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5]))))))

# Negative partners
d.neg.part.known.casual <- cbind(cbind(c(b$n[1], b$n[4], (b$n[1] + b$n[4]))),
                               cbind(rbind((100 * b$n[1] / (b$n[1] + b$n[2])),
                                           (100 * b$n[4] / (b$n[4] + b$n[5])),
                                           (100 * (b$n[1] + b$n[4]) / (b$n[1] + b$n[4] + b$n[2] + b$n[5])))))

# Positive partners
d.pos.part.known.casual <- cbind(cbind(c(b$n[2], b$n[5], (b$n[2] + b$n[5]))),
                               cbind(rbind((100 * b$n[2] / (b$n[1] + b$n[2])),
                                           (100 * b$n[5] / (b$n[4] + b$n[5])),
                                           (100 * (b$n[2] + b$n[5]) / (b$n[1] + b$n[4] + b$n[2] + b$n[5])))))

#### One-off partnerships ####

a <- artnetLong %>% filter(ptype == 3) %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% filter(ptype == 3) %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% filter(ptype == 3) %>% count(p_hiv) #HIV among partners

# HIV distribution among egos
totalHIV.ego.known.oneoff <- cbind(cbind(c("Negative", "Positive", "Total")),
                                 cbind(c((b$n[1] + b$n[2]), (b$n[4] + b$n[5]), (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                       cbind(rbind((100 * (b$n[1] + b$n[2]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                                   (100 * (b$n[4] + b$n[5]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5])),
                                                   (100 * (b$n[1] + b$n[2] + b$n[4] + b$n[5]) / (b$n[1] + b$n[2] + b$n[4] + b$n[5]))))))

# Negative partners
d.neg.part.known.oneoff <- cbind(cbind(c(b$n[1], b$n[4], (b$n[1] + b$n[4]))),
                               cbind(rbind((100 * b$n[1] / (b$n[1] + b$n[2])),
                                           (100 * b$n[4] / (b$n[4] + b$n[5])),
                                           (100 * (b$n[1] + b$n[4]) / (b$n[1] + b$n[4] + b$n[2] + b$n[5])))))

# Positive partners
d.pos.part.known.oneoff <- cbind(cbind(c(b$n[2], b$n[5], (b$n[2] + b$n[5]))),
                               cbind(rbind((100 * b$n[2] / (b$n[1] + b$n[2])),
                                           (100 * b$n[5] / (b$n[4] + b$n[5])),
                                           (100 * (b$n[2] + b$n[5]) / (b$n[1] + b$n[4] + b$n[2] + b$n[5])))))

#### Creating Table 3b ####
table3b.title <- c("HIV Serosorting, conditional on reporting Negative or Positive stratified by partnership type, crude ARTnet data", "", "", "", "", "", "")
table3b.title.main <- c("Main Partnerships", "", "", "Alters", "", "", "")
table3b.title.casual <- c("Casual Partnerships", "", "", "Alters", "", "", "")
table3b.title.oneoff <- c("One-off Partnerships", "", "", "Alters", "", "", "")
table3b.colnames <- c("Egos", "Total N", "%", "Negative n", "%", "Positive n", "%")


table3b <- rbind(table3b.title, 
                 table3b.title.main,
                 table3b.colnames,
                 cbind(totalHIV.ego.known.main, d.neg.part.known.main, d.pos.part.known.main),
                 table3b.title.casual,
                 table3b.colnames,
                 cbind(totalHIV.ego.known.casual, d.neg.part.known.casual, d.pos.part.known.casual),
                 table3b.title.oneoff,
                 table3b.colnames,
                 cbind(totalHIV.ego.known.oneoff, d.neg.part.known.oneoff, d.pos.part.known.oneoff))
write.csv(table3b, file = "aim1_table3b.csv")
