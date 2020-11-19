rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

#### Table 5a - PrEP mixing with unknown values ----------------------------------

# Ego == No PrEP
a <- artnetLong %>% 
        filter(d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP" | d_hivprep == "NOP-POS") %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP" | d_hivprep == "YEP-POS") %>%
        count(d_hivprep) 

# Ego == HIV+
c <- artnetLong %>% 
        filter(d_hivprep == "POS-NOP" | d_hivprep == "POS-YEP" | d_hivprep == "POS-POS") %>%
        count(d_hivprep) 

# Partners, HIV(-) No PrEP

d.nop.part <- cbind(cbind(c("No PrEP", "PrEP", "HIV+")),
                    cbind(c(a$n[1], b$n[1], c$n[1])),
                    cbind(rbind((100 * a$n[1] / sum(a$n)),
                                (100 * b$n[1] / sum(b$n)),
                                (100 * c$n[1] / sum(c$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part <- cbind(cbind(c(a$n[3], b$n[3], c$n[3])),
                    cbind(rbind((100 * a$n[3] / sum(a$n)),
                                (100 * b$n[3] / sum(b$n)),
                                (100 * c$n[3] / sum(c$n)))))

# Partners, HIV(+)

d.pos.part <- cbind(cbind(c(a$n[2], b$n[2], c$n[2])),
                    cbind(rbind((100 * a$n[2] / sum(a$n)),
                                (100 * b$n[2] / sum(b$n)),
                                (100 * c$n[2] / sum(c$n)))))

table5a.title <- c("HIV-PrEP Sorting, no missing", "Alters", "", "", "", "", "")
table5a.colnames <- c("Egos", "No PrEP n", "%", "PrEP n", "%", "HIV+ n", "%")

table5a <- rbind(table5a.title, table5a.colnames,
                 cbind(d.nop.part, d.yep.part, d.pos.part))
write.csv(table5a, file = "aim1_table5a.csv")


#### Table 5b - PrEP mixing with unknown values, stratified by partnership type ----------------------------------

#### Main Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP" | d_hivprep == "NOP-POS")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP" | d_hivprep == "YEP-POS")) %>%
        count(d_hivprep) 

# Ego == HIV+
c <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "POS-NOP" | d_hivprep == "POS-YEP" | d_hivprep == "POS-POS")) %>%
        count(d_hivprep) 

# Partners, HIV(-) No PrEP

d.nop.part.main <- cbind(cbind(c("No PrEP", "PrEP", "HIV+")),
                         cbind(c(a$n[1], b$n[1], c$n[1])),
                         cbind(rbind((100 * a$n[1] / sum(a$n)),
                                     (100 * b$n[1] / sum(b$n)),
                                     (100 * c$n[1] / sum(c$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.main <- cbind(cbind(c(a$n[3], b$n[3], c$n[3])),
                         cbind(rbind((100 * a$n[3] / sum(a$n)),
                                     (100 * b$n[3] / sum(b$n)),
                                     (100 * c$n[3] / sum(c$n)))))

# Partners, HIV(+)

d.pos.part.main <- cbind(cbind(c(a$n[2], b$n[2], c$n[2])),
                         cbind(rbind((100 * a$n[2] / sum(a$n)),
                                     (100 * b$n[2] / sum(b$n)),
                                     (100 * c$n[2] / sum(c$n)))))

#### Casual Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP" | d_hivprep == "NOP-POS")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP" | d_hivprep == "YEP-POS")) %>%
        count(d_hivprep) 

# Ego == HIV+
c <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "POS-NOP" | d_hivprep == "POS-YEP" | d_hivprep == "POS-POS")) %>%
        count(d_hivprep) 

# Partners, HIV(-) No PrEP

d.nop.part.casual <- cbind(cbind(c("No PrEP", "PrEP", "HIV+")),
                           cbind(c(a$n[1], b$n[1], c$n[1])),
                           cbind(rbind((100 * a$n[1] / sum(a$n)),
                                       (100 * b$n[1] / sum(b$n)),
                                       (100 * c$n[1] / sum(c$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.casual <- cbind(cbind(c(a$n[3], b$n[3], c$n[3])),
                           cbind(rbind((100 * a$n[3] / sum(a$n)),
                                       (100 * b$n[3] / sum(b$n)),
                                       (100 * c$n[3] / sum(c$n)))))

# Partners, HIV(+)

d.pos.part.casual <- cbind(cbind(c(a$n[2], b$n[2], c$n[2])),
                           cbind(rbind((100 * a$n[2] / sum(a$n)),
                                       (100 * b$n[2] / sum(b$n)),
                                       (100 * c$n[2] / sum(c$n)))))

#### One-off Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP" | d_hivprep == "NOP-POS")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP" | d_hivprep == "YEP-POS")) %>%
        count(d_hivprep) 

# Ego == HIV+
c <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "POS-NOP" | d_hivprep == "POS-YEP" | d_hivprep == "POS-POS")) %>%
        count(d_hivprep) 

# Partners, HIV(-) No PrEP

d.nop.part.oneoff <- cbind(cbind(c("No PrEP", "PrEP", "HIV+")),
                           cbind(c(a$n[1], b$n[1], c$n[1])),
                           cbind(rbind((100 * a$n[1] / sum(a$n)),
                                       (100 * b$n[1] / sum(b$n)),
                                       (100 * c$n[1] / sum(c$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.oneoff <- cbind(cbind(c(a$n[3], b$n[3], c$n[3])),
                           cbind(rbind((100 * a$n[3] / sum(a$n)),
                                       (100 * b$n[3] / sum(b$n)),
                                       (100 * c$n[3] / sum(c$n)))))

# Partners, HIV(+)

d.pos.part.oneoff <- cbind(cbind(c(a$n[2], b$n[2], c$n[2])),
                           cbind(rbind((100 * a$n[2] / sum(a$n)),
                                       (100 * b$n[2] / sum(b$n)),
                                       (100 * c$n[2] / sum(c$n)))))

#### Creating Table 5b ####

table5b.title <- c("HIV-PrEP Sorting, no missing, stratified by partnership type", "", "", "", "", "", "")
table5b.title.main <- c("Main Partnerships", "Alters", "", "", "", "", "")
table5b.title.casual <- c("Casual Partnerships", "Alters", "", "", "", "", "")
table5b.title.oneoff <- c("One-off Partnerships", "Alters", "", "", "", "", "")
table5b.colnames <- c("Egos", "No PrEP n", "%", "PrEP n", "%", "HIV+ n", "%")

table5b <- rbind(table5b.title, 
                 table5b.title.main,
                 table5b.colnames,
                 cbind(d.nop.part.main, d.yep.part.main, d.pos.part.main),
                 table5b.title.casual,
                 table5b.colnames,
                 cbind(d.nop.part.casual, d.yep.part.casual, d.pos.part.casual),
                 table5b.title.oneoff,
                 table5b.colnames,
                 cbind(d.nop.part.oneoff, d.yep.part.oneoff, d.pos.part.oneoff))
write.csv(table5b, file = "aim1_table5b.csv")
