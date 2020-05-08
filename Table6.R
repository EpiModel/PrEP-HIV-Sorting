rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

#### Table 6a - PrEP mixing among HIV(-) ----------------------------------

# Ego == No PrEP
a <- artnetLong %>% 
        filter(d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP") %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP") %>%
        count(d_hivprep)

# Partners, HIV(-) No PrEP

d.nop.part <- cbind(cbind(c("No PrEP", "PrEP")),
                    cbind(c(a$n[1], b$n[1])),
                    cbind(rbind((100 * a$n[1] / sum(a$n)),
                                (100 * b$n[1] / sum(b$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part <- cbind(cbind(c(a$n[2], b$n[2])),
                    cbind(rbind((100 * a$n[2] / sum(a$n)),
                                (100 * b$n[2] / sum(b$n)))))

table6a.title <- c("PrEP Sorting", "Alters", "", "", "")
table6a.colnames <- c("Egos", "No PrEP n", "%", "PrEP n", "%")

table6a <- rbind(table6a.title, table6a.colnames,
                 cbind(d.nop.part, d.yep.part))
write.csv(table6a, file = "aim1_table6a.csv")

#### Table 5b - PrEP mixing with unknown values, stratified by partnership type ----------------------------------

#### Main Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP")) %>%
        count(d_hivprep) 


# Partners, HIV(-) No PrEP

d.nop.part.main <- cbind(cbind(c("No PrEP", "PrEP")),
                         cbind(c(a$n[1], b$n[1])),
                         cbind(rbind((100 * a$n[1] / sum(a$n)),
                                     (100 * b$n[1] / sum(b$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.main <- cbind(cbind(c(a$n[2], b$n[2])),
                         cbind(rbind((100 * a$n[2] / sum(a$n)),
                                     (100 * b$n[2] / sum(b$n)))))

#### Casual Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP")) %>%
        count(d_hivprep) 

# Partners, HIV(-) No PrEP

d.nop.part.casual <- cbind(cbind(c("No PrEP", "PrEP")),
                           cbind(c(a$n[1], b$n[1])),
                           cbind(rbind((100 * a$n[1] / sum(a$n)),
                                       (100 * b$n[1] / sum(b$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.casual <- cbind(cbind(c(a$n[2], b$n[2])),
                           cbind(rbind((100 * a$n[2] / sum(a$n)),
                                       (100 * b$n[2] / sum(b$n)))))

#### One-off Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP")) %>%
        count(d_hivprep) 

# Partners, HIV(-) No PrEP

d.nop.part.oneoff <- cbind(cbind(c("No PrEP", "PrEP")),
                           cbind(c(a$n[1], b$n[1])),
                           cbind(rbind((100 * a$n[1] / sum(a$n)),
                                       (100 * b$n[1] / sum(b$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.oneoff <- cbind(cbind(c(a$n[2], b$n[2])),
                           cbind(rbind((100 * a$n[2] / sum(a$n)),
                                       (100 * b$n[2] / sum(b$n)))))

#### Creating Table 6b ####

table6b.title <- c("PrEP Sorting among HIV (-), stratified by partnership type", "", "", "", "")
table6b.title.main <- c("Main Partnerships", "Alters", "", "", "")
table6b.title.casual <- c("Casual Partnerships", "Alters", "", "", "")
table6b.title.oneoff <- c("One-off Partnerships", "Alters", "", "", "")
table6b.colnames <- c("Egos", "No PrEP n", "%", "PrEP n", "%")

table6b <- rbind(table6b.title, 
                 table6b.title.main,
                 table6b.colnames,
                 cbind(d.nop.part.main, d.yep.part.main),
                 table6b.title.casual,
                 table6b.colnames,
                 cbind(d.nop.part.casual, d.yep.part.casual),
                 table6b.title.oneoff,
                 table6b.colnames,
                 cbind(d.nop.part.oneoff, d.yep.part.oneoff))
write.csv(table6b, file = "aim1_table6b.csv")
