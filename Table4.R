rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

#### Table 4a - PrEP mixing with unknown values ----------------------------------

# Ego == No PrEP
a <- artnetLong %>% 
        filter(d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP" | d_hivprep == "NOP-PUK" | d_hivprep == "NOP-POS" | d_hivprep == "NOP-UNK") %>%
        count(d_hivprep)  

# Ego == PrEP
b <- artnetLong %>% 
        filter(d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP" | d_hivprep == "YEP-PUK" | d_hivprep == "YEP-POS" | d_hivprep == "YEP-UNK") %>%
        count(d_hivprep) 

# Ego == HIV+
c <- artnetLong %>% 
        filter(d_hivprep == "POS-NOP" | d_hivprep == "POS-YEP" | d_hivprep == "POS-PUK" | d_hivprep == "POS-POS" | d_hivprep == "POS-UNK") %>%
        count(d_hivprep) 

# Ego == HIV?
d <- artnetLong %>% 
        filter(d_hivprep == "UNK-NOP" | d_hivprep == "UNK-YEP" | d_hivprep == "UNK-PUK" | d_hivprep == "UNK-POS" | d_hivprep == "UNK-UNK") %>%
        count(d_hivprep)

# Partners, HIV(-) No PrEP

d.nop.part <- cbind(cbind(c("No PrEP", "PrEP", "HIV+", "HIV?")),
                    cbind(c(a$n[1], b$n[1], c$n[1], d$n[1])),
                    cbind(rbind((100 * a$n[1] / sum(a$n)),
                                (100 * b$n[1] / sum(b$n)),
                                (100 * c$n[1] / sum(c$n)),
                                (100 * d$n[1] / sum(d$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part <- cbind(cbind(c(a$n[5], b$n[5], c$n[5], d$n[5])),
                    cbind(rbind((100 * a$n[5] / sum(a$n)),
                                (100 * b$n[5] / sum(b$n)),
                                (100 * c$n[5] / sum(c$n)),
                                (100 * d$n[5] / sum(d$n)))))

# Partners, HIV(-) PrEP Unknown

d.puk.part <- cbind(cbind(c(a$n[3], b$n[3], c$n[3], d$n[3])),
                    cbind(rbind((100 * a$n[3] / sum(a$n)),
                                (100 * b$n[3] / sum(b$n)),
                                (100 * c$n[3] / sum(c$n)),
                                (100 * d$n[3] / sum(d$n)))))

# Partners, HIV(+)

d.pos.part <- cbind(cbind(c(a$n[2], b$n[2], c$n[2], d$n[2])),
                    cbind(rbind((100 * a$n[2] / sum(a$n)),
                                (100 * b$n[2] / sum(b$n)),
                                (100 * c$n[2] / sum(c$n)),
                                (100 * d$n[2] / sum(d$n)))))

#Partners, HIV(?)

d.unk.part <- cbind(cbind(c(a$n[4], b$n[4], c$n[4], d$n[4])),
                    cbind(rbind((100 * a$n[4] / sum(a$n)),
                                (100 * b$n[4] / sum(b$n)),
                                (100 * c$n[4] / sum(c$n)),
                                (100 * d$n[4] / sum(d$n)))))

table4a.title <- c("HIV-PrEP Sorting", "Alters", "", "", "", "", "", "", "", "", "")
table4a.colnames <- c("Egos", "No PrEP n", "%", "PrEP n", "%", "PrEP Unknown n", "%", "HIV+ n", "%", "HIV? n", "%")

table4a <- rbind(table4a.title, table4a.colnames,
                 cbind(d.nop.part, d.yep.part, d.puk.part, d.pos.part, d.unk.part))
write.csv(table4a, file = "aim1_table4a.csv")


#### Table 4b - PrEP mixing with unknown values, stratified by partnership type ----------------------------------

#### Main Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP" | d_hivprep == "NOP-PUK" | d_hivprep == "NOP-POS" | d_hivprep == "NOP-UNK")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP" | d_hivprep == "YEP-PUK" | d_hivprep == "YEP-POS" | d_hivprep == "YEP-UNK")) %>%
        count(d_hivprep) 

# Ego == HIV+
c <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "POS-NOP" | d_hivprep == "POS-YEP" | d_hivprep == "POS-PUK" | d_hivprep == "POS-POS" | d_hivprep == "POS-UNK")) %>%
        count(d_hivprep) 

# Ego == HIV?
d <- artnetLong %>% 
        filter(ptype == 1 & (d_hivprep == "UNK-NOP" | d_hivprep == "UNK-YEP" | d_hivprep == "UNK-PUK" | d_hivprep == "UNK-POS" | d_hivprep == "UNK-UNK")) %>%
        count(d_hivprep)

# Partners, HIV(-) No PrEP

d.nop.part.main <- cbind(cbind(c("No PrEP", "PrEP", "HIV+", "HIV?")),
                    cbind(c(a$n[1], b$n[1], c$n[1], d$n[1])),
                    cbind(rbind((100 * a$n[1] / sum(a$n)),
                                (100 * b$n[1] / sum(b$n)),
                                (100 * c$n[1] / sum(c$n)),
                                (100 * d$n[1] / sum(d$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.main <- cbind(cbind(c(a$n[5], b$n[5], c$n[5], d$n[5])),
                    cbind(rbind((100 * a$n[5] / sum(a$n)),
                                (100 * b$n[5] / sum(b$n)),
                                (100 * c$n[5] / sum(c$n)),
                                (100 * d$n[5] / sum(d$n)))))

# Partners, HIV(-) PrEP Unknown

d.puk.part.main <- cbind(cbind(c(a$n[3], b$n[3], c$n[3], d$n[3])),
                    cbind(rbind((100 * a$n[3] / sum(a$n)),
                                (100 * b$n[3] / sum(b$n)),
                                (100 * c$n[3] / sum(c$n)),
                                (100 * d$n[3] / sum(d$n)))))

# Partners, HIV(+)

d.pos.part.main <- cbind(cbind(c(a$n[2], b$n[2], c$n[2], d$n[2])),
                    cbind(rbind((100 * a$n[2] / sum(a$n)),
                                (100 * b$n[2] / sum(b$n)),
                                (100 * c$n[2] / sum(c$n)),
                                (100 * d$n[2] / sum(d$n)))))

#Partners, HIV(?)

d.unk.part.main <- cbind(cbind(c(a$n[4], b$n[4], c$n[4], d$n[4])),
                    cbind(rbind((100 * a$n[4] / sum(a$n)),
                                (100 * b$n[4] / sum(b$n)),
                                (100 * c$n[4] / sum(c$n)),
                                (100 * d$n[4] / sum(d$n)))))

#### Casual Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP" | d_hivprep == "NOP-PUK" | d_hivprep == "NOP-POS" | d_hivprep == "NOP-UNK")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP" | d_hivprep == "YEP-PUK" | d_hivprep == "YEP-POS" | d_hivprep == "YEP-UNK")) %>%
        count(d_hivprep) 

# Ego == HIV+
c <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "POS-NOP" | d_hivprep == "POS-YEP" | d_hivprep == "POS-PUK" | d_hivprep == "POS-POS" | d_hivprep == "POS-UNK")) %>%
        count(d_hivprep) 

# Ego == HIV?
d <- artnetLong %>% 
        filter(ptype == 2 & (d_hivprep == "UNK-NOP" | d_hivprep == "UNK-YEP" | d_hivprep == "UNK-PUK" | d_hivprep == "UNK-POS" | d_hivprep == "UNK-UNK")) %>%
        count(d_hivprep)

# Partners, HIV(-) No PrEP

d.nop.part.casual <- cbind(cbind(c("No PrEP", "PrEP", "HIV+", "HIV?")),
                         cbind(c(a$n[1], b$n[1], c$n[1], d$n[1])),
                         cbind(rbind((100 * a$n[1] / sum(a$n)),
                                     (100 * b$n[1] / sum(b$n)),
                                     (100 * c$n[1] / sum(c$n)),
                                     (100 * d$n[1] / sum(d$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.casual <- cbind(cbind(c(a$n[5], b$n[5], c$n[5], d$n[5])),
                         cbind(rbind((100 * a$n[5] / sum(a$n)),
                                     (100 * b$n[5] / sum(b$n)),
                                     (100 * c$n[5] / sum(c$n)),
                                     (100 * d$n[5] / sum(d$n)))))

# Partners, HIV(-) PrEP Unknown

d.puk.part.casual <- cbind(cbind(c(a$n[3], b$n[3], c$n[3], d$n[3])),
                         cbind(rbind((100 * a$n[3] / sum(a$n)),
                                     (100 * b$n[3] / sum(b$n)),
                                     (100 * c$n[3] / sum(c$n)),
                                     (100 * d$n[3] / sum(d$n)))))

# Partners, HIV(+)

d.pos.part.casual <- cbind(cbind(c(a$n[2], b$n[2], c$n[2], d$n[2])),
                         cbind(rbind((100 * a$n[2] / sum(a$n)),
                                     (100 * b$n[2] / sum(b$n)),
                                     (100 * c$n[2] / sum(c$n)),
                                     (100 * d$n[2] / sum(d$n)))))

#Partners, HIV(?)

d.unk.part.casual <- cbind(cbind(c(a$n[4], b$n[4], c$n[4], d$n[4])),
                         cbind(rbind((100 * a$n[4] / sum(a$n)),
                                     (100 * b$n[4] / sum(b$n)),
                                     (100 * c$n[4] / sum(c$n)),
                                     (100 * d$n[4] / sum(d$n)))))

#### One-off Partnerships ####

# Ego == No PrEP
a <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "NOP-NOP" | d_hivprep == "NOP-YEP" | d_hivprep == "NOP-PUK" | d_hivprep == "NOP-POS" | d_hivprep == "NOP-UNK")) %>%
        count(d_hivprep)

# Ego == PrEP
b <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "YEP-NOP" | d_hivprep == "YEP-YEP" | d_hivprep == "YEP-PUK" | d_hivprep == "YEP-POS" | d_hivprep == "YEP-UNK")) %>%
        count(d_hivprep) 

# Ego == HIV+
c <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "POS-NOP" | d_hivprep == "POS-YEP" | d_hivprep == "POS-PUK" | d_hivprep == "POS-POS" | d_hivprep == "POS-UNK")) %>%
        count(d_hivprep) 

# Ego == HIV?
d <- artnetLong %>% 
        filter(ptype == 3 & (d_hivprep == "UNK-NOP" | d_hivprep == "UNK-YEP" | d_hivprep == "UNK-PUK" | d_hivprep == "UNK-POS" | d_hivprep == "UNK-UNK")) %>%
        count(d_hivprep)

# Partners, HIV(-) No PrEP

d.nop.part.oneoff <- cbind(cbind(c("No PrEP", "PrEP", "HIV+", "HIV?")),
                         cbind(c(a$n[1], b$n[1], c$n[1], d$n[1])),
                         cbind(rbind((100 * a$n[1] / sum(a$n)),
                                     (100 * b$n[1] / sum(b$n)),
                                     (100 * c$n[1] / sum(c$n)),
                                     (100 * d$n[1] / sum(d$n)))))

# Partners, HIV(-) Yes PrEP

d.yep.part.oneoff <- cbind(cbind(c(a$n[5], b$n[5], c$n[5], d$n[5])),
                         cbind(rbind((100 * a$n[5] / sum(a$n)),
                                     (100 * b$n[5] / sum(b$n)),
                                     (100 * c$n[5] / sum(c$n)),
                                     (100 * d$n[5] / sum(d$n)))))

# Partners, HIV(-) PrEP Unknown

d.puk.part.oneoff <- cbind(cbind(c(a$n[3], b$n[3], c$n[3], d$n[3])),
                         cbind(rbind((100 * a$n[3] / sum(a$n)),
                                     (100 * b$n[3] / sum(b$n)),
                                     (100 * c$n[3] / sum(c$n)),
                                     (100 * d$n[3] / sum(d$n)))))

# Partners, HIV(+)

d.pos.part.oneoff <- cbind(cbind(c(a$n[2], b$n[2], c$n[2], d$n[2])),
                         cbind(rbind((100 * a$n[2] / sum(a$n)),
                                     (100 * b$n[2] / sum(b$n)),
                                     (100 * c$n[2] / sum(c$n)),
                                     (100 * d$n[2] / sum(d$n)))))

#Partners, HIV(?)

d.unk.part.oneoff <- cbind(cbind(c(a$n[4], b$n[4], c$n[4], d$n[4])),
                         cbind(rbind((100 * a$n[4] / sum(a$n)),
                                     (100 * b$n[4] / sum(b$n)),
                                     (100 * c$n[4] / sum(c$n)),
                                     (100 * d$n[4] / sum(d$n)))))

#### Creating Table 4b ####

table4b.title <- c("HIV-PrEP Sorting, stratified by partnership type", "", "", "", "", "", "", "", "", "", "")
table4b.title.main <- c("Main Partnerships", "Alters", "", "", "", "", "", "", "", "", "")
table4b.title.casual <- c("Casual Partnerships", "Alters", "", "", "", "", "", "", "", "", "")
table4b.title.oneoff <- c("One-off Partnerships", "Alters", "", "", "", "", "", "", "", "", "")
table4b.colnames <- c("Egos", "No PrEP n", "%", "PrEP n", "%", "PrEP Unknown n", "%", "HIV+ n", "%", "HIV? n", "%")

table4b <- rbind(table4b.title, 
                 table4b.title.main,
                 table4b.colnames,
                 cbind(d.nop.part.main, d.yep.part.main, d.puk.part.main, d.pos.part.main, d.unk.part.main),
                 table4b.title.casual,
                 table4b.colnames,
                 cbind(d.nop.part.casual, d.yep.part.casual, d.puk.part.casual, d.pos.part.casual, d.unk.part.casual),
                 table4b.title.oneoff,
                 table4b.colnames,
                 cbind(d.nop.part.oneoff, d.yep.part.oneoff, d.puk.part.oneoff, d.pos.part.oneoff, d.unk.part.oneoff))
write.csv(table4b, file = "aim1_table4b.csv")
