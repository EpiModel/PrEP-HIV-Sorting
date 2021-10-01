# Table 2: Mixing with unknown values included

# rm(list = ls())
# source('~/GitHub/PrEP-HIV-Sorting/3 Analysis.R')


t2.title <- vector("character", 9)
t2.title[1] <- "Table 2. HIV and PrEP mixing including unknown values"

t2.colname.a <- vector("character", 9) 
t2.colname.a[4] <- "Partners"

## HIV mixing 

t2.colname.b <- c("", "Total, Partnership-level", "", "HIV negative", "", "HIV positive", "", "HIV unknown", "")
t2.colname.c <- c("Respondents", "N", "Column %", "N", "Row %", "N", "Row %", "N", "Row %")

a <- artnetSort %>% count(hiv3) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(hiv3 == "Neg") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(hiv3 == "Pos") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(hiv3 == "Unk") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))
e <- artnetSort %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))

t2.hh <- rbind(
        cbind("All egos and partners", "", "", "", "", "", "", "", ""),
        cbind("Negative", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], b$n[3], b$perc[3]),
        cbind("Positive", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], c$n[3], c$perc[3]),
        cbind("Unknown",  a$n[3], a$perc[3], d$n[1], d$perc[1], d$n[2], d$perc[2], d$n[3], d$perc[3]),
        cbind("Total", sum(a$n), sum(a$perc), e$n[1], e$perc[1], e$n[2], e$perc[2], e$n[3], e$perc[3])
)

## HIV/PrEP mixing - partners are HIV -/?

t2.colname.d <- c("", "Total, partnership-level", "", "No PrEP", "", "PrEP", "", "PrEP unknown", "")
t2.colname.e <- c("Respondents", "N", "Column %", "N", "Row %", "N", "Row %", "N", "Row %")

a <- artnetSort %>% filter(!p_hiv == "Pos") %>% count(hiv3) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(!p_hiv == "Pos", hiv3 == "Neg") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(!p_hiv == "Pos", hiv3 == "Pos") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(!p_hiv == "Pos", hiv3 == "Unk") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
e <- artnetSort %>% filter(!p_hiv == "Pos") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))

t2.hp <- rbind(
        cbind("All egos, partners are HIV negative or unknown", "", "", "", "", "", "", "", ""),
        cbind("Negative", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], b$n[3], b$perc[3]),
        cbind("Positive", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], c$n[3], c$perc[3]),
        cbind("Unknown",  a$n[3], a$perc[3], d$n[1], d$perc[1], d$n[2], d$perc[2], d$n[3], d$perc[3]),
        cbind("Total", sum(a$n), sum(a$perc), e$n[1], e$perc[1], e$n[2], e$perc[2], e$n[3], e$perc[3])
)        

## PrEP-PrEP mixing - egos and partners are HIV -/?

t2.colname.f <- c("", "Total, partnership-level", "", "No PrEP", "", "PrEP", "", "PrEP unknown", "")
t2.colname.g <- c("Respondents", "N", "Column %", "N", "Row %", "N", "Row %", "N", "Row %")

a <- artnetSort %>% filter(!p_hiv == "Pos", hiv2 == 0) %>% count(prep.during.ego2) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(!p_hiv == "Pos", hiv2 == 0, prep.during.ego2 == "No") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(!p_hiv == "Pos", hiv2 == 0, prep.during.ego2 == "Yes") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(!p_hiv == "Pos", hiv2 == 0) %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))

t2.pp <- rbind(
        cbind("Egos & Partners HIV negative or unknown", "", "", "", "", "", "", "", ""),
        cbind("No PrEP", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], b$n[3], b$perc[3]),
        cbind("PrEP", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], c$n[3], c$perc[3]),
        cbind("Total", sum(a$n), sum(a$perc), d$n[1], d$perc[1], d$n[2], d$perc[2], d$n[3], d$perc[3])
)

## Assemble
table2 <- rbind(t2.title, t2.colname.a, 
                t2.colname.b, t2.colname.c, t2.hh, 
                t2.colname.d, t2.colname.e, t2.hp, 
                t2.colname.f, t2.colname.g, t2.pp)

write.csv(table2, file = "aim1_table2.csv")

