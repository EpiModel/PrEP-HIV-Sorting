# Table 3: HIV-HIV Mixing
# Required: ~/GitHub/PrEP-HIV-Sorting/Analysis.R
# By Kevin Maloney (kevin.maloney@emory.edu)
# 2020-11-18

rm(list = ls())

artnetSort <- artnetSort %>% filter(ptype == "Main" | ptype == "Casual")

t3.title <- vector("character", 9)
t3.title[1] <- "Table 3a. HIV-HIV Sorting - Persistent Partners"

t3.colname.a <- vector("character", 9) 
t3.colname.a[4] <- "Partners"

t3.colname.b <- c("", "Total, dyad-level", "", "HIV negative", "", "HIV positive", "", "HIV unknown", "")
t3.colname.c <- c("Respondents", "N", "Column %", "N", "Row %", "N", "Row %", "N", "Row %")

# Whole sample
a <- artnetSort %>% count(hiv3) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(hiv3 == "Neg") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(hiv3 == "Pos") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(hiv3 == "Unk") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))
e <- artnetSort %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))

t3.w <- rbind(
        cbind("Whole Sample", "", "", "", "", "", "", "", ""),
        cbind("Negative", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], b$n[3], b$perc[3]),
        cbind("Positive", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], c$n[3], c$perc[3]),
        cbind("Unknown",  a$n[3], a$perc[3], d$n[1], d$perc[1], d$n[2], d$perc[2], d$n[3], d$perc[3]),
        cbind("Total", sum(a$n), sum(a$perc), e$n[1], e$perc[1], e$n[2], e$perc[2], e$n[3], e$perc[3])
        )        

# Complete Case
a <- artnetSort %>% filter(!p_hiv == "Unk", !hiv3 == "Unk") %>% count(hiv3) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(!p_hiv == "Unk", hiv3 == "Neg") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(!p_hiv == "Unk", hiv3 == "Pos") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(!p_hiv == "Unk", !hiv3 == "Unk") %>% count(p_hiv) %>% mutate(perc = round(100*n/sum(n), 1))

t3.c <- rbind(
        cbind("Complete-case", "", "", "", "", "", "", "", ""),
        cbind("Negative", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], "", ""),
        cbind("Positive", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], "", ""),
        cbind("Total", sum(a$n), sum(a$perc), d$n[1], d$perc[1], d$n[2], d$perc[2], "", "")
)

# Reclassification
a <- artnetSort %>% count(hiv2) %>% mutate(perc = round(100*n/sum(n), 1))
b <- as.data.frame(results(dat = reclass.results, x = "hh.sort.n"))
c <- as.data.frame(results(dat = reclass.results, x = "hh.sort.p"))
d <- as.data.frame(results(dat = reclass.results, x = "hiv.imp.n"))
e <- as.data.frame(results(dat = reclass.results, x = "hiv.imp.p"))

t3.r <- rbind(
        cbind("Reclassification", "", "", "", "", "", "", "", ""),
        cbind("Negative/Unknown", a$n[1], a$perc[1], "", "", "", "", "", ""),
        cbind("Median", "", "", round(b[1,2]), round(100*c[1,2],1), round(b[3,2]), round(100*c[3,2],1), "", ""),
        cbind("95% interval", "", "", paste0("(",round(b[1,1]),", ", round(b[1,3]), ")"), paste0("(",round(100*c[1,1],1),", ",round(100*c[1,3],1), ")"), paste0("(",round(b[3,1]),", ", round(b[3,3]), ")"), paste0("(",round(100*c[3,1],1),", ",round(100*c[3,3],1), ")"), "", ""),
        cbind("Positive", a$n[2], a$perc[2], "", "", "", "", "", ""),
        cbind("Median", "", "", round(b[2,2]), round(100*c[2,2],1), round(b[4,2]), round(100*c[4,2],1), "", ""),
        cbind("95% interval", "", "", paste0("(",round(b[2,1]),", ", round(b[2,3]), ")"), paste0("(",round(100*c[2,1],1),", ",round(100*c[2,3],1), ")"), paste0("(",round(b[4,1]),", ", round(b[4,3]), ")"), paste0("(",round(100*c[4,1],1),", ",round(100*c[4,3],1), ")"), "", ""),
        cbind("Total", sum(a$n), sum(a$perc), "", "", "", "", "", ""),
        cbind("Median", "", "", round(d[1,2]), round(100*e[1,2],1), round(d[2,2]), round(100*e[2,2],1), "", ""),
        cbind("95% interval", "", "", paste0("(",round(d[1,1]),", ", round(d[1,3]), ")"), paste0("(",round(100*e[1,1],1),", ",round(100*e[1,3],1), ")"), paste0("(",round(d[2,1]),", ", round(d[2,3]), ")"), paste0("(",round(100*e[2,1],1),", ",round(100*e[2,3],1), ")"), "", "")        
)

table3 <- rbind(t3.title, t3.colname.a, t3.colname.b, t3.colname.c, t3.w, t3.c, t3.r)

write.csv(table3, file = "aim1_table3a_HH_pers.csv")

# table(artnetSort$hiv3, artnetSort$p_hiv, useNA = "ifany")
# prop.table(table(artnetSort$hiv3, artnetSort$p_hiv, useNA = "ifany"), 1)
