# Table 5: PrEP-PrEP Mixing
# Required: ~/GitHub/PrEP-HIV-Sorting/Analysis.R
# By Kevin Maloney (kevin.maloney@emory.edu)
# 2020-11-19

rm(list = ls())

artnetSort <- artnetSort %>% filter(ptype == "Main")

t5.title <- vector("character", 9)
t5.title[1] <- "Table 5a. HIV-PrEP Sorting - Persistent Partners"

t5.colname.a <- vector("character", 9) 
t5.colname.a[4] <- "Partners"

t5.colname.b <- c("", "Total, dyad-level", "", "No PrEP", "", "PrEP", "", "PrEP unknown", "")
t5.colname.c <- c("Respondents", "N", "Column %", "N", "Row %", "N", "Row %", "N", "Row %")

# Whole sample - egos and partners are HIV -/?
a <- artnetSort %>% filter(!p_hiv == "Pos", hiv2 == 0) %>% count(prep.during.ego2) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(!p_hiv == "Pos", hiv2 == 0, prep.during.ego2 == "No") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(!p_hiv == "Pos", hiv2 == 0, prep.during.ego2 == "Yes") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(!p_hiv == "Pos", hiv2 == 0) %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))

t5.w <- rbind(
        cbind("Whole Sample", "", "", "", "", "", "", "", ""),
        cbind("No PrEP", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], b$n[3], b$perc[3]),
        cbind("PrEP", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], c$n[3], c$perc[3]),
        cbind("Total", sum(a$n), sum(a$perc), d$n[1], d$perc[1], d$n[2], d$perc[2], d$n[3], d$perc[3])
        )        

# Complete Case - partners are HIV negative and known PrEP use
a <- artnetSort %>% filter(p_hiv == "Neg", !prep.during.part2 == "Unk", hiv2 == 0) %>% count(prep.during.ego2) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(p_hiv == "Neg", !prep.during.part2 == "Unk", hiv2 == 0, prep.during.ego2 == "No") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(p_hiv == "Neg", !prep.during.part2 == "Unk", hiv2 == 0, prep.during.ego2 == "Yes") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(p_hiv == "Neg", !prep.during.part2 == "Unk", hiv2 == 0) %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))

t5.c <- rbind(
        cbind("Complete-case", "", "", "", "", "", "", "", ""),
        cbind("No PrEP", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], "", ""),
        cbind("PrEP", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], "", ""),
        cbind("Total", sum(a$n), sum(a$perc), d$n[1], d$perc[1], d$n[2], d$perc[2], "", "")
)

# Reclassification
a <- results(dat = reclass.results, x = "eprep.pneg.n")
b <- results(dat = reclass.results, x = "eprep.pneg.p")
c <- results(dat = reclass.results, x = "pp.sort.n")
d <- results(dat = reclass.results, x = "pp.sort.p")
e <- results(dat = reclass.results, x = "ehiv.pneg.n")
f <- results(dat = reclass.results, x = "hp.sort.n")
g <- results(dat = reclass.results, x = "hp.sort.p")

t5.r <- rbind(
        cbind("Reclassification", "", "", "", "", "", "", "", ""),
        cbind("No PrEP", "", "", "", "", "", "", "", ""),
        cbind("Median", round(a[1,2]), round(100*b[1,2],1), round(c[1,2]), round(100*d[1,2],1), round(c[3,2]), round(100*d[3,2],1), "", ""),
        cbind("95% interval", paste0("(",round(a[1,1]),", ", round(a[1,3]), ")"), paste0("(",round(100*b[1,1],1),", ",round(100*b[1,3],1), ")"), paste0("(",round(c[1,1]),", ", round(c[1,3]), ")"), paste0("(",round(100*d[1,1],1),", ",round(100*d[1,3],1), ")"), paste0("(",round(c[3,1]),", ", round(c[3,3]), ")"), paste0("(",round(100*d[3,1],1),", ",round(100*d[3,3],1), ")"), "", ""),
        cbind("PrEP", "", "", "", "", "", "", "", ""),
        cbind("Median", round(a[2,2]), round(100*b[2,2],1), round(c[2,2]), round(100*d[2,2],1), round(c[4,2]), round(100*d[4,2],1), "", ""),
        cbind("95% interval", paste0("(",round(a[2,1]),", ", round(a[2,3]), ")"), paste0("(",round(100*b[2,1],1),", ",round(100*b[2,3],1), ")"), paste0("(",round(c[2,1]),", ", round(c[2,3]), ")"), paste0("(",round(100*d[2,1],1),", ",round(100*d[2,3],1), ")"), paste0("(",round(c[4,1]),", ", round(c[4,3]), ")"), paste0("(",round(100*d[4,1],1),", ",round(100*d[4,3],1), ")"), "", ""),
        cbind("Total", "", "", "", "", "", "", "", ""),
        cbind("Median", round(e[1,2]), 100, round(f[1,2]), round(100*g[1,2],1), round(f[3,2]), round(100*g[3,2],1), "", ""),
        cbind("95% interval", paste0("(",round(e[1,1]),", ", round(e[1,3]), ")"), "(1, 1)", paste0("(",round(f[1,1]),", ", round(f[1,3]), ")"), paste0("(",round(100*g[1,1],1),", ",round(100*g[1,3],1), ")"), paste0("(",round(f[3,1]),", ", round(f[3,3]), ")"), paste0("(",round(100*g[3,1],1),", ",round(100*g[3,3],1), ")"), "", "")        
)

table5 <- rbind(t5.title, t5.colname.a, t5.colname.b, t5.colname.c, t5.w, t5.c, t5.r)

write.csv(table5, file = "aim1_table5a_PP_pers.csv")
