# Table 4: HIV-PrEP Mixing
# Required: ~/GitHub/PrEP-HIV-Sorting/Analysis.R
# By Kevin Maloney (kevin.maloney@emory.edu)
# 2020-11-19

rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/3 Analysis.R')

t4.title <- vector("character", 9)
t4.title[1] <- "Table 4. HIV-PrEP Sorting"

t4.colname.a <- vector("character", 9) 
t4.colname.a[4] <- "Partners"

t4.colname.b <- c("", "Total, dyad-level", "", "No PrEP", "", "PrEP", "", "PrEP unknown", "")
t4.colname.c <- c("Respondents", "N", "Column %", "N", "Row %", "N", "Row %", "N", "Row %")

# Whole sample - partners are HIV -/?
a <- artnetSort %>% filter(!p_hiv == "Pos") %>% count(hiv3) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(!p_hiv == "Pos", hiv3 == "Neg") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(!p_hiv == "Pos", hiv3 == "Pos") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(!p_hiv == "Pos", hiv3 == "Unk") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
e <- artnetSort %>% filter(!p_hiv == "Pos") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))

t4.w <- rbind(
        cbind("Whole Sample", "", "", "", "", "", "", "", ""),
        cbind("Negative", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], b$n[3], b$perc[3]),
        cbind("Positive", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], c$n[3], c$perc[3]),
        cbind("Unknown",  a$n[3], a$perc[3], d$n[1], d$perc[1], d$n[2], d$perc[2], d$n[3], d$perc[3]),
        cbind("Total", sum(a$n), sum(a$perc), e$n[1], e$perc[1], e$n[2], e$perc[2], e$n[3], e$perc[3])
        )        

# Complete Case - partners are HIV negative and known PrEP use
a <- artnetSort %>% filter(p_hiv == "Neg", !prep.during.part2 == "Unk", !hiv3 == "Unk") %>% count(hiv3) %>% mutate(perc = round(100*n/sum(n), 1))
b <- artnetSort %>% filter(p_hiv == "Neg", !prep.during.part2 == "Unk", hiv3 == "Neg") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
c <- artnetSort %>% filter(p_hiv == "Neg", !prep.during.part2 == "Unk", hiv3 == "Pos") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))
d <- artnetSort %>% filter(p_hiv == "Neg", !prep.during.part2 == "Unk", !hiv3 == "Unk") %>% count(prep.during.part2) %>% mutate(perc = round(100*n/sum(n), 1))

t4.c <- rbind(
        cbind("Complete-case", "", "", "", "", "", "", "", ""),
        cbind("Negative", a$n[1], a$perc[1], b$n[1], b$perc[1], b$n[2], b$perc[2], "", ""),
        cbind("Positive", a$n[2], a$perc[2], c$n[1], c$perc[1], c$n[2], c$perc[2], "", ""),
        cbind("Total", sum(a$n), sum(a$perc), d$n[1], d$perc[1], d$n[2], d$perc[2], "", "")
)

# Reclassification
a <- results(dat = reclass.results, x = "ehiv.pneg.n")
b <- results(dat = reclass.results, x = "ehiv.pneg.p")
c <- as.data.frame(results(dat = reclass.results, x = "hp.sort.n"))
d <- as.data.frame(results(dat = reclass.results, x = "hp.sort.p"))
e <- as.data.frame(results(dat = reclass.results, x = "hiv.imp.n"))
f <- as.data.frame(results(dat = reclass.results, x = "hiv.imp.p"))
g <- as.data.frame(results(dat = reclass.results, x = "prep.imp.n"))
h <- as.data.frame(results(dat = reclass.results, x = "prep.imp.p"))

t4.r <- rbind(
        cbind("Reclassification", "", "", "", "", "", "", "", ""),
        cbind("Negative/Unknown", "", "", "", "", "", "", "", ""),
        cbind("Median", round(a[1,2]), round(100*b[1,2],1), round(c[1,2]), round(100*d[1,2],1), round(c[3,2]), round(100*d[3,2],1), "", ""),
        cbind("95% interval", paste0("(",round(a[1,1]),", ", round(a[1,3]), ")"), paste0("(",round(100*b[1,1],1),", ",round(100*b[1,3],1), ")"), paste0("(",round(c[1,1]),", ", round(c[1,3]), ")"), paste0("(",round(100*d[1,1],1),", ",round(100*d[1,3],1), ")"), paste0("(",round(c[3,1]),", ", round(c[3,3]), ")"), paste0("(",round(100*d[3,1],1),", ",round(100*d[3,3],1), ")"), "", ""),
        cbind("Positive", "", "", "", "", "", "", "", ""),
        cbind("Median", round(a[2,2]), round(100*b[2,2],1), round(c[2,2]), round(100*d[2,2],1), round(c[4,2]), round(100*d[4,2],1), "", ""),
        cbind("95% interval", paste0("(",round(a[2,1]),", ", round(a[2,3]), ")"), paste0("(",round(100*b[2,1],1),", ",round(100*b[2,3],1), ")"), paste0("(",round(c[2,1]),", ", round(c[2,3]), ")"), paste0("(",round(100*d[2,1],1),", ",round(100*d[2,3],1), ")"), paste0("(",round(c[4,1]),", ", round(c[4,3]), ")"), paste0("(",round(100*d[4,1],1),", ",round(100*d[4,3],1), ")"), "", ""),
        cbind("Total", "", "", "", "", "", "", "", ""),
        cbind("Median", round(e[1,2]), 100, round(g[1,2]), round(100*h[1,2],1), round(g[2,2]), round(100*h[2,2],1), "", ""),
        cbind("95% interval", paste0("(",round(e[1,1]),", ", round(e[1,3]), ")"), "(1, 1)", paste0("(",round(g[1,1]),", ", round(g[1,3]), ")"), paste0("(",round(100*h[1,1],1),", ",round(100*h[1,3],1), ")"), paste0("(",round(g[2,1]),", ", round(g[2,3]), ")"), paste0("(",round(100*h[2,1],1),", ",round(100*h[2,3],1), ")"), "", "")        
)

table4 <- rbind(t4.title, t4.colname.a, t4.colname.b, t4.colname.c, t4.w, t4.c, t4.r)

write.csv(table4, file = "aim1_table4_HP.csv")