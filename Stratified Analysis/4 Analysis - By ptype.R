rm(list = ls())
source('1 Data cleaning.R')


# Compiling all reclassification dfs to a list
dfs <- vector("list", 300)
for (i in seq_along(1:300)) {
        dfs[[i]] <- readRDS(paste("imp", i, ".rds", sep = ""))
}

### Reclassification analysis ----
reclass <- vector("list", 300)

for (i in seq_along(1:300)) {
        
        dfs[[i]] <- dfs[[i]] %>% 
                filter(ptype %in% c("Main", "Casual")) %>% 
                filter(!city2 %in% c("zOther1", "zOther2", "zOther3", "zOther4", "zOther5", "zOther6", "zOther7", "zOther8", "zOther9"))

        dfs[[i]] <- dfs[[i]] %>% 
                mutate(hiv.prep = ifelse(hiv2 == 1, 1, ifelse(prep.during.ego2 == "No",0,2)),
                        hiv.prep_p = ifelse(p_hiv.imp == 1, 1, ifelse(prep.imp == 0,0,2)))
                
        # Expected Table Counts for Aim 2
        reclass[[i]]$hiv.prep.mix <- table(dfs[[i]]$hiv.prep, dfs[[i]]$hiv.prep_p)
        
        # HIV prevalence
        reclass[[i]]$hiv.imp.n <- table(dfs[[i]]$p_hiv.imp)
        reclass[[i]]$hiv.imp.p <- prop.table(table(dfs[[i]]$p_hiv.imp))

        # HIV reclassification
        reclass[[i]]$hiv.reclass.n <- table(dfs[[i]]$p_hiv, dfs[[i]]$p_hiv.imp)
        reclass[[i]]$hiv.reclass.p <- prop.table(table(dfs[[i]]$p_hiv, dfs[[i]]$p_hiv.imp), 1)

        # HIV-HIV sorting
        reclass[[i]]$hh.sort.n <- table(dfs[[i]]$hiv2, dfs[[i]]$p_hiv.imp)
        reclass[[i]]$hh.sort.p <- prop.table(table(dfs[[i]]$hiv2, dfs[[i]]$p_hiv.imp), 1)

        # PrEP prevalence, given neg/unk
        reclass[[i]]$prep.imp.n <- table(dfs[[i]]$prep.imp)
        reclass[[i]]$prep.imp.p <- prop.table(table(dfs[[i]]$prep.imp))
        
        # PrEP reclassification
        reclass[[i]]$prep.reclass.n <- table(dfs[[i]]$prep.during.part2, dfs[[i]]$prep.imp)
        reclass[[i]]$prep.reclass.p <- prop.table(table(dfs[[i]]$prep.during.part2, dfs[[i]]$prep.imp), 1)
        
        # HIV prevalence among egos, given partner is HIV -/?
        reclass[[i]]$ehiv.pneg.n <- table(dfs[[i]]$hiv2[dfs[[i]]$p_hiv.imp == 0])
        reclass[[i]]$ehiv.pneg.p <- prop.table(table(dfs[[i]]$hiv2[dfs[[i]]$p_hiv.imp == 0]))
        
        # HIV-PrEP sorting
        reclass[[i]]$hp.sort.n <- table(dfs[[i]]$hiv2, dfs[[i]]$prep.imp)
        reclass[[i]]$hp.sort.p <- prop.table(table(dfs[[i]]$hiv2, dfs[[i]]$prep.imp),1)
        
        # PrEP prevalence among egos, given partner is HIV -/?
        reclass[[i]]$eprep.pneg.n <- table(dfs[[i]]$prep.during.ego2[dfs[[i]]$p_hiv.imp == 0 & dfs[[i]]$hiv2 == 0])
        reclass[[i]]$eprep.pneg.p <- prop.table(table(dfs[[i]]$prep.during.ego2[dfs[[i]]$p_hiv.imp == 0 & dfs[[i]]$hiv2 == 0]))
        
        # PrEP-PrEP sorting
        reclass[[i]]$pp.sort.n <- table(dfs[[i]]$prep.during.ego2[dfs[[i]]$hiv2 == 0], dfs[[i]]$prep.imp[dfs[[i]]$hiv2 == 0])
        reclass[[i]]$pp.sort.p <- prop.table(table(dfs[[i]]$prep.during.ego2[dfs[[i]]$hiv2 == 0], dfs[[i]]$prep.imp[dfs[[i]]$hiv2 == 0]), 1)
        
        # PrEP-HIV sorting
        reclass[[i]]$ph.sort.n <- table(dfs[[i]]$prep.during.ego2[dfs[[i]]$hiv2 == 0], dfs[[i]]$p_hiv.imp[dfs[[i]]$hiv2 == 0])
        reclass[[i]]$ph.sort.p <- prop.table(table(dfs[[i]]$prep.during.ego2[dfs[[i]]$hiv2 == 0], dfs[[i]]$p_hiv.imp[dfs[[i]]$hiv2 == 0]), 1)
        
}

listVec <- lapply(reclass, c, recursive=TRUE)
reclass.results <- as.data.frame(do.call(rbind, listVec))

results <- function(dat, x) {
        q <- select(dat, starts_with(x))
        return(t(apply(q, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = FALSE)))
}

# # HIV-PrEP Mixing - expected counts for Aim 2
results(dat = reclass.results, x = "hiv.prep.mix")

# # HIV prevalence
# results(dat = reclass.results, x = "hiv.imp.n")
 results(dat = reclass.results, x = "hiv.imp.p")
# 
# # HIV reclassification 
# results(dat = reclass.results, x = "hiv.reclass.n")
 results(dat = reclass.results, x = "hiv.reclass.p")
# 
# # HIV-HIV sorting
# results(dat = reclass.results, x = "hh.sort.n")
# results(dat = reclass.results, x = "hh.sort.p")
# 
# # PrEP prevalence among HIV -/?
# results(dat = reclass.results, x = "prep.imp.n")
 results(dat = reclass.results, x = "prep.imp.p")
# 
# # PrEP reclassification
# results(dat = reclass.results, x = "prep.reclass.n")
 results(dat = reclass.results, x = "prep.reclass.p")
# 
# # HIV prevalence among egos, given partner is HIV -/?
# results(dat = reclass.results, x = "ehiv.pneg.n")
# results(dat = reclass.results, x = "ehiv.pneg.p")
# 
# # HIV-PrEP sorting
# results(dat = reclass.results, x = "hp.sort.n")
# results(dat = reclass.results, x = "hp.sort.p")
# 
# # PrEP-PrEP sorting
# results(dat = reclass.results, x = "pp.sort.n")
# results(dat = reclass.results, x = "pp.sort.p")

# # PrEP-HIV sorting
# results(dat = reclass.results, x = "ph.sort.n")
# results(dat = reclass.results, x = "ph.sort.p")

pres <- artnetSort %>%
        mutate(hiv.prep = ifelse(hiv3 == "Pos", "Pos", 
                                 ifelse(hiv3 == "Unk", "Unk",
                                        ifelse(prep.during.ego2 == "No", "No PrEP", "PrEP")))) %>%
        mutate(hiv.prep.p = ifelse(p_hiv == "Pos", "Pos", 
                                 ifelse(p_hiv == "Unk", "Unk",
                                        ifelse(prep.during.part2 == "No", "No PrEP", "PrEP"))))
        

prop.table(table(pres$hiv.prep, pres$hiv.prep.p, useNA = "ifany"),1)
