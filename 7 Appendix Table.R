# Appendix Table: Results of the reclassification analysis
# By Kevin Maloney (kevin.maloney@emory.edu)
# 2021-03-06

source('~/GitHub/PrEP-HIV-Sorting/1 Data cleaning.R')
rm(artnet, artnetLong)

## Ego-reported values (HIV) ---
prop.table(table(artnetSort$p_hiv, useNA = "ifany"))
prop.table(table(artnetSort$p_age.cat_imp, artnetSort$p_hiv, useNA = "ifany"), 1)
prop.table(table(artnetSort$p_race.cat, artnetSort$p_hiv, useNA = "ifany"), 1)
prop.table(table(artnetSort$ptype, artnetSort$p_hiv, useNA = "ifany"), 1)

## Ego-reported values (PrEP) ---
NegUnk <- artnetSort %>% filter(p_hiv %in% c("Neg", "Unk"))

prop.table(table(NegUnk$prep.during.part2, useNA = "ifany"))
prop.table(table(NegUnk$p_age.cat_imp, NegUnk$prep.during.part2, useNA = "ifany"), 1)
prop.table(table(NegUnk$p_race.cat, NegUnk$prep.during.part2, useNA = "ifany"), 1)
prop.table(table(NegUnk$ptype, NegUnk$prep.during.part2, useNA = "ifany"), 1)


## Processing reclassification data ---

# Compiling all reclassification dfs to a list
dfs <- vector("list", 300)
for (i in seq_along(1:300)) {
        dfs[[i]] <- readRDS(paste("imp", i, ".rds", sep = ""))
}

# Summary data for each dataset
reclass <- vector("list", 300)
dfs2 <- vector("list", 300)

for (i in seq_along(1:300)) {

        ### HIV reclassification ----
        # P(y*=1) for HIV
        reclass[[i]]$star1.hiv.total <- mean(dfs[[i]]$star1.hiv)
        reclass[[i]]$star1.hiv.age <- by(dfs[[i]]$star1.hiv, dfs[[i]]$p_age.cat_imp, mean)
        reclass[[i]]$star1.hiv.race <- by(dfs[[i]]$star1.hiv, dfs[[i]]$p_race.cat, mean)
        reclass[[i]]$star1.hiv.er <- by(dfs[[i]]$star1.hiv, dfs[[i]]$p_hiv, mean)
        reclass[[i]]$star1.hiv.ptype <- by(dfs[[i]]$star1.hiv, dfs[[i]]$ptype, mean)
        reclass[[i]]$star1.hiv.main <- by(dfs[[i]]$star1.hiv[dfs[[i]]$ptype == "Main"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Main"], mean)
        reclass[[i]]$star1.hiv.casual <- by(dfs[[i]]$star1.hiv[dfs[[i]]$ptype == "Casual"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Casual"], mean)
        reclass[[i]]$star1.hiv.once <- by(dfs[[i]]$star1.hiv[dfs[[i]]$ptype == "Once"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Once"], mean)

        # Sens(x,r) for HIV
        reclass[[i]]$sens.hiv.total <- mean(dfs[[i]]$sens.xr.hiv)
        reclass[[i]]$sens.hiv.age <- by(dfs[[i]]$sens.xr.hiv, dfs[[i]]$p_age.cat_imp, mean)
        reclass[[i]]$sens.hiv.race <- by(dfs[[i]]$sens.xr.hiv, dfs[[i]]$p_race.cat, mean)
        reclass[[i]]$sens.hiv.er <- by(dfs[[i]]$sens.xr.hiv, dfs[[i]]$p_hiv, mean)
        reclass[[i]]$sens.hiv.ptype <- by(dfs[[i]]$sens.xr.hiv, dfs[[i]]$ptype, mean)
        reclass[[i]]$sens.hiv.main <- by(dfs[[i]]$sens.xr.hiv[dfs[[i]]$ptype == "Main"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Main"], mean)
        reclass[[i]]$sens.hiv.casual <- by(dfs[[i]]$sens.xr.hiv[dfs[[i]]$ptype == "Casual"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Casual"], mean)
        reclass[[i]]$sens.hiv.once <- by(dfs[[i]]$sens.xr.hiv[dfs[[i]]$ptype == "Once"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Once"], mean)
        
        # Spec(x,r) for HIV
        reclass[[i]]$spec.hiv.total <- mean(dfs[[i]]$spec.xr.hiv)
        reclass[[i]]$spec.hiv.age <- by(dfs[[i]]$spec.xr.hiv, dfs[[i]]$p_age.cat_imp, mean)
        reclass[[i]]$spec.hiv.race <- by(dfs[[i]]$spec.xr.hiv, dfs[[i]]$p_race.cat, mean)
        reclass[[i]]$spec.hiv.er <- by(dfs[[i]]$spec.xr.hiv, dfs[[i]]$p_hiv, mean)
        reclass[[i]]$spec.hiv.ptype <- by(dfs[[i]]$spec.xr.hiv, dfs[[i]]$ptype, mean)
        reclass[[i]]$spec.hiv.main <- by(dfs[[i]]$spec.xr.hiv[dfs[[i]]$ptype == "Main"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Main"], mean)
        reclass[[i]]$spec.hiv.casual <- by(dfs[[i]]$spec.xr.hiv[dfs[[i]]$ptype == "Casual"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Casual"], mean)
        reclass[[i]]$spec.hiv.once <- by(dfs[[i]]$spec.xr.hiv[dfs[[i]]$ptype == "Once"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Once"], mean)
        
        # P(y=1) for HIV
        reclass[[i]]$pred.hiv.total <- mean(dfs[[i]]$pred.hiv)
        reclass[[i]]$pred.hiv.age <- by(dfs[[i]]$pred.hiv, dfs[[i]]$p_age.cat_imp, mean)
        reclass[[i]]$pred.hiv.race <- by(dfs[[i]]$pred.hiv, dfs[[i]]$p_race.cat, mean)
        reclass[[i]]$pred.hiv.er <- by(dfs[[i]]$pred.hiv, dfs[[i]]$p_hiv, mean)
        reclass[[i]]$pred.hiv.ptype <- by(dfs[[i]]$pred.hiv, dfs[[i]]$ptype, mean)
        reclass[[i]]$pred.hiv.main <- by(dfs[[i]]$pred.hiv[dfs[[i]]$ptype == "Main"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Main"], mean)
        reclass[[i]]$pred.hiv.casual <- by(dfs[[i]]$pred.hiv[dfs[[i]]$ptype == "Casual"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Casual"], mean)
        reclass[[i]]$pred.hiv.once <- by(dfs[[i]]$pred.hiv[dfs[[i]]$ptype == "Once"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Once"], mean)
        
        # Prevalence of imputed HIV 
        reclass[[i]]$imp.hiv.total <- mean(dfs[[i]]$p_hiv.imp)
        reclass[[i]]$imp.hiv.age <- by(dfs[[i]]$p_hiv.imp, dfs[[i]]$p_age.cat_imp, mean)
        reclass[[i]]$imp.hiv.race <- by(dfs[[i]]$p_hiv.imp, dfs[[i]]$p_race.cat, mean)
        reclass[[i]]$imp.hiv.er <- by(dfs[[i]]$p_hiv.imp, dfs[[i]]$p_hiv, mean)
        reclass[[i]]$imp.hiv.ptype <- by(dfs[[i]]$p_hiv.imp, dfs[[i]]$ptype, mean)
        reclass[[i]]$imp.hiv.main <- by(dfs[[i]]$p_hiv.imp[dfs[[i]]$ptype == "Main"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Main"], mean)
        reclass[[i]]$imp.hiv.casual <- by(dfs[[i]]$p_hiv.imp[dfs[[i]]$ptype == "Casual"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Casual"], mean)
        reclass[[i]]$imp.hiv.once <- by(dfs[[i]]$p_hiv.imp[dfs[[i]]$ptype == "Once"], dfs[[i]]$p_hiv[dfs[[i]]$ptype == "Once"], mean)
        
        ### PrEP reclassification ----
        # Subset of alters imputed without diagnosed HIV
        
        dfs2[[i]] <- dfs[[i]] %>% filter(p_hiv.imp == 0)
        
        # P(y*=1) for PrEP
        reclass[[i]]$star1.prep.total <- mean(dfs2[[i]]$star1.prep)
        reclass[[i]]$star1.prep.age <- by(dfs2[[i]]$star1.prep, dfs2[[i]]$p_age.cat_imp, mean)
        reclass[[i]]$star1.prep.race <- by(dfs2[[i]]$star1.prep, dfs2[[i]]$p_race.cat, mean)
        reclass[[i]]$star1.prep.er <- by(dfs2[[i]]$star1.prep, dfs2[[i]]$prep.during.part2, mean)
        reclass[[i]]$star1.prep.ptype <- by(dfs2[[i]]$star1.prep, dfs2[[i]]$ptype, mean)
        reclass[[i]]$star1.prep.main <- by(dfs2[[i]]$star1.prep[dfs2[[i]]$ptype == "Main"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Main"], mean)
        reclass[[i]]$star1.prep.casual <- by(dfs2[[i]]$star1.prep[dfs2[[i]]$ptype == "Casual"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Casual"], mean)
        reclass[[i]]$star1.prep.once <- by(dfs2[[i]]$star1.prep[dfs2[[i]]$ptype == "Once"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Once"], mean)
        
        # Sens(x,r) for PrEP
        reclass[[i]]$sens.prep.total <- mean(dfs2[[i]]$sens.xr.prep)
        reclass[[i]]$sens.prep.age <- by(dfs2[[i]]$sens.xr.prep, dfs2[[i]]$p_age.cat_imp, mean)
        reclass[[i]]$sens.prep.race <- by(dfs2[[i]]$sens.xr.prep, dfs2[[i]]$p_race.cat, mean)
        reclass[[i]]$sens.prep.er <- by(dfs2[[i]]$sens.xr.prep, dfs2[[i]]$prep.during.part2, mean)
        reclass[[i]]$sens.prep.ptype <- by(dfs2[[i]]$sens.xr.prep, dfs2[[i]]$ptype, mean)
        reclass[[i]]$sens.prep.main <- by(dfs2[[i]]$sens.xr.prep[dfs2[[i]]$ptype == "Main"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Main"], mean)
        reclass[[i]]$sens.prep.casual <- by(dfs2[[i]]$sens.xr.prep[dfs2[[i]]$ptype == "Casual"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Casual"], mean)
        reclass[[i]]$sens.prep.once <- by(dfs2[[i]]$sens.xr.prep[dfs2[[i]]$ptype == "Once"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Once"], mean)
        
        # Spec(x,r) for PrEP
        reclass[[i]]$spec.prep.total <- mean(dfs2[[i]]$spec.xr.prep)
        reclass[[i]]$spec.prep.age <- by(dfs2[[i]]$spec.xr.prep, dfs2[[i]]$p_age.cat_imp, mean)
        reclass[[i]]$spec.prep.race <- by(dfs2[[i]]$spec.xr.prep, dfs2[[i]]$p_race.cat, mean)
        reclass[[i]]$spec.prep.er <- by(dfs2[[i]]$spec.xr.prep, dfs2[[i]]$prep.during.part2, mean)
        reclass[[i]]$spec.prep.ptype <- by(dfs2[[i]]$spec.xr.prep, dfs2[[i]]$ptype, mean)
        reclass[[i]]$spec.prep.main <- by(dfs2[[i]]$spec.xr.prep[dfs2[[i]]$ptype == "Main"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Main"], mean)
        reclass[[i]]$spec.prep.casual <- by(dfs2[[i]]$spec.xr.prep[dfs2[[i]]$ptype == "Casual"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Casual"], mean)
        reclass[[i]]$spec.prep.once <- by(dfs2[[i]]$spec.xr.prep[dfs2[[i]]$ptype == "Once"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Once"], mean)
        
        # P(y=1) for PrEP
        reclass[[i]]$pred.prep.total <- mean(dfs2[[i]]$pred.prep, na.rm = TRUE)
        reclass[[i]]$pred.prep.age <- by(dfs2[[i]]$pred.prep, dfs2[[i]]$p_age.cat_imp, mean, na.rm = TRUE)
        reclass[[i]]$pred.prep.race <- by(dfs2[[i]]$pred.prep, dfs2[[i]]$p_race.cat, mean, na.rm = TRUE)
        reclass[[i]]$pred.prep.er <- by(dfs2[[i]]$pred.prep, dfs2[[i]]$prep.during.part2, mean, na.rm = TRUE)
        reclass[[i]]$pred.prep.ptype <- by(dfs2[[i]]$pred.prep, dfs2[[i]]$ptype, mean, na.rm = TRUE)
        reclass[[i]]$pred.prep.main <- by(dfs2[[i]]$pred.prep[dfs2[[i]]$ptype == "Main"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Main"], mean, na.rm = TRUE)
        reclass[[i]]$pred.prep.casual <- by(dfs2[[i]]$pred.prep[dfs2[[i]]$ptype == "Casual"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Casual"], mean, na.rm = TRUE)
        reclass[[i]]$pred.prep.once <- by(dfs2[[i]]$pred.prep[dfs2[[i]]$ptype == "Once"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Once"], mean, na.rm = TRUE)
        
        # Prevalence of imputed PrEP 
        reclass[[i]]$imp.prep.total <- mean(dfs2[[i]]$prep.imp, na.rm = TRUE)
        reclass[[i]]$imp.prep.age <- by(dfs2[[i]]$prep.imp, dfs2[[i]]$p_age.cat_imp, mean, na.rm = TRUE)
        reclass[[i]]$imp.prep.race <- by(dfs2[[i]]$prep.imp, dfs2[[i]]$p_race.cat, mean, na.rm = TRUE)
        reclass[[i]]$imp.prep.er <- by(dfs2[[i]]$prep.imp, dfs2[[i]]$prep.during.part2, mean, na.rm = TRUE)
        reclass[[i]]$imp.prep.ptype <- by(dfs2[[i]]$prep.imp, dfs2[[i]]$ptype, mean, na.rm = TRUE)
        reclass[[i]]$imp.prep.main <- by(dfs2[[i]]$prep.imp[dfs2[[i]]$ptype == "Main"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Main"], mean, na.rm = TRUE)
        reclass[[i]]$imp.prep.casual <- by(dfs2[[i]]$prep.imp[dfs2[[i]]$ptype == "Casual"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Casual"], mean, na.rm = TRUE)
        reclass[[i]]$imp.prep.once <- by(dfs2[[i]]$prep.imp[dfs2[[i]]$ptype == "Once"], dfs2[[i]]$prep.during.part2[dfs2[[i]]$ptype == "Once"], mean, na.rm = TRUE)
        
        
}

listVec <- lapply(reclass, c, recursive=TRUE)
reclass.results <- as.data.frame(do.call(rbind, listVec))

results <- function(dat, x) {
        q <- select(dat, starts_with(x))
        return(t(apply(q, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = FALSE)))
        #return(t(apply(q, 2, quantile, probs = c(0, 0.5, 1), na.rm = FALSE)))
        #return(t(apply(q, 2, median, na.rm = FALSE)))
        
}

results(dat = reclass.results, x = "star1.hiv")
results(dat = reclass.results, x = "sens.hiv")
results(dat = reclass.results, x = "spec.hiv")
results(dat = reclass.results, x = "pred.hiv")
results(dat = reclass.results, x = "imp.hiv")

results(dat = reclass.results, x = "star1.prep")
results(dat = reclass.results, x = "sens.prep")
results(dat = reclass.results, x = "spec.prep")
results(dat = reclass.results, x = "pred.prep")
results(dat = reclass.results, x = "imp.prep")


