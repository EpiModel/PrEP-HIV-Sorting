rm(list = ls())
source('~/GitHub/PrEP-HIV-Sorting/1 Data cleaning.R')


# Compiling all reclassification dfs to a list
dfs <- vector("list", 300)
for (i in seq_along(1:300)) {
        dfs[[i]] <- readRDS(paste("imp", i, ".rds", sep = ""))
}

### Reclassification analysis ----
reclass <- vector("list", 300)

for (i in seq_along(1:300)) {
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

# # HIV prevalence
# results(dat = reclass.results, x = "hiv.imp.n")
# results(dat = reclass.results, x = "hiv.imp.p")
# 
# # HIV reclassification 
# results(dat = reclass.results, x = "hiv.reclass.n")
# results(dat = reclass.results, x = "hiv.reclass.p")
# 
# # HIV-HIV sorting
# results(dat = reclass.results, x = "hh.sort.n")
# results(dat = reclass.results, x = "hh.sort.p")
# 
# # PrEP prevalence among HIV -/?
# results(dat = reclass.results, x = "prep.imp.n")
# results(dat = reclass.results, x = "prep.imp.p")
# 
# # PrEP reclassification
# results(dat = reclass.results, x = "prep.reclass.n")
# results(dat = reclass.results, x = "prep.reclass.p")
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

c <- ggplot(reclass.results, aes(hiv.imp.p.1))
d <- c + geom_histogram(binwidth = 0.0025)
c + geom_area(stat="bin")

e <- ggplot(reclass.results, aes(prep.imp.p.1))
e + geom_histogram(binwidth = 0.0025)
e + geom_area(stat="bin")

## Marginal Distributions ---- 
dfs <- vector("list", 300)
for (i in seq_along(1:300)) {
        dfs[[i]] <- readRDS(paste("imp", i, ".rds", sep = ""))
}

glm.hiv <- vector("list", 300)
marg.hiv <- vector("list", 300)


for (i in seq_along(1:300)) {
        
        glm.hiv[[i]] <- glm(p_hiv.imp ~ p_age.cat_imp + p_race.cat + city2,
                            data = dfs[[i]],
                            family = binomial(link = "logit"))
        
        dfs[[i]]$glm.hiv <- glm.hiv[[i]]$fitted.values
        
        marg.hiv[[i]]$race1 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_race.cat == "black"])
        marg.hiv[[i]]$race2 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_race.cat == "hispanic"])
        marg.hiv[[i]]$race3 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_race.cat == "other"])
        marg.hiv[[i]]$race4 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_race.cat == "white"])
        
        marg.hiv[[i]]$age1 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_age.cat_imp == "15-24"])
        marg.hiv[[i]]$age2 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_age.cat_imp == "25-34"])
        marg.hiv[[i]]$age3 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_age.cat_imp == "35-44"])
        marg.hiv[[i]]$age4 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_age.cat_imp == "45-54"])
        marg.hiv[[i]]$age5 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_age.cat_imp == "55-65"])
        marg.hiv[[i]]$age6 <- mean(dfs[[i]]$glm.hiv[dfs[[i]]$p_age.cat_imp == "66+"])
        
}

marg.hiv <- as.data.frame(
        do.call(rbind, lapply(
                marg.hiv, c, recursive=TRUE)))

summary(marg.hiv)

## Standardized HIV & PrEP Prevalence ---- 

## Standard Population

# Single obs for each ego in sample pop
IDs <- artnetSort %>% select(AMIS_ID) %>% unique()
egos.stand <- left_join(IDs, artnet, by = "AMIS_ID")

# prep_curr indicates PrEP use at time of survey
egos.stand$prep_curr[egos.stand$hiv3 == 2] <- 0
egos.stand$prep_curr[egos.stand$PREP_REVISED == 0] <- 0
egos.stand$prep_curr[egos.stand$artnetPREP_CURRENT == 0] <- 0
egos.stand$prep_curr[egos.stand$artnetPREP_CURRENT == 1] <- 1

egos.stand$hiv.prep[egos.stand$hiv3 == 1] <- 1
egos.stand$hiv.prep[egos.stand$prep_curr == 0] <- 0
egos.stand$hiv.prep[egos.stand$prep_curr == 1] <- 2

egos.stand <- egos.stand %>% filter(!is.na(hiv.prep)) %>% #Removing N = 3 with Ever PrEP but skipped Current PrEP
        select(AMIS_ID, hiv.prep, p_age.cat_imp = age.cat, p_race.cat = race.cat, city2) # Renaming variables to match partners

# Subset for HIV -/?
egos.stand2 <- egos.stand %>% filter(hiv.prep %in% c(0,2))

## Standardization

# dfs <- vector("list", 300)
# for (i in seq_along(1:300)) {
#         dfs[[i]] <- readRDS(paste("imp", i, ".rds", sep = ""))
# }

glm.hiv <- vector("list", 300)
stand.hiv <- vector("double", 300)

glm.prep <- vector("list", 300)
stand.prep <- vector("double", 300)


for (i in seq_along(1:300)) {
        
        # HIV
        glm.hiv[[i]] <- glm(p_hiv.imp ~ p_age.cat_imp*p_race.cat + city2,
                            data = dfs[[i]],
                            family = binomial(link = "logit"))
        
        stand.hiv[i] <- mean(predict(glm.hiv[[i]], newdata = egos.stand, type = "response"))

        # PrEP
        glm.prep[[i]] <- glm(prep.imp ~ p_age.cat_imp*p_race.cat + city2,
                             data = dfs[[i]],
                             family = binomial(link = "logit"))
        
        stand.prep[i] <- mean(predict(glm.prep[[i]], newdata = egos.stand2, type = "response"))

}

summary(stand.hiv)
prop.table(table(egos.stand$hiv.prep))

summary(stand.prep)
prop.table(table(egos.stand2$hiv.prep))
