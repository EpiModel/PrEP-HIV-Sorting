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


