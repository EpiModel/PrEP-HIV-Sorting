rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

library("INLA")

#### Wide data frame -----
artnetWideSort1 <- artnet %>% 
        select(AMIS_ID, city2, 
               hiv3, race.cat, age.cat)

# Changing to factor and 2s to NA
artnetWideSort1$hiv3[artnetWideSort1$hiv3 == 2] <- NA
artnetWideSort1$age.cat = factor(artnetWideSort1$age.cat)
artnetWideSort1$race.cat = factor(artnetWideSort1$race.cat)

#### Ego's HIV Imputation ----

# Imputation model
hiv3.inla <- inla(hiv3 ~ race.cat + age.cat + race.cat:age.cat + city2, 
                        family = "binomial",
                        data = artnetWideSort1,
                        control.predictor = list(compute = TRUE),
                        control.compute = list(config = TRUE))

# Vector of obs with missing hiv3
hiv3.na <- which(is.na(artnetWideSort1$hiv3))

# Prints the posterior dist for each obs
# hiv3.inla$summary.fitted.values[hiv3.na, c("mean", "sd")]

n.imp <- 1

# Sampling from the posterior dist
hiv3.pred <- inla.posterior.sample(n.imp, hiv3.inla)
# write.csv(hiv3.pred[[1]]$latent[hiv3.na,], file = "hiv3.pred.csv")

# head(hiv3.pred[[1]]$latent[hiv3.na,])

# Truncating values > 0 to 0
hiv3.pred[[1]]$latent[hiv3.pred[[1]]$latent > 0] <- 0

# Imputing with random draw of 0 or 1
# p = linear predictor transformed to binomial probability
artnetWideSort2 <- artnetWideSort1
artnetWideSort2$hiv3[hiv3.na] <- rbinom(length(hiv3.na),1,exp(hiv3.pred[[1]]$latent[hiv3.na,]))


#### Long data frame ----
artnetSort1 <- artnetLong %>% 
        filter(!is.na(p_race.cat)) %>%
        select(AMIS_ID, city2, ptype, 
                hiv3, prep.during.ego2, race.cat, age.cat, 
                p_hiv, prep.during.part2, p_race.cat, p_age.cat_imp)

# Changing variables to numeric which is required by INLA for outcome
artnetSort1$hiv3 <- as.numeric(artnetSort1$hiv3)
artnetSort1$hiv3 = artnetSort1$hiv3 - 1
artnetSort1$hiv3[artnetSort1$hiv3 == 2] <- NA # This is unnecessary? Not sure why this is here. Double check if using this approach

artnetSort1$p_hiv <- as.numeric(artnetSort1$p_hiv)
artnetSort1$p_hiv = artnetSort1$p_hiv - 1
artnetSort1$p_hiv[artnetSort1$p_hiv == 2] <- NA

artnetSort1$prep.during.ego2 <- as.numeric(artnetSort1$prep.during.ego2)
artnetSort1$prep.during.ego2 = artnetSort1$prep.during.ego2 - 1
artnetSort1$prep.during.ego2[artnetSort1$prep.during.ego2 == 2] <- NA

artnetSort1$prep.during.part2 <- as.numeric(artnetSort1$prep.during.part2)
artnetSort1$prep.during.part2 = artnetSort1$prep.during.part2 - 1
artnetSort1$prep.during.part2[artnetSort1$prep.during.part2 == 2] <- NA

# Changing ptype to factor
artnetSort1$ptype = factor(artnetSort1$ptype,  labels = c("Main", "Casual", "Once"))

# Adding the imputed hiv3 values
hiv3.egos <- artnetWideSort2 %>% select(AMIS_ID, hiv3)
artnetSort2 <- left_join(artnetSort1, hiv3.egos, by = "AMIS_ID") %>%
        mutate(hiv3 = coalesce(hiv3.x, hiv3.y))
artnetSort2$hiv3.x <- NULL
artnetSort2$hiv3.y <- NULL

# Recoding ego's PrEP
artnetSort2$prep.during.ego2[artnetSort2$hiv3 == 1] <- 0
artnetSort2$prep.during.ego2[which(is.na(artnetSort2$prep.during.ego2))] <- 0

#### Partner's HIV Imputation ----

# Imputation model
p_hiv.inla <- inla(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                           ptype + hiv3 + prep.during.ego2 + 
                           ptype:hiv3 + ptype:prep.during.ego2 +
                           city2 + f(AMIS_ID, model = "iid"),
                           data = artnetSort2, family = "binomial",
                           control.predictor = list(link = 1, compute = TRUE),
                           control.compute = list(config = TRUE))

# Vector of obs with missing p_hiv
p_hiv.na <- which(is.na(artnetSort2$p_hiv))

# Prints the posterior dist for each obs
# p_hiv.inla$summary.fitted.values[p_hiv.na, c("mean", "sd")]

n.imp <- 1
p_hiv.pred <- inla.posterior.sample(n.imp, p_hiv.inla)

# write.csv(p_hiv.pred[[1]]$latent[p_hiv.na,], file = "p_hiv.pred.csv")

# head(p_hiv.pred[[1]]$latent[p_hiv.na,])

# Truncating values > 0 to 0
p_hiv.pred[[1]]$latent[p_hiv.pred[[1]]$latent > 0] <- 0

# Imputing with random draw of 0 or 1  
# p = linear predictor transformed to binomial probability
artnetSort3 <- artnetSort2
artnetSort3$p_hiv[p_hiv.na] <- rbinom(length(p_hiv.na),1,exp(p_hiv.pred[[1]]$latent[p_hiv.na,]))

#### Partner's PrEP Imputation ----

# No PrEP for HIV+
artnetSort3$prep.during.part2[artnetSort3$p_hiv == 1] <- 0

# Imputation model
prep.part.inla <- inla(prep.during.part2 ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                           ptype + hiv3 + prep.during.ego2 +
                           ptype:hiv3 + ptype:prep.during.ego2 +
                           city2 + f(AMIS_ID, model = "iid"),
                   data = subset(artnetSort3, p_hiv == 0), family = "binomial",
                   control.predictor = list(link = 1, compute = TRUE),
                   control.compute = list(config = TRUE))

# Vector of obs with missing p_hiv
prep.part.na <- which(is.na(artnetSort3$prep.during.part2))

# Prints the posterior dist for each obs
# prep.part.inla$summary.fitted.values[prep.part.na, c("mean", "sd")]

n.imp <- 1
prep.part.pred <- inla.posterior.sample(n.imp, prep.part.inla)

# write.csv(prep.part.pred[[1]]$latent[prep.part.na,], file = "prep.part.pred.csv")

# head(prep.part.pred[[1]]$latent[prep.part.na,])

# Truncating values > 0 to 0
prep.part.pred[[1]]$latent[prep.part.pred[[1]]$latent > 0] <- 0

# Imputing with random draw of 0 or 1  
# p = linear predictor transformed to binomial probability
artnetSort4 <- artnetSort3
artnetSort4$prep.during.part2[prep.part.na] <- rbinom(length(prep.part.na),1,exp(prep.part.pred[[1]]$latent[prep.part.na,]))

