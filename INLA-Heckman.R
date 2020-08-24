rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

library("INLA")

#### Df initializing ----
artnetSort1 <- artnetLong %>% 
        filter(!is.na(p_race.cat)) %>%
        select(AMIS_ID, city2, ptype, 
               hiv3, prep.during.ego2, race.cat, age.cat, 
               p_hiv, prep.during.part2, p_race.cat, p_age.cat_imp)

### Ego level variables

# hiv3: combining neg and unk (both 0)
artnetSort1$hiv3 <- as.numeric(artnetSort1$hiv3)
artnetSort1$hiv3 = artnetSort1$hiv3 - 1
artnetSort1$hiv3[artnetWideSort1$hiv3 == 2] <- 0

# prep: assuming no PrEP for those missing
artnetSort1$prep.during.ego2 <- as.numeric(artnetSort1$prep.during.ego2)
artnetSort1$prep.during.ego2 = artnetSort1$prep.during.ego2 - 1
artnetSort1$prep.during.ego2[artnetSort1$prep.during.ego2 == 2] <- 0
artnetSort1$prep.during.ego2[which(is.na(artnetSort1$prep.during.ego2))] <- 0

# Covariates set to factor
artnetSort1$age.cat = factor(artnetSort1$age.cat)
artnetSort1$race.cat = factor(artnetSort1$race.cat)

### Partner level variables

# p_hiv: unk set to NA
artnetSort1$p_hiv <- as.numeric(artnetSort1$p_hiv)
artnetSort1$p_hiv = artnetSort1$p_hiv - 1
artnetSort1$p_hiv[artnetSort1$p_hiv == 2] <- NA

# prep: unk set to NA
artnetSort1$prep.during.part2 <- as.numeric(artnetSort1$prep.during.part2)
artnetSort1$prep.during.part2 = artnetSort1$prep.during.part2 - 1
artnetSort1$prep.during.part2[artnetSort1$prep.during.part2 == 2] <- NA

# ptype set to factor
artnetSort1$ptype = factor(artnetSort1$ptype,  labels = c("Main", "Casual", "Once"))

### Selection variables

# p_hiv selection indicator (R.hiv)
artnetSort1$R.hiv <- NA
artnetSort1$R.hiv[which(!is.na(artnetSort1$p_hiv))] <- 1
artnetSort1$R.hiv[which(is.na(artnetSort1$p_hiv))] <- 0


### Joint data frame

# Outcomes (p_hiv and R.hiv)
n <- nrow(artnetSort1)
y <- matrix(NA, nrow = 2 * n, ncol = 2)
y[1:n, 1] <- artnetSort1$p_hiv
y[n + 1:n, 2] <- artnetSort1$R.hiv

# Intercepts
I <- matrix(NA, nrow = 2 * n, ncol = 2)
I[1:n, 1] <- 1
I[n + 1:n, 2] <- 1

#### Partner's HIV Imputation ----

# p_hiv model
p_hiv.inla <- inla(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                           ptype + hiv3 + prep.during.ego2 + 
                           ptype:hiv3 + ptype:prep.during.ego2 +
                           city2 + f(AMIS_ID, model = "iid"),
                   data = artnetSort2, family = "binomial",
                   control.predictor = list(link = 1, compute = TRUE),
                   control.compute = list(config = TRUE))

# selection model
R.hiv.inla <- inla(R.hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                           test + f(AMIS_ID, model = "iid"),
                   data = artnetSort1, family = "binomial",
                   control.predictor = list(link = 1, compute = TRUE),
                   control.compute = list(config = TRUE))

# joint model



# Modeling selection probabilities
R.hiv.inla <- inla(R.hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                           ptype + hiv3 + prep.during.ego2 + 
                           ptype:hiv3 + ptype:prep.during.ego2 +
                           city2 + f(AMIS_ID, model = "iid"),
                   data = artnetSort1, family = "binomial",
                   control.predictor = list(link = 1, compute = TRUE),
                   control.compute = list(config = TRUE))

p_hiv.obs <- which(!is.na(artnetSort1$p_hiv))

n.imp <- 1
R.hiv.pred <- inla.posterior.sample(n.imp, R.hiv.inla)

# Truncating values > 0 to 0
R.hiv.pred[[1]]$latent[R.hiv.pred[[1]]$latent > 0] <- 0

# Imputing with random draw of 0 or 1  
# p = linear predictor transformed to binomial probability
artnetSort2 <- artnetSort1
artnetSort2$R.hiv.pred <- NA
artnetSort2$R.hiv.pred[p_hiv.obs] <- rbinom(length(p_hiv.obs),1,exp(R.hiv.pred[[1]]$latent[p_hiv.obs,]))

p_hiv.pred <- inla(R.hiv.pred ~ p_hiv + f(AMIS_ID, model = "iid"),
                   data = artnetSort2, family = "binomial")
summary(p_hiv.pred)

# this seems like a reasonable model - p_hiv is associated with selection so using this approach I should be able to correct the imputation probabilities away from p_hiv = 1

