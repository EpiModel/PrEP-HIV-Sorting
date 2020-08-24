rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

library("INLA")

#### Long df ----
artnetSort1 <- artnetLong %>% 
        filter(!is.na(p_race.cat)) %>%
        select(AMIS_ID, city2, ptype, 
               hiv3, prep.during.ego2, race.cat, age.cat, 
               p_hiv, prep.during.part2, p_race.cat, p_age.cat_imp)

### Ego level variables

# prep: assuming no PrEP for those missing
artnetSort1$prep.during.ego2 <- as.numeric(artnetSort1$prep.during.ego2)
artnetSort1$prep.during.ego2 = artnetSort1$prep.during.ego2 - 1
artnetSort1$prep.during.ego2[artnetSort1$prep.during.ego2 == 2] <- 0
artnetSort1$prep.during.ego2[which(is.na(artnetSort1$prep.during.ego2))] <- 0

# Covariates set to factor
artnetSort1$age.cat = factor(artnetSort1$age.cat)
artnetSort1$race.cat = factor(artnetSort1$race.cat)

### Partner level variables

# p_hiv: 2 level (0 == neg or unk; 1 == pos)
artnetSort1$p_hiv2 <- artnetSort1$p_hiv
artnetSort1$p_hiv2 <- as.numeric(artnetSort1$p_hiv2)
artnetSort1$p_hiv2 = artnetSort1$p_hiv2 - 1
artnetSort1$p_hiv2[artnetSort1$p_hiv2 == 2] <- 0

# p_hiv: imputed
artnetSort1$p_hiv.imp <- NA
artnetSort1$p_hiv.imp[artnetSort1$p_hiv == "Pos"] <- 1

# deal with PrEP later
# # prep: unk set to NA
# artnetSort1$prep.during.part2 <- as.numeric(artnetSort1$prep.during.part2)
# artnetSort1$prep.during.part2 = artnetSort1$prep.during.part2 - 1
# artnetSort1$prep.during.part2[artnetSort1$prep.during.part2 == 2] <- NA

# ptype set to factor
artnetSort1$ptype = factor(artnetSort1$ptype,  labels = c("Main", "Casual", "Once"))

#### Imputation model -----
p_hiv2.lme4 <- glmer(p_hiv2 ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                             age.cat + race.cat + age.cat:race.cat +
                             ptype + hiv3 + prep.during.ego2 + 
                             ptype:hiv3 + ptype:prep.during.ego2 +
                             city2 + (1 | AMIS_ID),
                     data = artnetSort1, family = binomial(link=logit))


p_hiv2.inla <- inla(p_hiv2 ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                            age.cat + race.cat + age.cat:race.cat +
                            ptype + hiv3 + prep.during.ego2 + 
                            ptype:hiv3 + ptype:prep.during.ego2 +
                            city2 + f(AMIS_ID, model = "iid"),
                    data = artnetSort1, family = "binomial",
                    control.predictor = list(link = 1, compute = TRUE),
                    control.compute = list(config = TRUE))

# Helpful to inspect values
# p_hiv.neg <- which(artnetSort1$p_hiv == "Neg")
# p_hiv.unk <- which(artnetSort1$p_hiv == "Unk")
# p_hiv.pos <- which(artnetSort1$p_hiv == "Pos")

# write.csv(p_hiv2.pred[[1]]$latent[p_hiv.neg,], file = "hiv.pred.neg.csv")
# write.csv(p_hiv2.pred[[1]]$latent[p_hiv.unk,], file = "hiv.pred.unk.csv")
# write.csv(p_hiv2.pred[[1]]$latent[p_hiv.pos,], file = "hiv.pred.pos.csv")


# Drawing predictive values from posterior distribution
n.imp <- 1
p_hiv2.pred.star <- inla.posterior.sample(n.imp, p_hiv2.inla)
p_hiv2.pred.star[[1]]$latent[p_hiv2.pred.star[[1]]$latent > 0] <- 0

# P(hiv2* = 1)
n.alters <- nrow(artnetSort1)
artnetSort2 <- artnetSort1
artnetSort2$p_hiv2.star1 <- NA
artnetSort2$p_hiv2.star1 <- exp(p_hiv2.pred.star[[1]]$latent[1:n.alters])

## P(hiv2* = 0)
artnetSort2$p_hiv2.star0 <- NA
artnetSort2$p_hiv2.star0 <- 1 - artnetSort2$p_hiv2.star1

sens <- 24/37
spec <- 144/146

# Don't think I will use the pi method, which modifies a median value
# Will likely specify distributions for a few strata and draw randomly
q.sens <- log(sens/(1-sens)) - log(sens.pi/(1-sens.pi))
q.spec <- log(spec/(1-spec)) + log(spec.pi/(1-spec.pi))

artnetSort2$q.sens <- NA
artnetSort2$q.spec <- NA

# Arbitrarily picking values for proof of concept
artnetSort2$q.sens[artnetSort2$ptype == "Main"] <- log(0.98/(1-0.98))
artnetSort2$q.spec[artnetSort2$ptype == "Main"] <- log(0.999/(1-0.999))

artnetSort2$q.sens[artnetSort2$ptype == "Casual"] <- log(0.8/(1-0.8))
artnetSort2$q.spec[artnetSort2$ptype == "Casual"] <- log(0.99/(1-0.99))

artnetSort2$q.sens[artnetSort2$ptype == "Once"] <- log(sens/(1-sens))
artnetSort2$q.spec[artnetSort2$ptype == "Once"] <- log(spec/(1-spec))

# P(Y*=y*|Y,X,R=0)
artnetSort2$sens.xr <- NA
artnetSort2$spec.xr <- NA

artnetSort2$sens.xr <- (artnetSort2$p_hiv2.star1 * exp(artnetSort2$q.sens)) / (artnetSort2$p_hiv2.star0 + (artnetSort2$p_hiv2.star1 * exp(artnetSort2$q.sens)))
artnetSort2$spec.xr <- (artnetSort2$p_hiv2.star0 * exp(artnetSort2$q.spec)) / (artnetSort2$p_hiv2.star1 + (artnetSort2$p_hiv2.star0 * exp(artnetSort2$q.spec)))
        
# P(Y=1|X,R=0)
artnetSort2$p_hiv2.pred <- (artnetSort2$p_hiv2.star1 + artnetSort2$spec.xr - 1) / (artnetSort2$sens.xr + artnetSort2$spec.xr - 1)

