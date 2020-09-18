rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

library("INLA")
library("lme4")
library("purrr")

#### Long df ----
artnetSort1 <- artnetLong %>% 
        filter(!is.na(p_race.cat), !is.na(p_hiv)) %>%
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

#p_hiv: dummy variables
artnetSort1$p_pos <- NA
artnetSort1$p_pos[artnetSort1$p_hiv == "Pos"] <- 1
artnetSort1$p_pos[artnetSort1$p_hiv == "Neg" | artnetSort1$p_hiv == "Unk"] <- 0

artnetSort1$p_neg <- NA
artnetSort1$p_neg[artnetSort1$p_hiv == "Neg"] <- 1
artnetSort1$p_neg[artnetSort1$p_hiv == "Pos" | artnetSort1$p_hiv == "Unk"] <- 0

artnetSort1$p_unk <- NA
artnetSort1$p_unk[artnetSort1$p_hiv == "Unk"] <- 1
artnetSort1$p_unk[artnetSort1$p_hiv == "Pos" | artnetSort1$p_hiv == "Neg"] <- 0

# deal with PrEP later
# # prep: unk set to NA
# artnetSort1$prep.during.part2 <- as.numeric(artnetSort1$prep.during.part2)
# artnetSort1$prep.during.part2 = artnetSort1$prep.during.part2 - 1
# artnetSort1$prep.during.part2[artnetSort1$prep.during.part2 == 2] <- NA

# ptype set to factor
artnetSort1$ptype = factor(artnetSort1$ptype,  labels = c("Main", "Casual", "Once"))

#### Imputation models -----



artnetSort1$p_hiv3 <- artnetSort1$p_hiv2
artnetSort1$p_hiv3[artnetSort1$p_hiv == "Unk"] <- NA

# This induces too strong of a relationship with hiv3 and the predictive values
# p_hiv2.inla <- inla(p_hiv2 ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
#                             age.cat + race.cat + age.cat:race.cat +
#                             ptype + hiv3 + prep.during.ego2 + 
#                             ptype:hiv3 + ptype:prep.during.ego2 +
#                             city2 + f(AMIS_ID, model = "iid"),
#                     data = artnetSort1, family = "binomial",
#                     control.predictor = list(link = 1, compute = TRUE),
#                     control.compute = list(config = TRUE))

p_hiv2.inla3 <- inla(p_hiv3 ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                             age.cat + race.cat + age.cat:race.cat +
                             city2 + f(AMIS_ID, model = "iid"),
                     data = artnetSort1, family = "binomial",
                     control.predictor = list(link = 1, compute = TRUE),
                     control.compute = list(config = TRUE))


#### Drawing predictive values from posterior distribution ----
n.imp <- 1
p_hiv2.pred.star <- inla.posterior.sample(n.imp, p_hiv2.inla3)
p_hiv2.pred.star[[1]]$latent[p_hiv2.pred.star[[1]]$latent > 0] <- -0.001

# P(hiv2* = 1)
n.alters <- nrow(artnetSort1)
artnetSort1$p_hiv2.star1 <- exp(p_hiv2.pred.star[[1]]$latent[1:n.alters])

## P(hiv2* = 0)
artnetSort1$p_hiv2.star0 <- 1 - artnetSort1$p_hiv2.star1

# Creating a 2 level variable
artnetSort1$hiv2[artnetSort1$hiv3 %in% c("Neg","Unk")] <- 0
artnetSort1$hiv2[artnetSort1$hiv3 == "Pos"] <- 1

# # Using the median is just not working. Some values are way too high for observations
# # Might make more sense to do this for just a few conbimations of y* and x
# ## Median P(hiv2* = 1)
# n.imp.med <- 25
# p_hiv2.pred.star.med <- inla.posterior.sample(n.imp.med, p_hiv2.inla3)
# 
# # different way of doing what is below
# # ll <- lapply(p_hiv2.pred.star.med, function(x) x$latent <- ifelse(x$latent > 0, -0.001, x$latent))
# 
# ll <- lapply(p_hiv2.pred.star.med, function(x) {
#         x$latent <- ifelse(x$latent > 0, -0.001, x$latent)
#         x$latent
# })
# 
# df <- transpose(ll)
# df2 <- lapply(df, function(x) as.numeric(x))
# df3 <- vapply(df2, median, numeric(1))
# df4 <- exp(df3[1:n.alters])



#### q parameters ----
pi <- median(artnetSort1$p_hiv2.star1)
spec.pos <- runif(1, 0.986, 1)
spec.neg <- runif(1, 0.986, spec.pos)
spec.unk <- runif(1, spec.pos, 1)
        
sens.base <- runif(1, 0.98, 1)

sens.mult.unk <- 5

sens.add.main <- runif(1, -0.05, -0.03)
sens.add.casual <-sens.add.main + runif(1, -0.03, -0.01)
sens.add.once <- sens.add.casual + runif(1, -0.045, -0.035)

sens.mult.part.prep <- runif(1, 0, 1)
sens.mult.ego.hiv <- runif(1, 0, 1)

artnetSort1$sens.mult.part.prep <- ifelse(artnetSort1$prep.during.part2 == "Yes", sens.mult.part.prep, 1)
artnetSort1$sens.mult.ego.hiv <- ifelse(artnetSort1$hiv2 == 1, sens.mult.ego.hiv, 1)

# Lowest sensitivity
# pi <- median(artnetSort1$p_hiv2.star1)
# spec <- 0.986 #runif(1, median(artnetSort1$p_hiv2.star1), 1)
# sens.base <- 0.98 #runif(1, 0.98, 1)
# 
# sens.mult.unk <- 5
# 
# sens.add.main <- -0.05 # runif(1, -0.05, -0.03)
# sens.add.casual <- -0.05-0.03 # sens.add.main + runif(1, -0.03, -0.01)
# sens.add.once <- -0.05-0.03-0.045 #sens.add.casual + runif(1, -0.045, -0.035)
# 
# sens.mult.part.prep <- 1 #runif(1, 0, 1)
# sens.mult.ego.hiv <- 1 #runif(1, 0, 1)
# 
# artnetSort1$sens.mult.part.prep <- ifelse(artnetSort1$prep.during.part2 == "Yes", sens.mult.part.prep, 1)
# artnetSort1$sens.mult.ego.hiv <- ifelse(artnetSort1$hiv2 == 1, sens.mult.ego.hiv, 1)

# Highest sensitivity
# pi <- median(artnetSort1$p_hiv2.star1)
# spec <- 0.99 #runif(1, median(artnetSort1$p_hiv2.star1), 1)
# sens.base <- 0.999 #runif(1, 0.98, 1)
# 
# sens.mult.unk <- 5
# 
# sens.add.main <- -0.03 # runif(1, -0.05, -0.03)
# sens.add.casual <- -0.03-0.01 # sens.add.main + runif(1, -0.03, -0.01)
# sens.add.once <- -0.03-0.01-0.035 #sens.add.casual + runif(1, -0.045, -0.035)
# 
# sens.mult.part.prep <- 0 #runif(1, 0, 1)
# sens.mult.ego.hiv <- 0 #runif(1, 0, 1)
# 
# artnetSort1$sens.mult.part.prep <- ifelse(artnetSort1$prep.during.part2 == "Yes", sens.mult.part.prep, 1)
# artnetSort1$sens.mult.ego.hiv <- ifelse(artnetSort1$hiv2 == 1, sens.mult.ego.hiv, 1)


## Median q parameters
pi <- median(artnetSort1$p_hiv2.star1)
spec.neg <- 0.9895
spec.pos <- 0.993 #runif(1, 0.986, 1)
spec.unk <- 0.9965
sens.base <- 0.99 #runif(1, 0.98, 1)

sens.mult.unk <- 5

sens.add.main <- -0.04 #runif(1, -0.05, -0.03)
sens.add.casual <-sens.add.main + -0.02 #runif(1, -0.03, -0.01)
sens.add.once <- sens.add.casual + -0.04 #runif(1, -0.045, -0.035)

sens.mult.part.prep <- 0 #runif(1, 0, 1)
sens.mult.ego.hiv <- 0 #runif(1, 0, 1)

artnetSort1$sens.mult.part.prep <- ifelse(artnetSort1$prep.during.part2 == "Yes", sens.mult.part.prep, 1)
artnetSort1$sens.mult.ego.hiv <- ifelse(artnetSort1$hiv2 == 1, sens.mult.ego.hiv, 1)

# calculating target sens and spec
artnetSort1$sens[artnetSort1$ptype == "Main" & artnetSort1$p_hiv == "Neg"] <- sens.base
artnetSort1$sens[artnetSort1$ptype == "Main" & artnetSort1$p_hiv == "Unk"] <- sens.base + sens.add.main * sens.mult.unk * artnetSort1$sens.mult.part.prep[artnetSort1$ptype == "Main" & artnetSort1$p_hiv == "Unk"]

artnetSort1$sens[artnetSort1$ptype == "Casual" & artnetSort1$p_hiv == "Neg"] <- sens.base + sens.add.casual * artnetSort1$sens.mult.part.prep[artnetSort1$ptype == "Casual" & artnetSort1$p_hiv == "Neg"] * artnetSort1$sens.mult.ego.hiv[artnetSort1$ptype == "Casual" & artnetSort1$p_hiv == "Neg"]
artnetSort1$sens[artnetSort1$ptype == "Casual" & artnetSort1$p_hiv == "Unk"] <- sens.base + sens.add.casual * sens.mult.unk * artnetSort1$sens.mult.part.prep[artnetSort1$ptype == "Casual" & artnetSort1$p_hiv == "Unk"]

artnetSort1$sens[artnetSort1$ptype == "Once" & artnetSort1$p_hiv == "Neg"] <- sens.base + sens.add.once * artnetSort1$sens.mult.part.prep[artnetSort1$ptype == "Once" & artnetSort1$p_hiv == "Neg"] * artnetSort1$sens.mult.ego.hiv[artnetSort1$ptype == "Once" & artnetSort1$p_hiv == "Neg"]
artnetSort1$sens[artnetSort1$ptype == "Once" & artnetSort1$p_hiv == "Unk"] <- sens.base + sens.add.once * sens.mult.unk * artnetSort1$sens.mult.part.prep[artnetSort1$ptype == "Once" & artnetSort1$p_hiv == "Unk"]

artnetSort1$q.sens <- log(artnetSort1$sens/(1-artnetSort1$sens)) - log(pi/(1-pi))

artnetSort1$q.spec[artnetSort1$p_hiv == "Pos"] <- log(spec.pos/(1-spec.pos)) + log(pi/(1-pi))
artnetSort1$q.spec[artnetSort1$p_hiv == "Neg"] <- log(spec.neg/(1-spec.neg)) + log(pi/(1-pi))
artnetSort1$q.spec[artnetSort1$p_hiv == "Unk"] <- log(spec.unk/(1-spec.unk)) + log(pi/(1-pi))

artnetSort1$q.sens[artnetSort1$p_hiv == "Pos"] <- 0.01

# P(Y*=y*|Y,X,R=0)
artnetSort1$sens.xr <- NA
artnetSort1$spec.xr <- NA

artnetSort1$sens.xr <- (artnetSort1$p_hiv2.star1 * exp(artnetSort1$q.sens)) / (artnetSort1$p_hiv2.star0 + (artnetSort1$p_hiv2.star1 * exp(artnetSort1$q.sens)))
artnetSort1$spec.xr <- (artnetSort1$p_hiv2.star0 * exp(artnetSort1$q.spec)) / (artnetSort1$p_hiv2.star1 + (artnetSort1$p_hiv2.star0 * exp(artnetSort1$q.spec)))

#### HIV Imputations ------        
# P(Y=1|X,R=0)
artnetSort1$p_hiv2.pred <- (artnetSort1$p_hiv2.star1 + artnetSort1$spec.xr - 1) / (artnetSort1$sens.xr + artnetSort1$spec.xr - 1)

# p_hiv: imputed
artnetSort1$p_hiv.imp <- NA
artnetSort1$p_hiv.imp <- rbinom(nrow(artnetSort1),1,artnetSort1$p_hiv2.pred)

# table(artnetSort1$p_hiv, artnetSort1$p_hiv.imp)
# table(artnetSort1$p_hiv.imp)
# 
# table(artnetSort1$hiv2, artnetSort1$p_hiv.imp)
# table(artnetSort1$prep.during.part2, artnetSort1$p_hiv.imp)
# table(artnetSort1$prep.during.part2, artnetSort1$p_hiv)
# 
# mean(artnetSort1$p_hiv2.pred)
# by(artnetSort1$p_hiv2.pred, artnetSort1$p_hiv, mean)
# 
# artnetSort3 <- filter(artnetSort1, p_hiv == "Unk", hiv3 == "Pos")
# by(artnetSort3$p_hiv2.pred, artnetSort3$ptype, mean)

# table(artnetSort1$ptype, artnetSort1$p_age.cat_imp)
 
# table(artnetSort1$hiv2, artnetSort1$p_hiv.imp, artnetSort1$ptype)
# table(artnetSort1$p_hiv, artnetSort1$p_hiv.imp, artnetSort1$ptype)
# table(artnetSort1$p_hiv2, artnetSort1$p_hiv.imp, artnetSort1$prep.during.part2)


#### PrEP Sorting ------

## Data set
artnetSort2 <- artnetSort1 %>% filter(artnetSort1$p_hiv.imp == 0)

# prep: set NA to UNK
artnetSort2$prep.during.part2[is.na(artnetSort2$prep.during.part2)] <- "Unk"

# prep: unk set to NA
artnetSort2$prep.part <- as.numeric(artnetSort2$prep.during.part2)
artnetSort2$prep.part = artnetSort2$prep.part - 1
artnetSort2$prep.part[artnetSort2$prep.part == 2] <- NA

# Imputation model
prep.inla <- inla(prep.part ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                             age.cat + race.cat + age.cat:race.cat +
                             city2 + f(AMIS_ID, model = "iid"),
                     data = artnetSort2, family = "binomial",
                     control.predictor = list(link = 1, compute = TRUE),
                     control.compute = list(config = TRUE))


# table(artnetSort2$prep.during.part2, useNA = "ifany")

# table(artnetSort1$prep.during.part2, artnetSort1$p_hiv.imp)
# table(artnetSort1$prep.during.part2)
 
# by(artnetSort1$p_hiv2.pred, artnetSort1$prep.during.part2, mean)


#### Drawing predictive values from posterior distribution ----
n.imp <- 1
prep.pred.star <- inla.posterior.sample(n.imp, prep.inla)
prep.pred.star[[1]]$latent[prep.pred.star[[1]]$latent > 0] <- -0.001

## P(prep* = 1)
n.alters <- nrow(artnetSort2)
artnetSort2$prep.star1 <- exp(prep.pred.star[[1]]$latent[1:n.alters])

## P(prep* = 0)
artnetSort2$prep.star0 <- 1 - artnetSort2$prep.star1

#### q parameters ----
prep.pi <- median(artnetSort2$prep.star1)
prep.spec.yes <- runif(1, 0.986, 1)
prep.spec.no <- runif(1, 0.986, spec.pos)
prep.spec.unk <- runif(1, spec.pos, 1)

prep.sens.base <- runif(1, 0.98, 1)

prep.sens.mult.unk <- 5

prep.sens.add.main <- runif(1, -0.05, -0.03)
prep.sens.add.casual <-prep.sens.add.main + runif(1, -0.03, -0.01)
prep.sens.add.once <- prep.sens.add.casual + runif(1, -0.045, -0.035)

prep.sens.mult.ego.prep <- runif(1, 0, 1)
prep.sens.mult.ego.hiv <- runif(1, 0, 1)

artnetSort2$prep.sens.mult.ego.prep <- ifelse(artnetSort2$prep.during.ego2 == 1, prep.sens.mult.ego.prep, 1)
artnetSort2$prep.sens.mult.ego.hiv <- ifelse(artnetSort2$hiv2 == 1, prep.sens.mult.ego.hiv, 1)


# median q parameters
prep.pi <- median(artnetSort2$prep.star1)
prep.spec.no <- 0.8
prep.spec.yes <- 0.75
prep.spec.unk <- 0.9965

prep.sens.base <- 0.99 #runif(1, 0.98, 1)

prep.sens.mult.unk <- 5

prep.sens.add.main <- -0.04 #runif(1, -0.05, -0.03)
prep.sens.add.casual <- prep.sens.add.main + -0.02 #runif(1, -0.03, -0.01)
prep.sens.add.once <- prep.sens.add.casual + -0.04 #runif(1, -0.045, -0.035)

prep.sens.mult.ego.prep <- 0 #runif(1, 0, 1)
prep.sens.mult.ego.hiv <- 0 #runif(1, 0, 1)

artnetSort2$prep.sens.mult.ego.prep <- ifelse(artnetSort2$prep.during.ego2 == 1, prep.sens.mult.ego.prep, 1)
artnetSort2$prep.sens.mult.ego.hiv <- ifelse(artnetSort2$hiv2 == 1, prep.sens.mult.ego.hiv, 1)

# calculating target sens and spec
artnetSort2$prep.sens[artnetSort2$ptype == "Main" & artnetSort2$prep.during.part2 == "No"] <- prep.sens.base
artnetSort2$prep.sens[artnetSort2$ptype == "Main" & artnetSort2$prep.during.part2 == "Unk"] <- prep.sens.base + prep.sens.add.main * prep.sens.mult.unk

artnetSort2$prep.sens[artnetSort2$ptype == "Casual" & artnetSort2$prep.during.part2 == "No"] <- prep.sens.base + prep.sens.add.casual * artnetSort2$prep.sens.mult.ego.prep[artnetSort2$ptype == "Casual" & artnetSort2$prep.during.part2 == "No"] * artnetSort2$prep.sens.mult.ego.hiv[artnetSort2$ptype == "Casual" & artnetSort2$prep.during.part2 == "No"]
artnetSort2$prep.sens[artnetSort2$ptype == "Casual" & artnetSort2$prep.during.part2 == "Unk"] <- prep.sens.base + prep.sens.add.casual * prep.sens.mult.unk

artnetSort2$prep.sens[artnetSort2$ptype == "Once" & artnetSort2$prep.during.part2 == "No"] <- prep.sens.base + prep.sens.add.once * artnetSort2$prep.sens.mult.ego.prep[artnetSort2$ptype == "Once" & artnetSort2$prep.during.part2 == "No"] * artnetSort2$prep.sens.mult.ego.hiv[artnetSort2$ptype == "Once" & artnetSort2$prep.during.part2 == "No"]
artnetSort2$prep.sens[artnetSort2$ptype == "Once" & artnetSort2$prep.during.part2 == "Unk"] <- prep.sens.base + prep.sens.add.once * prep.sens.mult.unk

# artnetSort2$prep.sens[artnetSort2$prep.during.part2 == "Yes"] <- 0.999

artnetSort2$prep.q.sens <- log(artnetSort2$prep.sens/(1-artnetSort2$prep.sens)) - log(prep.pi/(1-prep.pi))
artnetSort2$prep.q.sens[artnetSort2$prep.during.part2 == "Yes"] <- 0.01

artnetSort2$prep.q.spec[artnetSort2$prep.during.part2 == "No"] <- log(prep.spec.no/(1-prep.spec.no)) + log(prep.pi/(1-prep.pi))
artnetSort2$prep.q.spec[artnetSort2$prep.during.part2 == "Yes"] <- log(prep.spec.yes/(1-prep.spec.yes)) + log(prep.pi/(1-prep.pi))
artnetSort2$prep.q.spec[artnetSort2$prep.during.part2 == "Unk"] <- log(prep.spec.unk/(1-prep.spec.unk)) + log(prep.pi/(1-prep.pi))

# P(Y*=y*|Y,X,R=0)
artnetSort2$prep.sens.xr <- NA
artnetSort2$prep.spec.xr <- NA

artnetSort2$prep.sens.xr <- (artnetSort2$prep.star1 * exp(artnetSort2$prep.q.sens)) / (artnetSort2$prep.star0 + (artnetSort2$prep.star1 * exp(artnetSort2$prep.q.sens)))
artnetSort2$prep.spec.xr <- (artnetSort2$prep.star0 * exp(artnetSort2$prep.q.spec)) / (artnetSort2$prep.star1 + (artnetSort2$prep.star0 * exp(artnetSort2$prep.q.spec)))

#### PrEP Imputations ------        
# P(Y=1|X,R=0)
artnetSort2$prep.pred <- (artnetSort2$prep.star1 + artnetSort2$prep.spec.xr - 1) / (artnetSort2$prep.sens.xr + artnetSort2$prep.spec.xr - 1)

# prep: imputed
artnetSort2$prep.imp <- NA
artnetSort2$prep.imp <- rbinom(nrow(artnetSort2),1,artnetSort2$prep.pred)


by(artnetSort2$prep.pred, artnetSort2$prep.during.part2, mean)

by(artnetSort2$prep.star1, artnetSort2$prep.during.part2, mean)
table(artnetSort2$prep.during.part2, artnetSort2$ptype, useNA = "ifany")
