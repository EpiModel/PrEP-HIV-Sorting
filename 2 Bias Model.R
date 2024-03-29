# Corrects bias due to misclassification and unknown values in egocentric data
# Required: data cleaning.R

rm(list = ls())

source('1 Data cleaning.R')

# need to install INLA and load outside of renv -- might be a way to include in lockfile, but I don't know
# https://www.r-inla.org/download-install
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library("INLA")

rm(artnet, artnetLong)


#### Initializing imputation lists ------
nsim <- vector("double", 300)

imp.hiv <- vector("list", length(nsim))
pred.star.hiv <- vector("list", length(nsim))
rv.hiv <- vector("list", length(nsim))

imp.prep <- vector("list", length(nsim))
inla.prep <- vector("list", length(nsim))
pred.star.prep <- vector("list", length(nsim))
rv.prep <- vector("list", length(nsim))

imputations <- vector("list", length(nsim))

#### HIV INLA ------
inla.hiv <- inla(p_hiv2 ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                         age.cat + race.cat + age.cat:race.cat +
                         ptype + hp + ptype:hp +
                         city2 + f(AMIS_ID, model = "iid"),
                 data = artnetSort, family = "binomial",
                 control.predictor = list(link = 1, compute = TRUE),
                 control.compute = list(config = TRUE))


for (i in seq_along(nsim)) {
        
#### HIV Imputation -----
        imp.hiv[[i]] <- artnetSort
        
        ## Predictive values from posterior distribution of regression model ----        
        pred.star.hiv[[i]] <- inla.posterior.sample(1, inla.hiv)
        
        # P(hiv2* = 1)
        imp.hiv[[i]]$star1.hiv <- exp(pred.star.hiv[[i]][[1]]$latent[1:nrow(artnetSort)]) / (1 + exp(pred.star.hiv[[i]][[1]]$latent[1:nrow(artnetSort)]))
        
        # P(hiv2* = 0)
        imp.hiv[[i]]$star0.hiv <- 1 - imp.hiv[[i]]$star1.hiv
        
        ## Random starting sens & spec
        rv.hiv[[i]]$pi.hiv <- median(imp.hiv[[i]]$star1.hiv)
        rv.hiv[[i]]$spec.pos.hiv <- runif(1, 1-rv.hiv[[i]]$pi.hiv + 0.004, 1)
        rv.hiv[[i]]$spec.neg.hiv <- runif(1, 1-rv.hiv[[i]]$pi.hiv, 1-rv.hiv[[i]]$pi.hiv + rv.hiv[[i]]$pi.hiv/2) 
        rv.hiv[[i]]$spec.unk.hiv <- runif(1, rv.hiv[[i]]$spec.neg.hiv, 1)

        rv.hiv[[i]]$sens.base.hiv <- runif(1, 0.98, 1)

        rv.hiv[[i]]$sens.mult.unk.hiv <- 5

        rv.hiv[[i]]$sens.add.main.hiv   <- runif(1, -0.05, -0.03)
        rv.hiv[[i]]$sens.add.casual.hiv <- rv.hiv[[i]]$sens.add.main.hiv + runif(1, -0.03, -0.01)
        rv.hiv[[i]]$sens.add.once.hiv   <- rv.hiv[[i]]$sens.add.casual.hiv + runif(1, -0.045, -0.035)

        rv.hiv[[i]]$sens.mult.pprep.hiv <- runif(1, 0, 1)
        rv.hiv[[i]]$sens.mult.ehiv.hiv <- runif(1, 0, 1)

        imp.hiv[[i]]$sens.mult.pprep.hiv <- ifelse(imp.hiv[[i]]$prep.during.part2 == "Yes", rv.hiv[[i]]$sens.mult.pprep.hiv, 1)
        imp.hiv[[i]]$sens.mult.ehiv.hiv <- ifelse(imp.hiv[[i]]$hiv2 == 1, rv.hiv[[i]]$sens.mult.ehiv.hiv, 1)

        ## Individual target sens & spec
        imp.hiv[[i]]$sens.hiv[imp.hiv[[i]]$ptype == "Main" & imp.hiv[[i]]$p_hiv == "Neg"] <-   rv.hiv[[i]]$sens.base.hiv
        imp.hiv[[i]]$sens.hiv[imp.hiv[[i]]$ptype == "Main" & imp.hiv[[i]]$p_hiv == "Unk"] <-   rv.hiv[[i]]$sens.base.hiv + rv.hiv[[i]]$sens.add.main.hiv * rv.hiv[[i]]$sens.mult.unk.hiv * imp.hiv[[i]]$sens.mult.pprep.hiv[imp.hiv[[i]]$ptype == "Main" & imp.hiv[[i]]$p_hiv == "Unk"]
        
        imp.hiv[[i]]$sens.hiv[imp.hiv[[i]]$ptype == "Casual" & imp.hiv[[i]]$p_hiv == "Neg"] <- rv.hiv[[i]]$sens.base.hiv + rv.hiv[[i]]$sens.add.casual.hiv * imp.hiv[[i]]$sens.mult.pprep.hiv[imp.hiv[[i]]$ptype == "Casual" & imp.hiv[[i]]$p_hiv == "Neg"] * imp.hiv[[i]]$sens.mult.ehiv.hiv[imp.hiv[[i]]$ptype == "Casual" & imp.hiv[[i]]$p_hiv == "Neg"]
        imp.hiv[[i]]$sens.hiv[imp.hiv[[i]]$ptype == "Casual" & imp.hiv[[i]]$p_hiv == "Unk"] <- rv.hiv[[i]]$sens.base.hiv + rv.hiv[[i]]$sens.add.casual.hiv * imp.hiv[[i]]$sens.mult.pprep.hiv[imp.hiv[[i]]$ptype == "Casual" & imp.hiv[[i]]$p_hiv == "Unk"] * rv.hiv[[i]]$sens.mult.unk.hiv
        
        imp.hiv[[i]]$sens.hiv[imp.hiv[[i]]$ptype == "Once" & imp.hiv[[i]]$p_hiv == "Neg"] <-   rv.hiv[[i]]$sens.base.hiv + rv.hiv[[i]]$sens.add.once.hiv * imp.hiv[[i]]$sens.mult.pprep.hiv[imp.hiv[[i]]$ptype == "Once" & imp.hiv[[i]]$p_hiv == "Neg"] * imp.hiv[[i]]$sens.mult.ehiv.hiv[imp.hiv[[i]]$ptype == "Once" & imp.hiv[[i]]$p_hiv == "Neg"]
        imp.hiv[[i]]$sens.hiv[imp.hiv[[i]]$ptype == "Once" & imp.hiv[[i]]$p_hiv == "Unk"] <-   rv.hiv[[i]]$sens.base.hiv + rv.hiv[[i]]$sens.add.once.hiv * imp.hiv[[i]]$sens.mult.pprep.hiv[imp.hiv[[i]]$ptype == "Once" & imp.hiv[[i]]$p_hiv == "Unk"] * rv.hiv[[i]]$sens.mult.unk.hiv
        
        ## q parameters
        imp.hiv[[i]]$q.sens.hiv <- log(imp.hiv[[i]]$sens.hiv/(1-imp.hiv[[i]]$sens.hiv)) - log(rv.hiv[[i]]$pi.hiv/(1-rv.hiv[[i]]$pi.hiv))

        imp.hiv[[i]]$q.sens.hiv[imp.hiv[[i]]$p_hiv == "Pos"] <- 0.01
                
        imp.hiv[[i]]$q.spec.hiv[imp.hiv[[i]]$p_hiv == "Pos"] <- log(rv.hiv[[i]]$spec.pos/(1-rv.hiv[[i]]$spec.pos)) + log(rv.hiv[[i]]$pi/(1-rv.hiv[[i]]$pi))
        imp.hiv[[i]]$q.spec.hiv[imp.hiv[[i]]$p_hiv == "Neg"] <- log(rv.hiv[[i]]$spec.neg/(1-rv.hiv[[i]]$spec.neg)) + log(rv.hiv[[i]]$pi/(1-rv.hiv[[i]]$pi))
        imp.hiv[[i]]$q.spec.hiv[imp.hiv[[i]]$p_hiv == "Unk"] <- log(rv.hiv[[i]]$spec.unk/(1-rv.hiv[[i]]$spec.unk)) + log(rv.hiv[[i]]$pi/(1-rv.hiv[[i]]$pi))
        
        ## P(Y*=y*|Y,X,R=0)
        imp.hiv[[i]]$sens.xr.hiv <- (imp.hiv[[i]]$star1.hiv * exp(imp.hiv[[i]]$q.sens.hiv)) / (imp.hiv[[i]]$star0.hiv + (imp.hiv[[i]]$star1.hiv * exp(imp.hiv[[i]]$q.sens.hiv)))
        imp.hiv[[i]]$spec.xr.hiv <- (imp.hiv[[i]]$star0.hiv * exp(imp.hiv[[i]]$q.spec.hiv)) / (imp.hiv[[i]]$star1.hiv + (imp.hiv[[i]]$star0.hiv * exp(imp.hiv[[i]]$q.spec.hiv)))

        #### HIV Imputations
        # P(Y=1|X,R=0)
        imp.hiv[[i]]$pred.hiv <- (imp.hiv[[i]]$star1.hiv + imp.hiv[[i]]$spec.xr.hiv - 1) / (imp.hiv[[i]]$sens.xr.hiv + imp.hiv[[i]]$spec.xr.hiv - 1)
        
        # p_hiv: imputed
        imp.hiv[[i]]$p_hiv.imp <- rbinom(nrow(artnetSort), 1, imp.hiv[[i]]$pred.hiv)
        
#### PrEP INLA -----
        ## PrEP data set
        imp.prep[[i]] <- imp.hiv[[i]] %>% filter(p_hiv.imp == 0)
       
        # prep: unk set to NA
        imp.prep[[i]]$prep.part <- as.numeric(imp.prep[[i]]$prep.during.part2)
        imp.prep[[i]]$prep.part = imp.prep[[i]]$prep.part - 1
        imp.prep[[i]]$prep.part[imp.prep[[i]]$prep.part == 2] <- NA
        
        # Imputation model
        inla.prep[[i]] <- inla(prep.part ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                                  age.cat + race.cat + age.cat:race.cat +
                                  ptype + hp + ptype:hp +
                                  city2 + f(AMIS_ID, model = "iid"),
                          data = imp.prep[[i]], family = "binomial",
                          control.predictor = list(link = 1, compute = TRUE),
                          control.compute = list(config = TRUE))

#### PrEP Imputation -----
        ## Predictive values from posterior distribution of regression models ----
        pred.star.prep[[i]] <- inla.posterior.sample(1, inla.prep[[i]])
        
        ## P(prep* = 1)
        imp.prep[[i]]$star1.prep <- exp(pred.star.prep[[i]][[1]]$latent[1:nrow(imp.prep[[i]])])/(1+exp(pred.star.prep[[i]][[1]]$latent[1:nrow(imp.prep[[i]])]))
        
        ## P(prep* = 0)
        imp.prep[[i]]$star0.prep <- 1 - imp.prep[[i]]$star1.prep
        
        ## Random starting sens & spec
        rv.prep[[i]]$pi.prep <- median(imp.prep[[i]]$star1.prep)
        rv.prep[[i]]$spec.no.prep <- runif(1, 1 - rv.prep[[i]]$pi.prep + 0.01, 1 - rv.prep[[i]]$pi.prep + 0.04)
        rv.prep[[i]]$spec.unk.prep <- runif(1, rv.prep[[i]]$spec.no.prep, rv.prep[[i]]$spec.no.prep + 0.04)

        rv.prep[[i]]$spec.yes.prep <- 1 - rv.prep[[i]]$pi.prep
        rv.prep[[i]]$spec.add.casual.prep <- runif(1, 0, rv.prep[[i]]$pi.prep)
        rv.prep[[i]]$spec.add.main.prep <- runif(1, rv.prep[[i]]$spec.add.casual.prep, rv.prep[[i]]$pi.prep)
        rv.prep[[i]]$spec.add.once.prep <- runif(1, 0, rv.prep[[i]]$spec.add.casual.prep)
        
        rv.prep[[i]]$sens.base.prep <- runif(1, 0.98, 1)
        
        rv.prep[[i]]$sens.mult.unk.prep <- 3
        
        rv.prep[[i]]$sens.add.main.prep <- runif(1, -0.04, -0.02)
        rv.prep[[i]]$sens.add.casual.prep <- rv.prep[[i]]$sens.add.main.prep + runif(1, -0.03, -0.01)
        rv.prep[[i]]$sens.add.once.prep <- rv.prep[[i]]$sens.add.casual.prep + runif(1, -0.04, -0.02)
        
        rv.prep[[i]]$sens.mult.eprep.prep <- runif(1, 0, 1)
        rv.prep[[i]]$sens.mult.ehiv.prep <- runif(1, 0, 1)
        
        imp.prep[[i]]$sens.mult.eprep.prep <- ifelse(imp.prep[[i]]$prep.during.ego2 == "Yes", rv.prep[[i]]$sens.mult.eprep.prep, 1)
        imp.prep[[i]]$sens.mult.ehiv.prep <- ifelse(imp.prep[[i]]$hiv2 == 1, rv.prep[[i]]$sens.mult.ehiv.prep, 1)
        
        # Individual target sens and spec
        imp.prep[[i]]$spec.prep[imp.prep[[i]]$ptype == "Main" & imp.prep[[i]]$prep.during.part2 == "Yes"] <- rv.prep[[i]]$spec.yes.prep + rv.prep[[i]]$spec.add.main
        imp.prep[[i]]$spec.prep[imp.prep[[i]]$ptype == "Casual" & imp.prep[[i]]$prep.during.part2 == "Yes"] <- rv.prep[[i]]$spec.yes.prep + rv.prep[[i]]$spec.add.casual.prep
        imp.prep[[i]]$spec.prep[imp.prep[[i]]$ptype == "Once" & imp.prep[[i]]$prep.during.part2 == "Yes"] <- rv.prep[[i]]$spec.yes.prep + rv.prep[[i]]$spec.add.once.prep
        
        imp.prep[[i]]$sens.prep[imp.prep[[i]]$ptype == "Main" & imp.prep[[i]]$prep.during.part2 == "No"] <- rv.prep[[i]]$sens.base.prep
        imp.prep[[i]]$sens.prep[imp.prep[[i]]$ptype == "Main" & imp.prep[[i]]$prep.during.part2 == "Unk"] <- rv.prep[[i]]$sens.base.prep + rv.prep[[i]]$sens.add.main.prep * rv.prep[[i]]$sens.mult.unk.prep
        
        imp.prep[[i]]$sens.prep[imp.prep[[i]]$ptype == "Casual" & imp.prep[[i]]$prep.during.part2 == "No"] <- rv.prep[[i]]$sens.base.prep + rv.prep[[i]]$sens.add.casual.prep * imp.prep[[i]]$sens.mult.eprep.prep[imp.prep[[i]]$ptype == "Casual" & imp.prep[[i]]$prep.during.part2 == "No"] * imp.prep[[i]]$sens.mult.ehiv.prep[imp.prep[[i]]$ptype == "Casual" & imp.prep[[i]]$prep.during.part2 == "No"]
        imp.prep[[i]]$sens.prep[imp.prep[[i]]$ptype == "Casual" & imp.prep[[i]]$prep.during.part2 == "Unk"] <- rv.prep[[i]]$sens.base.prep + rv.prep[[i]]$sens.add.casual.prep * rv.prep[[i]]$sens.mult.unk.prep
        
        imp.prep[[i]]$sens.prep[imp.prep[[i]]$ptype == "Once" & imp.prep[[i]]$prep.during.part2 == "No"] <- rv.prep[[i]]$sens.base.prep + rv.prep[[i]]$sens.add.once.prep * imp.prep[[i]]$sens.mult.eprep.prep[imp.prep[[i]]$ptype == "Once" & imp.prep[[i]]$prep.during.part2 == "No"] * imp.prep[[i]]$sens.mult.ehiv.prep[imp.prep[[i]]$ptype == "Once" & imp.prep[[i]]$prep.during.part2 == "No"]
        imp.prep[[i]]$sens.prep[imp.prep[[i]]$ptype == "Once" & imp.prep[[i]]$prep.during.part2 == "Unk"] <- rv.prep[[i]]$sens.base.prep + rv.prep[[i]]$sens.add.once.prep * rv.prep[[i]]$sens.mult.unk.prep

        ## q parameters
        imp.prep[[i]]$q.sens.prep <- log(imp.prep[[i]]$sens.prep/(1-imp.prep[[i]]$sens.prep)) - log(rv.prep[[i]]$pi.prep/(1-rv.prep[[i]]$pi.prep))
        
        imp.prep[[i]]$q.sens.prep[imp.prep[[i]]$prep.during.part2 == "Yes"] <- runif(1, 0.01, 0.1)

        imp.prep[[i]]$q.spec.prep <- log(imp.prep[[i]]$spec.prep / (1 - imp.prep[[i]]$spec.prep)) + log(rv.prep[[i]]$pi.prep / (1 - rv.prep[[i]]$pi.prep))
        
        imp.prep[[i]]$q.spec.prep[imp.prep[[i]]$prep.during.part2 == "No"] <- log(rv.prep[[i]]$spec.no.prep / (1 - rv.prep[[i]]$spec.no.prep)) + log(rv.prep[[i]]$pi.prep / (1 - rv.prep[[i]]$pi.prep))
        imp.prep[[i]]$q.spec.prep[imp.prep[[i]]$prep.during.part2 == "Unk"] <- log(rv.prep[[i]]$spec.unk.prep / (1 - rv.prep[[i]]$spec.unk.prep)) + log(rv.prep[[i]]$pi.prep / (1 - rv.prep[[i]]$pi.prep))

        # P(Y*=y*|Y,X,R=0)
        imp.prep[[i]]$sens.xr.prep <- (imp.prep[[i]]$star1.prep * exp(imp.prep[[i]]$q.sens.prep)) / (imp.prep[[i]]$star0.prep + (imp.prep[[i]]$star1.prep * exp(imp.prep[[i]]$q.sens.prep)))
        imp.prep[[i]]$spec.xr.prep <- (imp.prep[[i]]$star0.prep * exp(imp.prep[[i]]$q.spec.prep)) / (imp.prep[[i]]$star1.prep + (imp.prep[[i]]$star0.prep * exp(imp.prep[[i]]$q.spec.prep)))

        ## PrEP Imputations
        # P(Y=1|X,R=0)
        imp.prep[[i]]$pred.prep <- (imp.prep[[i]]$star1.prep + imp.prep[[i]]$spec.xr.prep - 1) / (imp.prep[[i]]$sens.xr.prep + imp.prep[[i]]$spec.xr.prep - 1)

        # prep: imputed
        imp.prep[[i]]$prep.imp <- rbinom(nrow(imp.prep[[i]]), 1, imp.prep[[i]]$pred.prep)
        
#### Combining the datasets -----
        imp.prep[[i]] <- imp.prep[[i]] %>% select(alter_id, 
                                                  star1.prep, star0.prep,
                                                  sens.mult.eprep.prep, sens.mult.ehiv.prep,
                                                  spec.prep, sens.prep,
                                                  q.spec.prep, q.sens.prep,
                                                  sens.xr.prep, spec.xr.prep,
                                                  pred.prep, prep.imp)
        
        imputations[[i]] <- left_join(imp.hiv[[i]], imp.prep[[i]], by = "alter_id")

        saveRDS(imputations[[i]], file = paste("imp", i, ".rds", sep = ""))
        
        imp.hiv <- vector("list", length(nsim))
        pred.star.hiv <- vector("list", length(nsim))
        rv.hiv <- vector("list", length(nsim))
        
        imp.prep <- vector("list", length(nsim))
        inla.prep <- vector("list", length(nsim))
        pred.star.prep <- vector("list", length(nsim))
        rv.prep <- vector("list", length(nsim))
        
        imputations <- vector("list", length(nsim))
        
}      
