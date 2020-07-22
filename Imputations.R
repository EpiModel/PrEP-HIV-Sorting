rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

library("lme4")
library("gee")
library("mice")
library("RCurl")

#### Index imputations -----
artnetWideSort1 <- artnet %>% 
        select(AMIS_ID, city2, 
               hiv3, race.cat, age.cat)

# Changing to factor and 2s to NA
artnetWideSort1$hiv3[artnetWideSort1$hiv3 == 2] <- NA
artnetWideSort1$hiv3 = factor(artnetWideSort1$hiv3, labels = c("Neg", "Pos"))
artnetWideSort1$age.cat = factor(artnetWideSort1$age.cat)
artnetWideSort1$race.cat = factor(artnetWideSort1$race.cat)

# Setting options for the imputations
ini.egos <- mice(artnetWideSort1, maxit = 0) # Dry run to get default settings

ini.egos$visitSequence # Checking default visit sequence
# [1] "AMIS_ID"          
# [2] "city2"            
# [3] "hiv3"            
# [4] "race.cat"             
# [5] "age.cat" 

vis.egos <- 3 # Reordering visit sequence

ini.egos$method # Checking the default method
meth.egos <- c("", "","logreg", "", "") # Setting new methods

ini.egos$predictorMatrix

pred.egos <- ini.egos$predictorMatrix
pred.egos[,] <- 0
pred.egos["hiv3", c("city2", "race.cat", "age.cat")] <- 1

# Imputation model
imp.egos <- mice(artnetWideSort1, m = 1, vis = vis.egos, method = meth.egos, pred = pred.egos)



#### Partners df ----
artnetSort1 <- artnetLong %>% 
        select(AMIS_ID, city2, ptype, 
                hiv3, prep.during.ego2, race.cat, age.cat, 
                p_hiv, prep.during.part2, p_race.cat, p_age.cat_imp)

# Recoding the 2s as NA which is required by MICE
artnetSort1$hiv3[artnetSort1$hiv3 == "Unk"] <- NA
artnetSort1$p_hiv[artnetSort1$p_hiv == "Unk"] <- NA
artnetSort1$prep.during.ego2[artnetSort1$prep.during.ego2 == "Unk"] <- NA
artnetSort1$prep.during.part2[artnetSort1$prep.during.part2 == "Unk"] <- NA

# Changing to factor levels
artnetSort1$hiv3 = factor(artnetSort1$hiv3, labels = c("Neg", "Pos"))
artnetSort1$p_hiv = factor(artnetSort1$p_hiv, labels = c("Neg", "Pos"))
artnetSort1$prep.during.ego2 = factor(artnetSort1$prep.during.ego2, labels = c("No", "Yes"))
artnetSort1$prep.during.part2 = factor(artnetSort1$prep.during.part2, labels = c("No", "Yes"))
artnetSort1$ptype = factor(artnetSort1$ptype)

artnetSort1 %>% count(prep.during.ego2)
# Checking the missing data pattern
md.pattern(artnetSort1, plot = TRUE, rotate.names = TRUE)

p <- md.pairs(artnetSort1)
write.csv(p$rr, file = "aim1_missing_patt_both.csv")
write.csv(p$mm, file = "aim1_missing_patt_neither.csv")

# Setting options for the imputations
ini.part <- mice(artnetSort1, maxit = 0, m = 5) # Dry run to get default settings

ini.part$visitSequence # Checking default visit sequence
        # [1] "AMIS_ID"          
        # [2] "city2"            
        # [3] "ptype"            
        # [4] "hiv3"             
        # [5] "prep.during.ego2" 
        # [6] "race.cat"         
        # [7] "age.cat"          
        # [8] "p_hiv"            
        # [9] "prep.during.part2"
        # [10] "p_race.cat"       
        # [11] "p_age.cat_imp"    

vis.part <- c(4, 8, 9) # Reordering visit sequence

ini.part$method # Checking the default method
meth.part <- c("", "", "", "logreg", "", "", "", "logreg", "logreg", "", "") # Setting new methods

# Establishing new prediction matrix
pred.part <- ini.part$predictorMatrix
pred.part[,] <- 0
pred.part["hiv3", c("city2", "race.cat", "age.cat")] <- 1
pred.part["p_hiv", c("city2", "ptype", "hiv3", "prep.during.ego2", "p_race.cat", "p_age.cat_imp")] <- 1
pred.part["prep.during.part2", c("city2", "ptype", "hiv3", "prep.during.ego2", "p_hiv", "p_race.cat", "p_age.cat_imp")] <- 1

# Imputation Model
imp.part <- mice(artnetSort1, m = 5, vis = vis, method = meth, pred = pred)

# Can create m = n identical datasets by setting maxit = 0
# This is how I will create all the mids to attach
# the index imputations to
imp.part <- mice(artnetSort1, maxit = 0, m = 5, vis = vis.part, method = meth.part, pred = pred.part)
com1 <- complete(imp.part, 1)
com2 <- complete(imp.part, 2)

# com1 %>% count(hiv3)
# 
# 
# glm1 <- glm(p_hiv ~ p_race.cat + p_age.cat_imp + hiv3, data = artnetSort1, family = binomial(link=logit))
# 
# 
# 
# glmer4 <- glmer(p_hiv ~ hiv3 + (1 | AMIS_ID), data = artnetSort1, family = binomial(link=logit))
# 
# glmer5 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + ptype + hiv3 + city2 + (1 | AMIS_ID), data = artnetSort1, family = binomial(link=logit))
# tt <- getME(glmer5, "theta")
# ll <- getME(glmer5, "lower")
# min(tt[ll==0])
# 
# ss <- getME(glmer5,c("theta","fixef"))
# m2 <- update(glmer5, start = ss, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# m3 <- update(glmer5, start = ss, 
#              control = glmerControl(optimizer = "bobyqa",
#                         optCtrl = list(maxfun=2e5)))
# summary(m3)
# 
# glmer7 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat*p_age.cat_imp + ptype + hiv3 + ptype*hiv3 + city2 + (1 | AMIS_ID), 
#                 data = artnetSort1, family = binomial(link=logit))
# 
# ss7 <- getME(glmer7,c("theta","fixef"))
# m7 <- update(glmer7, start = ss7, 
#              control = glmerControl(optimizer = "bobyqa",
#                                     optCtrl = list(maxfun=2e5)))
# 
# 
# glmer8 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + ptype + hiv3 + ptype*hiv3 + city2 + (1 | AMIS_ID), 
#                 data = artnetSort1, family = binomial(link=logit))
# 
# ss8 <- getME(glmer8,c("theta","fixef"))
# m8 <- update(glmer8, start = ss8, 
#              control = glmerControl(optimizer = "bobyqa",
#                                     optCtrl = list(maxfun=2e5)))

# Imputed egos' HIV and now merging with the long dataset
com.egos1 <- complete(imp.egos, 1)
com.egos2 <- com.egos1 %>% select(AMIS_ID, hiv3)

artnetSort2 <- artnetSort1
artnetSort3 <- left_join(artnetSort2, com.egos2, by = "AMIS_ID")  %>% 
        mutate(hiv3 = coalesce(hiv3.x, hiv3.y))
artnetSort3$hiv3.x <- NULL
artnetSort3$hiv3.y <- NULL

artnetSort3 %>% count(hiv3) 
#%>% filter(is.na(hiv3))


# glmer9 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + ptype + hiv3 + ptype*hiv3 + city2 + (1 | AMIS_ID), 
#                 data = artnetSort3, family = binomial(link=logit))
# tt9 <- getME(glmer9, "theta")
# ll9 <- getME(glmer9, "lower")
# min(tt9[ll9==0])
# 
# 
# ss9 <- getME(glmer9,c("theta","fixef"))
# m9 <- update(glmer9, start = ss9, 
#              control = glmerControl(optimizer = "bobyqa",
#                                     optCtrl = list(maxfun=2e5)))
# 
# m9v2 <- allFit(m9, verbose=F)


library("dfoptim")
library("nloptr")
library("optimx")

# m9v3 <- update(glmer9, start = ss9, 
#              control = glmerControl(optimizer = "nlminb",
#                                     optCtrl = list(maxfun=2e5)))
# 
# 
# # Trying the model with the interaction term for race
# # Didn't converge with the bobyqa optimizer
# glmer10 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat*p_age.cat_imp + 
#                          ptype + hiv3 + ptype*hiv3 + city2 + (1 | AMIS_ID), 
#                 data = artnetSort3, family = binomial(link=logit))
# tt10 <- getME(glmer10, "theta")
# ll10 <- getME(glmer10, "lower")
# min(tt10[ll10==0])
# 
# ss10 <- getME(glmer10,c("theta","fixef"))
# m10 <- update(glmer10, start = ss10, 
#              control = glmerControl(optimizer = "bobyqa",
#                                     optCtrl = list(maxfun=2e5)))

# Now filtering out guys with NA or "other" as race
artnetSort4 <- artnetSort3 %>% filter(
        (p_race.cat == "black" | p_race.cat == "white" | p_race.cat == "hispanic") &
        (race.cat == "black" | race.cat == "white" | race.cat == "hispanic"))

# Recoding prep.during.ego2
artnetSort4$prep.during.ego2[artnetSort4$hiv3 == "Pos"] <- "No"
artnetSort4$prep.during.ego2[which(is.na(artnetSort4$prep.during.ego2))] <- "No"

# # The model converged! This may be the solution
# # Need to figure out if it was dropping the race data or the
# # adding the PrEP info that helped
# glmer11 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + 
#                          ptype + hiv3 + prep.during.ego2 + 
#                          hiv3*prep.during.ego2 + hiv3*ptype + prep.during.ego2*ptype + 
#                          hiv3*prep.during.ego2*ptype +
#                          city2 + (1 | AMIS_ID), 
#                  data = artnetSort4, family = binomial(link=logit))
# 
#         tt11 <- getME(glmer11, "theta")
#         ll11 <- getME(glmer11, "lower")
#         min(tt11[ll11==0])
# 
# ss11 <- getME(glmer11,c("theta","fixef"))
# m11 <- update(glmer11, start = ss11, 
#                      control = glmerControl(optimizer = "bobyqa",
#                                             optCtrl = list(maxfun=2e5)))

# Now trying with the full racial data but with the recoded prep
# Recoding prep.during.ego2
# m10 converged and is a candidate model
# 10,869 obs, 4073 egos
artnetSort5 <- artnetSort3
artnetSort5$prep.during.ego2[artnetSort5$hiv3 == "Pos"] <- "No"
artnetSort5$prep.during.ego2[which(is.na(artnetSort5$prep.during.ego2))] <- "No"
   
glmer12 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp +
                         ptype + hiv3 + prep.during.ego2 +
                         hiv3:prep.during.ego2 + hiv3:ptype + prep.during.ego2:ptype +
                         hiv3:prep.during.ego2:ptype +
                         city2 + (1 | AMIS_ID),
                 data = artnetSort5, family = binomial(link=logit))

ss12 <- getME(glmer12,c("theta","fixef"))
m12 <- update(glmer12, start = ss12,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
# 
# 
# # Trying the above model but with the interaction for race*age
# # This did not converge 
# glmer13 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
#                          ptype + hiv3 + prep.during.ego2 + 
#                          hiv3:prep.during.ego2 + hiv3:ptype + prep.during.ego2:ptype + 
#                          hiv3:prep.during.ego2:ptype +
#                          city2 + (1 | AMIS_ID), 
#                  data = artnetSort5, family = binomial(link=logit))
# 
# ss13 <- getME(glmer13,c("theta","fixef"))
# m13 <- update(glmer13, start = ss13, 
#               control = glmerControl(optimizer = "bobyqa",
#                                      optCtrl = list(maxfun=2e5)))
# 
# 
# # Trying the race*age interaction with the revised PrEP
# # Excluding race == other
# # Did not converge
# glmer14 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
#                          ptype + hiv3 + prep.during.ego2 + 
#                          hiv3:prep.during.ego2 + hiv3:ptype + prep.during.ego2:ptype + 
#                          hiv3:prep.during.ego2:ptype +
#                          city2 + (1 | AMIS_ID), 
#                  data = artnetSort4, family = binomial(link=logit))
# ss14 <- getME(glmer14,c("theta","fixef"))
# m14 <- update(glmer14, start = ss14, 
#               control = glmerControl(optimizer = "bobyqa",
#                                      optCtrl = list(maxfun=2e5)))
# 
# 
# # Now excluding race == other and NA
# # Including the interaction but not city2
# glmer15 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
#                          ptype + hiv3 + prep.during.ego2 +
#                          hiv3:prep.during.ego2 + hiv3:ptype + prep.during.ego2:ptype +
#                          hiv3:prep.during.ego2:ptype +
#                          (1 | AMIS_ID),
#                  data = artnetSort4, family = binomial(link=logit))
# ss15 <- getME(glmer15,c("theta","fixef"))
# m15 <- update(glmer15, start = ss15,
#               control = glmerControl(optimizer = "bobyqa",
#                                      optCtrl = list(maxfun=2e5)))
# 
# 
# # Full racial data
# # Including the interaction but not city2
# # Did not converge
# glmer16 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
#                          ptype + hiv3 + prep.during.ego2 +
#                          hiv3:prep.during.ego2 + hiv3:ptype + prep.during.ego2:ptype +
#                          hiv3:prep.during.ego2:ptype +
#                          (1 | AMIS_ID),
#                  data = artnetSort5, family = binomial(link=logit))
# ss16 <- getME(glmer16,c("theta","fixef"))
# m16 <- update(glmer16, start = ss16,
#               control = glmerControl(optimizer = "bobyqa",
#                                      optCtrl = list(maxfun=2e5)))


# Excluding the 66+ partner group, limited racial data
# Does not converge - running out of options
artnetSort6 <- artnetSort3 %>% filter(
        (p_race.cat == "black" | p_race.cat == "white" | p_race.cat == "hispanic") &
                (race.cat == "black" | race.cat == "white" | race.cat == "hispanic"),
                p_age.cat_imp != "66+")
artnetSort6$prep.during.ego2[artnetSort6$hiv3 == "Pos"] <- "No"
artnetSort6$prep.during.ego2[which(is.na(artnetSort6$prep.during.ego2))] <- "No"

# glmer17 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
#                          ptype + hiv3 + prep.during.ego2 +
#                          hiv3:prep.during.ego2 + hiv3:ptype + prep.during.ego2:ptype +
#                          hiv3:prep.during.ego2:ptype +
#                          (1 | AMIS_ID),
#                  data = artnetSort6, family = binomial(link=logit))
# ss17 <- getME(glmer17,c("theta","fixef"))
# m17 <- update(glmer17, start = ss17,
#               control = glmerControl(optimizer = "bobyqa",
#                                      optCtrl = list(maxfun=2e5)))


# # Adding in city2 in case that helps
# glmer18 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
#                          ptype + hiv3 + prep.during.ego2 +
#                          hiv3:prep.during.ego2 + hiv3:ptype + prep.during.ego2:ptype +
#                          hiv3:prep.during.ego2:ptype +
#                          city2 + (1 | AMIS_ID),
#                  data = artnetSort6, family = binomial(link=logit))
# ss18 <- getME(glmer18,c("theta","fixef"))
# m18 <- update(glmer18, start = ss17,
#               control = glmerControl(optimizer = "bobyqa",
#                                      optCtrl = list(maxfun=2e5)))
# 
# 
# # Taking city2 out of a model that previously converged
# glmer19 <- glmer(p_hiv ~ p_race.cat + p_age.cat_imp + 
#                          ptype + hiv3 + prep.during.ego2 + 
#                          hiv3*prep.during.ego2 + hiv3*ptype + prep.during.ego2*ptype + 
#                          hiv3*prep.during.ego2*ptype +
#                          (1 | AMIS_ID), 
#                  data = artnetSort4, family = binomial(link=logit))
# 
# ss19 <- getME(glmer19,c("theta","fixef"))
# m19 <- update(glmer19, start = ss19, 
#              control = glmerControl(optimizer = "bobyqa",
#                                     optCtrl = list(maxfun=2e5)))


#
glmer20 <- glmer(p_hiv ~ ptype + hiv3 + prep.during.ego2 +
                         hiv3*prep.during.ego2 + hiv3*ptype + prep.during.ego2*ptype +
                         hiv3*prep.during.ego2*ptype + city2 +
                         (1 | AMIS_ID),
                 data = artnetSort4, family = binomial(link=logit))

ss20 <- getME(glmer20,c("theta","fixef"))
# m20 <- update(glmer20, start = ss5, 
#              control = glmerControl(optimizer = "bobyqa",
#                                     optCtrl = list(maxfun=2e5)))


glm20 <- glm(p_hiv ~ ptype + hiv3 + prep.during.ego2 + 
                    hiv3*ptype + prep.during.ego2*ptype + city2,
                    data = artnetSort4, family = binomial(link=logit))



glm20.log <- glm(p_hiv ~ ptype + hiv3 + prep.during.ego2 + 
                     hiv3*ptype + prep.during.ego2*ptype + city2,
             data = artnetSort4, family = binomial(link=log))


glm12 <- glm(p_hiv ~ p_race.cat + p_age.cat_imp + 
                    ptype + hiv3 + prep.during.ego2 +
                    hiv3:ptype + prep.during.ego2:ptype +
                    city2, data = artnetSort5, family = binomial(link=logit))


artnetSort7 <- artnetSort4
artnetSort7$p_hiv = as.numeric(artnetSort7$p_hiv)
artnetSort7$p_hiv[artnetSort7$p_hiv == 1] <- 0
artnetSort7$p_hiv[artnetSort7$p_hiv == 2] <- 1

gee20ex <- gee(p_hiv ~ ptype + hiv3 + prep.during.ego2 + 
                     hiv3*ptype + prep.during.ego2*ptype + city2,
             data = artnetSort7, id = AMIS_ID, 
             family = binomial(link=logit),
             corstr = "exchangeable")

gee20un <- gee(p_hiv ~ ptype + hiv3 + prep.during.ego2 + 
                     hiv3*ptype + prep.during.ego2*ptype + city2,
             data = artnetSort7, id = AMIS_ID, 
             family = binomial(link=logit),
             corstr = "unstructured")

gee20un.coef <- coef(gee20un)

ss20gee <- ss20
ss20gee$theta <- 0
ss20gee$fixef <- gee20un.coef

m20gee <- update(glmer20, start = ss20gee, 
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun=2e5)))

#Need to change p_hiv to numeric as below
artnetSort8 <- artnetSort5
artnetSort8$p_hiv = as.numeric(artnetSort8$p_hiv)
artnetSort8$p_hiv[artnetSort8$p_hiv == 1] <- 0
artnetSort8$p_hiv[artnetSort8$p_hiv == 2] <- 1

gee12un <- gee(p_hiv ~ p_race.cat + p_age.cat_imp + 
                     ptype + hiv3 + prep.during.ego2 + 
                     hiv3:ptype + prep.during.ego2:ptype + city2, 
             data = artnetSort8, id = AMIS_ID, 
             family = binomial(link=logit),
             corstr = "unstructured")

# Using the results of the GEE model as starting values for updating glmer
gee12un.coef <- coef(gee12un)

ss12gee <- ss12
ss12gee$theta <- 0
ss12gee$fixef <- gee12un.coef

# Converged but the estimates are very similar to glmer
m12gee <- update(glmer12, start = ss12gee,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun=2e5)))


# Similar to glmer12
inla1 <- inla(p_hiv ~ p_race.cat + p_age.cat_imp +
                      ptype + hiv3 + prep.during.ego2 + 
                      hiv3:ptype + prep.during.ego2:ptype + city2 +
                      f(AMIS_ID, model = "iid"), 
              data = artnetSort8, 
              family = "binomial", control.predictor = list(link = 1))

# Similar to gee20un
inla2 <- inla(p_hiv ~ ptype + hiv3 + prep.during.ego2 + 
                      hiv3:ptype + prep.during.ego2:ptype + city2 +
                      f(AMIS_ID, model = "iid"), 
              data = artnetSort7, family = "binomial",  control.predictor = list(link = 1))


# Ideal model
inla3 <- inla(p_hiv ~ p_race.cat + p_age.cat_imp + p_race.cat:p_age.cat_imp +
                      ptype + hiv3 + prep.during.ego2 + 
                      hiv3:ptype + prep.during.ego2:ptype + city2 +
                      f(AMIS_ID, model = "iid"), 
              data = artnetSort8, family = "binomial", control.predictor = list(link = 1))


# Setting options for the imputations
ini.part <- mice(artnetSort8, maxit = 0, m = 5) # Dry run to get default settings

ini.part$visitSequence # Checking default visit sequence
# [1] "AMIS_ID"          
# [2] "city2"            
# [3] "ptype"            
# [4] "prep.during.ego2" 
# [5] "race.cat"         
# [6] "age.cat"          
# [7] "p_hiv"            
# [8] "prep.during.part2"
# [9] "p_race.cat"       
# [10] "p_age.cat_imp"    
# [11] "hiv3"

vis.part <- c(7) # Reordering visit sequence

ini.part$method # Checking the default method
meth.part <- c("", "", "", "", "", "", "myfunc", "", "", "", "") # Setting new methods

# Establishing new prediction matrix
pred.part <- ini.part$predictorMatrix
pred.part[,] <- 0
pred.part["hiv3", c("city2", "race.cat", "age.cat")] <- 1
pred.part["p_hiv", c("city2", "ptype", "hiv3", "prep.during.ego2", "p_race.cat", "p_age.cat_imp")] <- 1
pred.part["prep.during.part2", c("city2", "ptype", "hiv3", "prep.during.ego2", "p_hiv", "p_race.cat", "p_age.cat_imp")] <- 1

# Imputation Model
imp.part <- mice(artnetSort1, m = 5, vis = vis, method = meth, pred = pred)