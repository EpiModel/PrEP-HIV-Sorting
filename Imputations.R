rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

library("lme4")

artnetSort1 <- artnetLong %>% 
        select(AMIS_ID, city2, ptype, 
                hiv3, prep.during.ego, prep.during.ego2, race.cat, age.cat, 
                p_hiv, prep.during.part, prep.during.part2, p_race.cat, p_age.cat_imp) %>% 
        filter(!is.na(p_race.cat), p_hiv != 2)

artnetSort1 %>% count(p_hiv)

glmer(p_hiv ~ p_race.cat + p_age.cat_imp + hiv3 + (hiv3 | AMIS_ID), data = artnetSort1, family = binomial(link=logit))
