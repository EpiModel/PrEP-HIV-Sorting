## Standardized HIV  ---- 

## Standard Population

# Single obs for each ego in sample pop
IDs <- artnetSort %>% select(AMIS_ID) %>% unique()
egos.stand <- left_join(IDs, artnet, by = "AMIS_ID")

alters.stand <- artnetSort %>% 
        select(p_age.cat_imp, p_race.cat, city2) %>%
        rename(age.cat = p_age.cat_imp, race.cat = p_race.cat) %>%
        filter(!age.cat == "66+")


glm.hiv <- glm(hiv2 ~ age.cat*race.cat + city2,
               data = egos.stand,
               family = binomial(link = "logit"))

pred.hiv <- predict(glm.hiv, newdata = alters.stand, type = "response")
mean(pred.hiv) #0.09910399

hiv2 <- rbinom(9830,1,pred.hiv)

alters.stand$hiv2 <- hiv2

## Standardized PrEP ----
egos.stand2 <- egos.stand %>% 
        filter(hiv2 == 0) %>%
        mutate(prep_curr = ifelse(hiv3 == 2, 0,
                           ifelse(PREP_REVISED == 0, 0,
                           ifelse(artnetPREP_CURRENT == 0, 0,
                           ifelse(artnetPREP_CURRENT == 1, 1, NA))))) %>%
        filter(prep_curr %in% c(0,1))

alters.stand2 <- alters.stand %>% filter(hiv2 == 0)

glm.prep <- glm(prep_curr ~ age.cat*race.cat + city2,
               data = egos.stand2,
               family = binomial(link = "logit"))

pred.prep <- predict(glm.prep, newdata = alters.stand2, type = "response")
mean(pred.prep) #0.1834529