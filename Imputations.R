rm(list = ls())

source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

library("lme4")
library("gee")
library("mice")

artnetSort1 <- artnetLong %>% 
        select(AMIS_ID, city2, ptype, 
                hiv3, prep.during.ego2, race.cat, age.cat, 
                p_hiv, prep.during.part2, p_race.cat, p_age.cat_imp)
        
# %>% filter(!is.na(p_race.cat), p_hiv != 2)

artnetSort1 %>% count(p_race.cat)

# Recoding the 2s as NA which is required by MICE
artnetSort1$hiv3[artnetSort1$hiv3 == "Unk"] <- NA
artnetSort1$p_hiv[artnetSort1$p_hiv == "Unk"] <- NA
artnetSort1$prep.during.ego2[artnetSort1$prep.during.ego2 == "Unk"] <- NA
artnetSort1$prep.during.part2[artnetSort1$prep.during.part2 == "Unk"] <- NA

# Changing factor levels
artnetSort1$hiv3 = factor(artnetSort1$hiv3, labels = c("Neg", "Pos"))
artnetSort1$p_hiv = factor(artnetSort1$p_hiv, labels = c("Neg", "Pos"))
artnetSort1$prep.during.ego2 = factor(artnetSort1$prep.during.ego2, labels = c("No", "Yes"))
artnetSort1$prep.during.part2 = factor(artnetSort1$prep.during.part2, labels = c("No", "Yes"))


# Creating new empty variables for misclassification correction
# artnetSort1$hiv3_reclass <- artnetSort1$hiv3
# artnetSort1$hiv3_reclass <- NA
# artnetSort1$p_hiv_reclass <- artnetSort1$p_hiv
# artnetSort1$p_hiv_reclass <- NA

# Checking the missing data pattern

md.pattern(artnetSort1, plot = TRUE, rotate.names = TRUE)

p <- md.pairs(artnetSort1)
write.csv(p$rr, file = "aim1_missing_patt_both.csv")
write.csv(p$mm, file = "aim1_missing_patt_neither.csv")

# Dry run to get default settings
ini <- mice(artnetSort1, maxit = 0)

# Reordering visitation

ini$visitSequence

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

vis <- c(4, 8, 9)

# Method

ini$method

meth <- c("", "", "", "logreg", "", "", "", "logreg", "logreg", "", "")

# Prediction matrix

pred <- ini$predictorMatrix
pred[,] <- 0
pred["hiv3", c("city2", "race.cat", "age.cat")] <- 1
pred["p_hiv", c("city2", "ptype", "hiv3", "prep.during.ego2", "p_race.cat", "p_age.cat_imp")] <- 1
pred["prep.during.part2", c("city2", "ptype", "hiv3", "prep.during.ego2", "p_hiv", "p_race.cat", "p_age.cat_imp")] <- 1

# Imputation Model
imp <- mice(artnetSort1, maxit = 0, vis = vis, method = meth, pred = pred)



pred1 <- imp$predictorMatrix


artnetSort1 %>% count(hiv3)

glm1 <- glm(p_hiv ~ p_race.cat + p_age.cat_imp + hiv3, data = artnetSort1, family = binomial(link=logit))

