library("ARTnetData")
library("tidyverse")

artnet <- ARTnet.wide
artnetLong <- ARTnet.long

#### Egos -----

# Ego Age
artnet$age.cat[artnet$age >= 15 & artnet$age <= 24] <- "15-24"
artnet$age.cat[artnet$age >= 25 & artnet$age <= 34] <- "25-34"
artnet$age.cat[artnet$age >= 35 & artnet$age <= 44] <- "35-44"
artnet$age.cat[artnet$age >= 45 & artnet$age <= 54] <- "45-54"
artnet$age.cat[artnet$age >= 55 & artnet$age <= 65] <- "55-65"
artnet$age.cat[artnet$age > 65] <- "66+"

#### Long Data Set -----

# Partner Age --- using imputed age
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 15 & artnetLong$p_age_imp <= 24] <- "15-24"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 25 & artnetLong$p_age_imp <= 34] <- "25-34"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 35 & artnetLong$p_age_imp <= 44] <- "35-44"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 45 & artnetLong$p_age_imp <= 54] <- "45-54"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 55 & artnetLong$p_age_imp <= 65] <- "55-65"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp > 65] <- "66+"

# Ego Age
artnetLong$age.cat[artnetLong$age >= 15 & artnetLong$age <= 24] <- "15-24"
artnetLong$age.cat[artnetLong$age >= 25 & artnetLong$age <= 34] <- "25-34"
artnetLong$age.cat[artnetLong$age >= 35 & artnetLong$age <= 44] <- "35-44"
artnetLong$age.cat[artnetLong$age >= 45 & artnetLong$age <= 54] <- "45-54"
artnetLong$age.cat[artnetLong$age >= 55 & artnetLong$age <= 65] <- "55-65"
artnetLong$age.cat[artnetLong$age > 65] <- "66+"

# PrEP and HIV for each partner
p1.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART1ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART1PREPUSE, 
                  prep.during.ego.start = PART1PREPSTART, 
                  prep.during.part = PART1PREPUSE_PART,
                  part_hiv = PART1HIV) %>%
        mutate(PARTNER_ID = 1)
        
p2.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART2ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART2PREPUSE, 
                  prep.during.ego.start = PART2PREPSTART, 
                  prep.during.part = PART2PREPUSE_PART,
                  part_hiv = PART2HIV) %>%
        mutate(PARTNER_ID = 2)

p3.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART3ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART3PREPUSE, 
                  prep.during.ego.start = PART3PREPSTART, 
                  prep.during.part = PART3PREPUSE_PART,
                  part_hiv = PART3HIV) %>%
        mutate(PARTNER_ID = 3)

p4.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART4ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART4PREPUSE, 
                  prep.during.ego.start = PART4PREPSTART, 
                  prep.during.part = PART4PREPUSE_PART,
                  part_hiv = PART4HIV) %>%
        mutate(PARTNER_ID = 4)

p5.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART5ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART5PREPUSE, 
                  prep.during.ego.start = PART5PREPSTART, 
                  prep.during.part = PART5PREPUSE_PART,
                  part_hiv = PART5HIV) %>%
        mutate(PARTNER_ID = 5)

# Combining all of the datasets
pall.vars <- rbind(p1.vars, p2.vars, p3.vars, p4.vars, p5.vars)

# Dropping observations where ONCE is NA
pall.vars <- pall.vars[which(!is.na(pall.vars$ONCE)),]

# Dropping ONCE from the DF (already in artnetLong)
pall.vars$ONCE <- NULL

# Adding the new partner variables to artnetLong
artnetLong <- inner_join(artnetLong, pall.vars, by = c("AMIS_ID", "PARTNER_ID"))

# Removing unnecessary objects
rm(p1.vars, p2.vars, p3.vars, p4.vars, p5.vars, pall.vars)

#### Administrative variable to mark observations for deletion (1 == keep, 0 == remove)
artnetLong$keep <- 1

# Restricting to just anal sex
artnetLong$keep[artnetLong$RAI == 0 & artnetLong$IAI == 0] <- 0

#### Removing missing race
artnetLong$keep[which(is.na(artnetLong$p_race.cat))] <- 0

## Egos' PrEP during partnership, Ever == 1 & Never == 0, 0 for all HIV+ and HIV?
artnetLong$keep[artnetLong$hiv3 == 0 & is.na(artnetLong$prep.ever.ego)] <- 0 # Egos were inadvertently not asked about PrEP
artnetLong$keep[artnetLong$prep.ever.ego == 1 & !artnetLong$prep.during.ego %in% c(1,2,3)] <- 0 # Missing info on prep.during.ego

artnetLong$prep.during.ego2[artnetLong$prep.ever.ego == 0] <- 0 # Never PrEP set to Never During Partnership (was NA)
artnetLong$prep.during.ego2[artnetLong$prep.during.ego == 3] <- 0 # No PrEP during
artnetLong$prep.during.ego2[which(artnetLong$prep.during.ego %in% c(1,2))] <- 1 # Always or Sometimes PrEP
artnetLong$prep.during.ego2[which(artnetLong$hiv3 %in% c(1,2))] <- 0 #All HIV+ and HIV-Unknown are missing PrEP

## Partners' HIV
artnetLong$keep[is.na(artnetLong$p_hiv)] <- 0 # p_hiv == NA
artnetLong$keep[artnetLong$part_hiv == 99] <- 0 # "prefer not to say" for part_hiv

## Partners' PrEP
artnetLong$keep[artnetLong$prep.during.part == 99] <- 0 #"prefer not to answer"
artnetLong$keep[artnetLong$p_hiv %in% c(0,2) & is.na(artnetLong$prep.during.part)] <- 0 #prep.during.part == NA

artnetLong$prep.during.part2[artnetLong$prep.during.part == 88] <- 2 # I don't know
artnetLong$prep.during.part2[artnetLong$prep.during.part == 3] <- 0 # No PrEP during
artnetLong$prep.during.part2[which(artnetLong$prep.during.part %in% c(1,2))] <- 1 # Always or Sometimes PrEP
artnetLong$prep.during.part2[artnetLong$p_hiv == 1] <- 0 # Set all HIV+ to No PrEP

## Removing all of the observations marked for deletion
artnetLong <- artnetLong[artnetLong$keep == 1,]

#### Changing to factors
artnetLong$hiv3 = factor(artnetLong$hiv3, labels = c("Neg", "Pos", "Unk"))
artnetLong$p_hiv = factor(artnetLong$p_hiv, labels = c("Neg", "Pos", "Unk"))
artnetLong$prep.during.ego2 = factor(artnetLong$prep.during.ego2, labels = c("No", "Yes"))
artnetLong$prep.during.part2 = factor(artnetLong$prep.during.part2, labels = c("No", "Yes", "Unk"))

artnetLong$age.cat = factor(artnetLong$age.cat)
artnetLong$race.cat = factor(artnetLong$race.cat)
artnetLong$p_age.cat_imp = factor(artnetLong$p_age.cat_imp)
artnetLong$p_race.cat = factor(artnetLong$p_race.cat)

#### Starting dataset for imputations
artnetSort <- artnetLong %>%
        select(AMIS_ID, city2, ptype, 
               hiv3, prep.during.ego2, race.cat, age.cat, 
               p_hiv, prep.during.part2, p_race.cat, p_age.cat_imp)

## Unique ID for each alter
artnetSort$alter_id <- seq(1:nrow(artnetSort))

# Covariates set to factor
artnetSort$age.cat = factor(artnetSort$age.cat)
artnetSort$race.cat = factor(artnetSort$race.cat)
artnetSort$ptype = factor(artnetSort$ptype,  labels = c("Main", "Casual", "Once"))

### Ego level variables

# hiv2: 2 level variable from hiv3
artnetSort$hiv2[artnetSort$hiv3 %in% c("Neg","Unk")] <- 0
artnetSort$hiv2[artnetSort$hiv3 == "Pos"] <- 1

# HIV+, HIV?, No PrEP, & PrEP
artnetSort$hp[artnetSort$hiv3 == "Pos"] <- "Pos"
artnetSort$hp[artnetSort$hiv3 == "Unk"] <- "Unk"
artnetSort$hp[artnetSort$hiv3 == "Neg" & artnetSort$prep.during.ego2 == "No"] <- "NoPrEP"
artnetSort$hp[artnetSort$hiv3 == "Neg" & artnetSort$prep.during.ego2 == "Yes"] <- "PrEP"

### Partner level variables

# p_hiv2: 2 level (Neg == 0; Pos == 1; Unk == NA)
artnetSort$p_hiv2 <- as.numeric(artnetSort$p_hiv)
artnetSort$p_hiv2 = artnetSort$p_hiv2 - 1
artnetSort$p_hiv2[artnetSort$p_hiv2 == 2] <- NA

# HIV+, No PrEP, PrEP, & PrEP Unknown
artnetSort <- artnetSort %>% mutate(
        p_hp = ifelse(p_hiv == "Pos", "Pos",
                      ifelse(prep.during.part2 == "No", "NoPrEP",
                             ifelse(prep.during.part2 == "Yes", "PrEP", "UnkPrEP"))))
