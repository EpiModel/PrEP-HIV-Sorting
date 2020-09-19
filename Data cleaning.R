library("ARTnetData")
library("tidyverse")

artnet <- ARTnet.wide
artnetLong <- ARTnet.long

### Egos

# Age
artnet$age.cat <- rep(NA, nrow(artnet))
artnet$age.cat[artnet$age >= 15 & artnet$age <= 24] <- "15-24"
artnet$age.cat[artnet$age >= 25 & artnet$age <= 34] <- "25-34"
artnet$age.cat[artnet$age >= 35 & artnet$age <= 44] <- "35-44"
artnet$age.cat[artnet$age >= 45 & artnet$age <= 54] <- "45-54"
artnet$age.cat[artnet$age >= 55 & artnet$age <= 65] <- "55-65"
artnet$age.cat[artnet$age > 65] <- "66+"

# Education
artnet$education <- rep(NA, nrow(artnet))
artnet$education[artnet$HLEDUCAT %in% c(0, 1, 2)] <- "Less than High School"
artnet$education[artnet$HLEDUCAT == 3] <- "High School Graduate"
artnet$education[artnet$HLEDUCAT == 4] <- "Some College or Associates/Technical"
artnet$education[artnet$HLEDUCAT == 5] <- "College or Greater"

### Long Data Set

# Age --- using imputed age
# Check on what imputed age is & whether this is most appropriate option
artnetLong$p_age.cat_imp <- rep(NA, nrow(artnetLong))
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 15 & artnetLong$p_age_imp <= 24] <- "15-24"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 25 & artnetLong$p_age_imp <= 34] <- "25-34"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 35 & artnetLong$p_age_imp <= 44] <- "35-44"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 45 & artnetLong$p_age_imp <= 54] <- "45-54"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 55 & artnetLong$p_age_imp <= 65] <- "55-65"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp > 65] <- "66+"


artnetLong$age.cat <- rep(NA, nrow(artnetLong))
artnetLong$age.cat[artnetLong$age >= 15 & artnetLong$age <= 24] <- "15-24"
artnetLong$age.cat[artnetLong$age >= 25 & artnetLong$age <= 34] <- "25-34"
artnetLong$age.cat[artnetLong$age >= 35 & artnetLong$age <= 44] <- "35-44"
artnetLong$age.cat[artnetLong$age >= 45 & artnetLong$age <= 54] <- "45-54"
artnetLong$age.cat[artnetLong$age >= 55 & artnetLong$age <= 65] <- "55-65"
artnetLong$age.cat[artnetLong$age > 65] <- "66+"

# PrEP Use

# PREP_CURRENT PREPCURRENT_AMIS artnetPREP_CURRENT PARTXPREPSTART PARTXPREPUSE PARTXPREPUSE_PART

# Variables for each partner
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

### Cleaning the crude variables

## Partners' HIV
# artnetLong$p_hiv[which(is.na(artnetLong$p_hiv))] <- 2 #  Recoding unknown (NA) partners' hiv as unknown (2)

## Egos' PrEP
# Having trouble with the following line, so I just initialized all values to 2 and then recoded accordingly
# Can't change the NA values for only HIV == 0
# artnetLong$prep.during.ego2[artnetLong$hiv3 == 0 & is.na(artnetLong$prep.during.ego)] <- 2
artnetLong$prep.during.ego2 <- rep(2, nrow(artnetLong))
artnetLong$prep.during.ego2[artnetLong$prep.ever.ego == 0] <- 0 # Recoding Never PrEP as Never During Partnership (was NA)
artnetLong$prep.during.ego2[artnetLong$prep.during.ego == 3] <- 0 # No PrEP during
artnetLong$prep.during.ego2[which(artnetLong$prep.during.ego %in% c(1,2))] <- 1 # Always or Sometimes PrEP
artnetLong$prep.during.ego2[artnetLong$prep.during.ego == 88] <- 2 # I don't know
artnetLong$prep.during.ego2[artnetLong$prep.during.ego == 99] <- NA # Prefer not to say

artnetLong$prep.during.ego2[artnetLong$hiv3 == 1] <- NA # Setting back to NA for HIV-positive
artnetLong$prep.during.ego2[artnetLong$hiv3 == 2] <- NA # Setting back to NA for HIV-unknown
artnetLong$prep.during.ego2[artnetLong$hiv3 == 2 & artnetLong$prep.ever.ego == 0] <- 0 # Unknown HIV but Never used PrEP
artnetLong$prep.during.ego2[artnetLong$hiv3 == 2 & artnetLong$prep.ever.ego == 1] <- 2 # Unknown HIV but HAVE used PrEP before

## Partners' PrEP
# Same issue as above so starting out by initializing all as 2
artnetLong$prep.during.part2 <- rep(2, nrow(artnetLong))
artnetLong$prep.during.part2[artnetLong$prep.during.part == 3] <- 0 # No PrEP during
artnetLong$prep.during.part2[which(artnetLong$prep.during.part %in% c(1,2))] <- 1 # Always or Sometimes PrEP
artnetLong$prep.during.ego2[artnetLong$prep.during.ego == 88] <- 2 # I don't know
artnetLong$prep.during.ego2[artnetLong$prep.during.ego == 99] <- NA # Prefer not to say

artnetLong$prep.during.part2[artnetLong$p_hiv == 1] <- NA # Setting back to NA for HIV-positive

#### Dyads

#HIV mixing
artnetLong$d_hiv <- NULL

artnetLong$d_hiv[artnetLong$hiv3 == 0 & artnetLong$p_hiv == 0] <- "NEGNEG"
artnetLong$d_hiv[artnetLong$hiv3 == 0 & artnetLong$p_hiv == 1] <- "NEGPOS"
artnetLong$d_hiv[artnetLong$hiv3 == 0 & artnetLong$p_hiv == 2] <- "NEGUNK"

artnetLong$d_hiv[artnetLong$hiv3 == 1 & artnetLong$p_hiv == 0] <- "POSNEG"
artnetLong$d_hiv[artnetLong$hiv3 == 1 & artnetLong$p_hiv == 1] <- "POSPOS"
artnetLong$d_hiv[artnetLong$hiv3 == 1 & artnetLong$p_hiv == 2] <- "POSUNK"

artnetLong$d_hiv[artnetLong$hiv3 == 2 & artnetLong$p_hiv == 0] <- "UNKNEG"
artnetLong$d_hiv[artnetLong$hiv3 == 2 & artnetLong$p_hiv == 1] <- "UNKPOS"
artnetLong$d_hiv[artnetLong$hiv3 == 2 & artnetLong$p_hiv == 2] <- "UNKUNK"

### PREP & HIV mixing ###
artnetLong$d_hivprep <- NULL

## Ego == (-) no PrEP
        # Partner == (-) no PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 0 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 0] <- "NOP-NOP"
        
        # Partner == (-) PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 0 &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 1] <- "NOP-YEP"
        
        # Partner == (-) PrEP UNK
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 0 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 2] <- "NOP-PUK"
        
        # Partner == (+)
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 0 & 
                                     artnetLong$p_hiv == 1] <- "NOP-POS"
        
        # Partner == (?)
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 0 & 
                                     artnetLong$p_hiv == 2] <- "NOP-UNK"

## Ego == (-) PrEP
        # Partner == (-) no PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 1 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 0] <- "YEP-NOP"
        
        # Partner == (-) PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 1 &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 1] <- "YEP-YEP"
        
        # Partner == (-) PrEP UNK
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 1 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 2] <- "YEP-PUK"
        
        # Partner == (+)
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 1 & 
                                     artnetLong$p_hiv == 1] <- "YEP-POS"
        
        # Partner == (?)
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego2 == 1 & 
                                     artnetLong$p_hiv == 2] <- "YEP-UNK"

## Ego == (+)
        # Partner == (-) no PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 1 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 0] <- "POS-NOP"
        
        # Partner == (-) PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 1 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 1] <- "POS-YEP"
        
        # Partner == (-) PrEP UNK
        artnetLong$d_hivprep[artnetLong$hiv3 == 1 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 2] <- "POS-PUK"
        
        # Partner == (+)
        artnetLong$d_hivprep[artnetLong$hiv3 == 1 & 
                                     artnetLong$p_hiv == 1] <- "POS-POS"
        
        # Partner == (?)
        artnetLong$d_hivprep[artnetLong$hiv3 == 1 & 
                                     artnetLong$p_hiv == 2] <- "POS-UNK"
        
## Ego == (?)
        # Partner == (-) no PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 0] <- "UNK-NOP"
        
        # Partner == (-) PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 1] <- "UNK-YEP"
        
        # Partner == (-) PrEP UNK
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part2 == 2] <- "UNK-PUK"
        
        # Partner == (+)
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 1] <- "UNK-POS"
        
        # Partner == (?)
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 2] <- "UNK-UNK"
        
#### Changing to factors

artnetLong$hiv3 = factor(artnetLong$hiv3, labels = c("Neg", "Pos", "Unk"))
artnetLong$p_hiv = factor(artnetLong$p_hiv, labels = c("Neg", "Pos", "Unk"))
artnetLong$prep.during.ego2 = factor(artnetLong$prep.during.ego2, labels = c("No", "Yes", "Unk"))
artnetLong$prep.during.part2 = factor(artnetLong$prep.during.part2, labels = c("No", "Yes", "Unk"))

artnetLong$age.cat = factor(artnetLong$age.cat)
artnetLong$race.cat = factor(artnetLong$race.cat)
artnetLong$p_age.cat_imp = factor(artnetLong$p_age.cat_imp)
artnetLong$p_race.cat = factor(artnetLong$p_race.cat)
