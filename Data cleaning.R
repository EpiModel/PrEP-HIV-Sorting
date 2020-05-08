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


### Partners

# Age --- using imputed age
# Check on what imputed age is & whether this is most appropriate option
artnetLong$p_age.cat_imp <- rep(NA, nrow(artnetLong))
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 15 & artnetLong$p_age_imp <= 24] <- "15-24"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 25 & artnetLong$p_age_imp <= 34] <- "25-34"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 35 & artnetLong$p_age_imp <= 44] <- "35-44"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 45 & artnetLong$p_age_imp <= 54] <- "45-54"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp >= 55 & artnetLong$p_age_imp <= 65] <- "55-65"
artnetLong$p_age.cat_imp[artnetLong$p_age_imp > 65] <- "66+"


# PrEP Use

# PREP_CURRENT PREPCURRENT_AMIS artnetPREP_CURRENT PARTXPREPSTART PARTXPREPUSE PARTXPREPUSE_PART

# Variables for each partner
p1.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART1ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART1PREPUSE, 
                  prep.during.ego.start = PART1PREPSTART, 
                  prep.during.part = PART1PREPUSE_PART) %>%
        mutate(PARTNER_ID = 1)
        
p2.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART2ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART2PREPUSE, 
                  prep.during.ego.start = PART2PREPSTART, 
                  prep.during.part = PART2PREPUSE_PART) %>%
        mutate(PARTNER_ID = 2)

p3.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART3ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART3PREPUSE, 
                  prep.during.ego.start = PART3PREPSTART, 
                  prep.during.part = PART3PREPUSE_PART) %>%
        mutate(PARTNER_ID = 3)

p4.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART4ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART4PREPUSE, 
                  prep.during.ego.start = PART4PREPSTART, 
                  prep.during.part = PART4PREPUSE_PART) %>%
        mutate(PARTNER_ID = 4)

p5.vars <- select(artnet, AMIS_ID, 
                  ONCE = PART5ONCE,
                  prep.ever.ego = PREP_REVISED,
                  prep.during.ego = PART5PREPUSE, 
                  prep.during.ego.start = PART5PREPSTART, 
                  prep.during.part = PART5PREPUSE_PART) %>%
        mutate(PARTNER_ID = 5)

# Combining all of the datasets
pall.vars <- rbind(p1.vars, p2.vars, p3.vars, p4.vars, p5.vars)

# Dropping observations where ONCE is NA
pall.vars <- pall.vars[which(!is.na(pall.vars$ONCE)),]

# Dropping ONCE from the DF (already in artnetLong)
pall.vars$ONCE <- NULL

# Adding the new partner variables to artnetLong
artnetLong <- inner_join(artnetLong, pall.vars, by = c("AMIS_ID", "PARTNER_ID"))

# Recoding Never PrEP as Never During Partnership (was NA)
artnetLong$prep.during.ego[artnetLong$prep.ever.ego == 0] <- 3

# Recoding 88 & 99 as NA
artnetLong$prep.during.ego[which(artnetLong$prep.during.ego %in% c(88, 99))] <- NA
artnetLong$prep.during.part[artnetLong$prep.during.part == 99] <- NA
artnetLong$prep.during.ego.start[which(artnetLong$prep.during.ego.start %in% c(88, 99))] <- NA


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
                                     artnetLong$prep.during.ego == 3 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part == 3] <- "NOP-NOP"
        
        # Partner == (-) PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego == 3 &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part %in% c(1, 2)] <- "NOP-YEP"
        
        # Partner == (-) PrEP UNK
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego == 3 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part == 88] <- "NOP-PUK"
        
        # Partner == (+)
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego == 3 & 
                                     artnetLong$p_hiv == 1] <- "NOP-POS"
        
        # Partner == (?)
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego == 3 & 
                                     artnetLong$p_hiv == 2] <- "NOP-UNK"

## Ego == (-) PrEP
        # Partner == (-) no PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego %in% c(1, 2) & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part == 3] <- "YEP-NOP"
        
        # Partner == (-) PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego %in% c(1, 2) &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part %in% c(1, 2)] <- "YEP-YEP"
        
        # Partner == (-) PrEP UNK
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego %in% c(1, 2) & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part == 88] <- "YEP-PUK"
        
        # Partner == (+)
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego %in% c(1, 2) & 
                                     artnetLong$p_hiv == 1] <- "YEP-POS"
        
        # Partner == (?)
        artnetLong$d_hivprep[artnetLong$hiv3 == 0 & 
                                     artnetLong$prep.during.ego %in% c(1, 2) & 
                                     artnetLong$p_hiv == 2] <- "YEP-UNK"

## Ego == (+)
        # Partner == (-) no PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 1 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part == 3] <- "POS-NOP"
        
        # Partner == (-) PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 1 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part %in% c(1, 2)] <- "POS-YEP"
        
        # Partner == (-) PrEP UNK
        artnetLong$d_hivprep[artnetLong$hiv3 == 1 & 
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part == 88] <- "POS-PUK"
        
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
                                     artnetLong$prep.during.part == 3] <- "UNK-NOP"
        
        # Partner == (-) PrEP
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part %in% c(1, 2)] <- "UNK-YEP"
        
        # Partner == (-) PrEP UNK
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 0 & 
                                     artnetLong$prep.during.part == 88] <- "UNK-PUK"
        
        # Partner == (+)
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 1] <- "UNK-POS"
        
        # Partner == (?)
        artnetLong$d_hivprep[artnetLong$hiv3 == 2 &
                                     artnetLong$p_hiv == 2] <- "UNK-UNK"