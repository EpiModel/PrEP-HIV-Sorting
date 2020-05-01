source('~/GitHub/PrEP-HIV-Sorting/Data cleaning.R')

### Prior to recoding 88 & 99 data as NA: ###

## Egos

# artnetLong %>% filter(hiv3 == 0) %>% count(prep.during.ego)
# 1 [Always: I took it the entire time I~  2424
# 2 [Sometimes: I took it for part of th~   511
# 3 [Never: I never took PrEP while I wa~  8470
# 88 [I dont know]                            38
# 99 [I prefer not to answer]                  9
# NA                                        1346
# 1336 NA d/t prep.ever.ego as NA
# Need to figure out other 57

## Partners

#artnetLong %>% filter(p_hiv == 0) %>% count(prep.during.part)
# 1 [Always: He took it the entire time ~  1474
# 2 [Sometimes: He took it for part of t~   346
# 3 [Never: He never took PrEP while I w~  4019
# 88 [I dont know]                          4505
# 99 [I prefer not to answer]                 13

#### Table 4a - PrEP mixing with unknown values ####

a <- artnetLong %>% count(hiv3)  #HIV among egos
b <- artnetLong %>% count(d_hiv) #HIV by dyad pair
c <- artnetLong %>% count(p_hiv) #HIV among partners

