a <- artnetSort %>% filter(
        p_hiv %in% c("Neg", "Pos") &
                hiv3 %in% c("Neg", "Pos") &
                prep.during.part2 %in% c("No", "Yes") &
                prep.during.ego2 %in% c("No", "Yes"))

table(a$p_hp, useNA = "ifany")
prop.table(table(a$p_hp, useNA = "ifany"))
