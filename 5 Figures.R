# Figures showing mixing patterns
# By Kevin Maloney (kevin.maloney@emory.edu)
# 2021-02-25

library("tidyverse")
library("ggpubr")
library("viridis")
library("gridExtra")

source('~/GitHub/PrEP-HIV-Sorting/3 Analysis.R')
rm(artnet, artnetLong)

mycol4 <- viridis(4)
mycol.ss.full <- c(mycol4[4], mycol4[2:3])
mycol.ss.cc <- c(mycol4[2:3])
mycol.ss.re <- c(mycol4[1], mycol4[3])
mycol.ss.comp <- c(mycol4[1], mycol4[2:3])

mycol.ps.full <- c(mycol4[4], mycol4[1:3])
mycol.ps.cc <- c(mycol4[1:3])
mycol.ps.re <- mycol.ps.cc

### Serosorting ----

## Full sample
ss.table <- prop.table(table(artnetSort$hiv3, artnetSort$p_hiv, useNA = "ifany"), 1)
ego <- factor(rep(c("Diagnosed HIV\n(11.2%)", "Test-negative\n(76.4%)", "HIV unknown\n(12.3%)"), each = 3),
                 levels = c("Test-negative\n(76.4%)", "Diagnosed HIV\n(11.2%)", "HIV unknown\n(12.3%)"))
part <- rep(c("Diagnosed HIV", "Test-negative", "HIV unknown"), times = 3)
perc <- c(ss.table[2,2], ss.table[2,1], ss.table[2,3],
             ss.table[1,2], ss.table[1,1], ss.table[1,3],
             ss.table[3,2], ss.table[3,1], ss.table[3,3])
ss.full.df <- data.frame(ego, part, perc)
rm(ego, part, perc, ss.table)

(ss.full <- ggplot(ss.full.df, aes(x = ego, y = perc, 
                fill = factor(part, levels = c("HIV unknown", "Test-negative", "Diagnosed HIV")))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = scales::percent)) +
        geom_label(aes(label = scales::percent(perc, accuracy = 0.1)), 
                  position = position_stack(vjust = 0.5),
                  show.legend = FALSE, fill = "white") +
        labs(fill = "HIV among partners",
             title = "Serosorting based on respondent knowledge",
             x = "Respondents") +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.y = element_blank(),
              text = element_text(size = 12),
              strip.text = element_text(size = 12),
              legend.position = "right") +
        scale_fill_manual(values = mycol.ss.full)

## Complete-case analysis
cc.df <- artnetSort %>% filter(!p_hiv == "Unk", !hiv3 == "Unk")
ss.table <- prop.table(table(cc.df$hiv3, cc.df$p_hiv, useNA = "ifany"), 1)
ego <- factor(rep(c("Diagnosed HIV\n(11.5%)", "Test-negative\n(88.5%)"), each = 3),
              levels = c("Test-negative\n(88.5%)", "Diagnosed HIV\n(11.5%)"))
part <- rep(c("Diagnosed HIV", "Test-negative or HIV unknown", "Test-negative"), times = 2)
perc <- c(ss.table[2,2], 0, ss.table[2,1],
          ss.table[1,2], 0, ss.table[1,1])
ss.cc.df <- data.frame(ego, part, perc)
rm(ego, part, perc, ss.table, cc.df)

(ss.cc <- ggplot(ss.cc.df, aes(x = ego, y = perc, 
                                   fill = factor(part, levels = c("Test-negative or HIV unknown", "Test-negative", "Diagnosed HIV")))) +
                geom_bar(stat = "identity") +
                scale_y_continuous(labels = scales::percent)) +
        geom_label(data = subset(ss.cc.df, perc > 0),
                   aes(label = scales::percent(perc, accuracy = 0.1)), 
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE, fill = "white") +
        labs(fill = "HIV among partners",
             title = "Complete-case analysis",
             x = "Respondents") +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              text = element_text(size = 12),
              axis.title.y = element_blank(),
              strip.text = element_text(size = 12),
              legend.position = "right") +
        scale_fill_manual(values = mycol.ss.comp)

## Reclassification Analysis
ss.table <- as.data.frame(results(dat = reclass.results, x = "hh.sort.p"))
ego <- factor(rep(c("Diagnosed HIV\n(11.2%)", "Test-negative or HIV unknown\n(88.8%)"), each = 3),
              levels = c("Test-negative or HIV unknown\n(88.8%)", "Diagnosed HIV\n(11.2%)"))
part <- rep(c("Diagnosed HIV", "Test-negative or HIV unknown", "Test-negative"), times = 2)
perc <- c(ss.table[4,2], ss.table[2,2], 0, ss.table[3,2], ss.table[1,2], 0)
ss.reclass.df <- data.frame(ego, part, perc)
rm(ego, part, perc, ss.table)

(ss.reclass <- ggplot(ss.reclass.df, 
                      aes(x = ego, y = perc, 
                          fill = factor(part, levels = c("Test-negative or HIV unknown", "Test-negative", "Diagnosed HIV")))) +
                geom_bar(stat = "identity") +
                scale_y_continuous(labels = scales::percent)) +
                geom_label(data = subset(ss.reclass.df, perc > 0),
                           aes(label = scales::percent(perc, accuracy = 0.1)), 
                           position = position_stack(vjust = 0.5),
                           show.legend = FALSE, fill = "white") +
                labs(fill = "HIV among partners",
                        title = "Reclassification analysis",
                        x = "Respondents") +
                theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        text = element_text(size = 12),
                       axis.title.y = element_blank(),
                       strip.text = element_text(size = 12),
                        legend.position = "right") +
                scale_fill_manual(values = mycol.ss.comp)

## Combined cc and reclass
(ss.compare <- ggarrange(ss.cc, ss.reclass,
                     ncol = 2, nrow = 1,
                     common.legend = TRUE,
                     legend = "bottom"))


ss.cc.df$scen <- "Complete-case analysis"
ss.reclass.df$scen <- "Reclassification analysis"

ss.compare.df <- rbind(ss.cc.df, ss.reclass.df)

(ss.compare <- ggplot(ss.compare.df, aes(x = ego, y = perc, 
                                         fill = factor(part, levels = c("Test-negative or HIV unknown", "Test-negative", "Diagnosed HIV")))) +
                geom_bar(stat = "identity") +
                scale_y_continuous(labels = scales::percent)) +
        facet_wrap(~ factor(scen, levels = c("Complete-case analysis", "Reclassification analysis"))) +
        geom_label(aes(label = scales::percent(perc, accuracy = 0.1)), 
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE, fill = "white") +
        labs(fill = "HIV among partners",
             title = "Serosorting based on alternative classification mechanisms",
             x = "Respondents") +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.y = element_blank(),
              text = element_text(size = 12),
              strip.text = element_text(size = 12),
              legend.position = "bottom") +
        scale_fill_manual(values = mycol.ss.comp)


## PrEP Sorting ----

## Full Sample
ps.table <- prop.table(table(artnetSort$hp, artnetSort$p_hp),1)
ego <- factor(rep(c("Diagnosed HIV\n(11.2%)", "No PrEP\n(54.5%)", "PrEP\n(21.9%)", "HIV unknown\n(12.3%)"), each = 4),
              levels = c("No PrEP\n(54.5%)", "PrEP\n(21.9%)", "Diagnosed HIV\n(11.2%)", "HIV unknown\n(12.3%)"))
part <- rep(c("Diagnosed HIV", "No PrEP", "PrEP", "PrEP Unknown"), times = 4)
perc <- c(ps.table[2,2], ps.table[2,1], ps.table[2,3], ps.table[2,4],
          ps.table[1,2], ps.table[1,1], ps.table[1,3], ps.table[1,4],
          ps.table[3,2], ps.table[3,1], ps.table[3,3], ps.table[3,4],
          ps.table[4,2], ps.table[4,1], ps.table[4,3], ps.table[4,4])
ps.full.df <- data.frame(ego, part, perc)
rm(ego, part, perc, ps.table)

(ps.full <- ggplot(ps.full.df, aes(x = ego, y = perc, 
                fill = factor(part, levels = c("PrEP Unknown", "PrEP", "No PrEP", "Diagnosed HIV")))) +
                geom_bar(stat = "identity") +
                scale_y_continuous(labels = scales::percent)) +
        geom_label(aes(label = scales::percent(perc, accuracy = 0.1)), 
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE, fill = "white") +
        labs(fill = "HIV & PrEP among partners",
             title = "HIV & PrEP sorting based on respondent knowledge",
             x = "Respondents") +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.y = element_blank(),
              text = element_text(size = 12),
              strip.text = element_text(size = 12),
              legend.position = "right") +
        scale_fill_manual(values = mycol.ps.full)

## Complete-case analysis
cc.df <- artnetSort %>% filter(!p_hiv == "Unk" & !prep.during.part2 == "Unk" & !hiv3 == "Unk")
ps.table <- prop.table(table(cc.df$hp, cc.df$p_hp),1)
ego <- factor(rep(c("Diagnosed HIV\n(14.0%)", "No PrEP\n(60.2%)", "PrEP\n(25.8%)"), each = 3),
              levels = c("No PrEP\n(60.2%)", "PrEP\n(25.8%)", "Diagnosed HIV\n(14.0%)"))
part <- rep(c("Diagnosed HIV", "No PrEP", "PrEP"), times = 3)
perc <- c(ps.table[2,2], ps.table[2,1], ps.table[2,3],
          ps.table[1,2], ps.table[1,1], ps.table[1,3],
          ps.table[3,2], ps.table[3,1], ps.table[3,3])

ps.cc.df <- data.frame(ego, part, perc)
rm(ego, part, perc, ps.table)

(ps.cc <- ggplot(ps.cc.df, aes(x = ego, y = perc, 
                                   fill = factor(part, levels = c("PrEP", "No PrEP", "Diagnosed HIV")))) +
                geom_bar(stat = "identity") +
                scale_y_continuous(labels = scales::percent)) +
        geom_label(aes(label = scales::percent(perc, accuracy = 0.1)), 
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE, fill = "white") +
        labs(fill = "HIV & PrEP among partners",
             title = "Complete-case analysis",
             x = "Respondents") +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.y = element_blank(),
              text = element_text(size = 12),
              strip.text = element_text(size = 12),
              legend.position = "right") +
        scale_fill_manual(values = mycol.ps.cc)

## Reclassification analysis
ps.table <- as.data.frame(results(dat = reclass.results, x = "full.sort.p"))
ego <- factor(rep(c("Diagnosed HIV\n(11.2%)", "No PrEP\n(66.9%)", "PrEP\n(21.9%)"), each = 3),
              levels = c("No PrEP\n(66.9%)", "PrEP\n(21.9%)", "Diagnosed HIV\n(11.2%)"))
part <- rep(c("Diagnosed HIV", "No PrEP", "PrEP"), times = 3)
perc <- c(ps.table[5,2], ps.table[2,2], ps.table[8,2],
          ps.table[4,2], ps.table[1,2], ps.table[7,2],
          ps.table[6,2], ps.table[3,2], ps.table[9,2])
ps.reclass.df <- data.frame(ego, part, perc)
rm(ego, part, perc, ps.table)

(ps.reclass <- ggplot(ps.reclass.df, aes(x = ego, y = perc, 
                               fill = factor(part, levels = c("PrEP", "No PrEP", "Diagnosed HIV")))) +
                geom_bar(stat = "identity") +
                scale_y_continuous(labels = scales::percent)) +
        geom_label(aes(label = scales::percent(perc, accuracy = 0.1)), 
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE, fill = "white") +
        labs(fill = "HIV & PrEP among partners",
             title = "Reclassification analysis",
             x = "Respondents") +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.y = element_blank(),
              text = element_text(size = 12),
              strip.text = element_text(size = 12),
              legend.position = "right") +
        scale_fill_manual(values = mycol.ps.re)

