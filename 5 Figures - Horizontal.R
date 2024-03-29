# Figures showing mixing patterns

library("tidyverse")
library("ggpubr")
library("viridis")
library("gridExtra")

source('3 Analysis.R')
rm(artnet, artnetLong)

mycol4 <- viridis(4)

mycol.ps.full <- c(mycol4[4], mycol4[1:3])
mycol.ps.cc <- c(mycol4[1:3])
mycol.ps.re <- mycol.ps.cc

### Serosorting ----
fig1.panels <- function(scen){

        if (scen == "full") {
                ss.table <- prop.table(table(artnetSort$hiv3, artnetSort$p_hiv, useNA = "ifany"), 1)
                ego <- factor(rep(c("Diagnosed HIV\n", "Test-negative", "HIV unknown"), each = 4),
                              levels = c("Test-negative", "Diagnosed HIV\n", "HIV unknown"))
                part <- rep(c("Diagnosed HIV", "Test-negative", "HIV unknown", "Without diagnosed HIV"), times = 3)
                perc <- c(ss.table[2,2], ss.table[2,1], ss.table[2,3], 0,
                          ss.table[1,2], ss.table[1,1], ss.table[1,3], 0,
                          ss.table[3,2], ss.table[3,1], ss.table[3,3], 0)
                panel.title <- "Ego Knowledge\n"}
        
        
        if (scen == "cc") {
                cc.df <- artnetSort %>% filter(!p_hiv == "Unk", !hiv3 == "Unk")
                ss.table <- prop.table(table(cc.df$hiv3, cc.df$p_hiv, useNA = "ifany"), 1)
                ego <- factor(rep(c("Diagnosed HIV\n", "Test-negative"), each = 4),
                              levels = c("Test-negative", "Diagnosed HIV\n"))
                part <- rep(c("Diagnosed HIV", "Test-negative", "HIV unknown", "Without diagnosed HIV"), times = 2)
                perc <- c(ss.table[2,2], ss.table[2,1], 0, 0,
                          ss.table[1,2], ss.table[1,1], 0, 0)
                panel.title <- "\nComplete-Case\n"}
        
        if (scen == "reclass") {
                ss.table <- as.data.frame(results(dat = reclass.results, x = "hh.sort.p"))
                ego <- factor(rep(c("Diagnosed HIV", "Without\ndiagnosed HIV"), each = 4),
                              levels = c("Without\ndiagnosed HIV", "Diagnosed HIV"))
                part <- rep(c("Diagnosed HIV", "Test-negative", "HIV unknown", "Without diagnosed HIV"), times = 2)
                perc <- c(ss.table[4,2], 0, 0, ss.table[2,2], 
                          ss.table[3,2], 0, 0, ss.table[1,2])
                panel.title <- "\nReclassification\n"}
                
        ss.df <- data.frame(ego, part, perc)
        bar.order <- c("HIV unknown", "Test-negative", "Without diagnosed HIV", "Diagnosed HIV")
                
        fig1.panel <- ggplot(ss.df, 
                        aes(x = ego, y = perc, 
                        fill = factor(part, levels = bar.order))) +
                geom_bar(stat = "identity") +
                coord_flip() +
                scale_y_continuous(labels = scales::percent,
                                   expand = c(0.02,0)) +
                scale_x_discrete(expand = c(0,0)) +
                geom_label(data = subset(ss.df, perc > 0),
                           aes(label = scales::percent(perc, accuracy = 0.1)), 
                           position = position_stack(vjust = 0.5),
                           show.legend = FALSE, fill = "white") +
                labs(fill = "Alter HIV status",
                     title = panel.title,
                     x = "") +
                theme(plot.title = element_text(hjust = 0.5, size = 14),
                      axis.title = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_text(size = 12,
                                               face = "bold",
                                               color = "black"),
                      axis.text.y = element_blank(),
                      text = element_text(size = 12,
                                          face = "bold",
                                          color = "black"),
                      strip.text = element_text(size = 12,
                                                face = "bold",
                                                color = "black"),
                      legend.position = "none",
                      legend.text = element_text(size = 12,
                                                 face = "bold",
                                                 color = "black"),
                      strip.background = element_blank(),
                      panel.background = element_blank()) +
                scale_fill_manual(values = c("grey28", mycol4[2], mycol4[1], mycol4[3]))
        
        if (scen == "full") {ss.full <<- fig1.panel}
        if (scen == "cc") {ss.cc <<- fig1.panel}
        if (scen == "reclass") {ss.reclass <<- fig1.panel}
        
}

fig1.panels(scen = "full")
fig1.panels(scen = "cc")
fig1.panels(scen = "reclass")
ss.compare <- ggarrange(ss.cc, ss.reclass,
                        ncol = 1)

pdf("DefenseFig-SS1.pdf", height = 4, width = 8)
ss.full
dev.off()

## Combined plots
pdf("DefenseFig-SS2.pdf", height = 5, width = 8)
ss.compare
dev.off()


## PrEP Sorting ----

fig2.panels <- function(scen){
        
        if (scen == "full") {
                ps.table <- prop.table(table(artnetSort$hp, artnetSort$p_hp),1)
                ego <- factor(rep(c("Diagnosed\nHIV", "Never PrEP", "PrEP", "HIV\nunknown"), each = 4),
                              levels = c("Never PrEP", "PrEP", "Diagnosed\nHIV", "HIV\nunknown"))
                part <- rep(c("Diagnosed HIV", "Never PrEP", "Ever PrEP", "Unknown PrEP"), times = 4)
                perc <- c(ps.table[2,2], ps.table[2,1], ps.table[2,3], ps.table[2,4],
                          ps.table[1,2], ps.table[1,1], ps.table[1,3], ps.table[1,4],
                          ps.table[3,2], ps.table[3,1], ps.table[3,3], ps.table[3,4],
                          ps.table[4,2], ps.table[4,1], ps.table[4,3], ps.table[4,4])
                panel.title <- "Ego Knowledge\n"}
        
        
        if (scen == "cc") {
                cc.df <- artnetSort %>% filter(!p_hiv == "Unk" & !prep.during.part2 == "Unk" & !hiv3 == "Unk")
                ps.table <- prop.table(table(cc.df$hp, cc.df$p_hp),1)
                ego <- factor(rep(c("Diagnosed\nHIV", "Never PrEP", "PrEP"), each = 4),
                              levels = c("Never PrEP", "PrEP", "Diagnosed\nHIV"))
                part <- rep(c("Diagnosed HIV", "Never PrEP", "Ever PrEP", "Unknown PrEP"), times = 3)
                perc <- c(ps.table[2,2], ps.table[2,1], ps.table[2,3], 0,
                          ps.table[1,2], ps.table[1,1], ps.table[1,3], 0,
                          ps.table[3,2], ps.table[3,1], ps.table[3,3], 0)
                panel.title <- "\nComplete-Case\n"}
        
        if (scen == "reclass") {
                ps.table <- as.data.frame(results(dat = reclass.results, x = "full.sort.p"))
                ego <- factor(rep(c("Diagnosed\nHIV", "Never PrEP", "PrEP"), each = 4),
                              levels = c("Never PrEP", "PrEP", "Diagnosed\nHIV"))
                part <- rep(c("Diagnosed HIV", "Never PrEP", "Ever PrEP", "Unknown PrEP"), times = 3)
                perc <- c(ps.table[5,2], ps.table[2,2], ps.table[8,2], 0,
                          ps.table[4,2], ps.table[1,2], ps.table[7,2], 0,
                          ps.table[6,2], ps.table[3,2], ps.table[9,2], 0)
                panel.title <- "\nReclassification\n"}
        
        ps.df <- data.frame(ego, part, perc)
        bar.order <- c("Unknown PrEP", "Ever PrEP", "Never PrEP", "Diagnosed HIV")
        
        fig2.panel <- ggplot(ps.df, 
                             aes(x = ego, y = perc, 
                                 fill = factor(part, levels = bar.order))) +
                geom_bar(stat = "identity") +
                coord_flip() +
                scale_y_continuous(labels = scales::percent,
                                   expand = c(0.02,0)) +
                scale_x_discrete(expand = c(0,0)) +
                geom_label(data = subset(ps.df, perc > 0),
                           aes(label = scales::percent(perc, accuracy = 0.1)), 
                           position = position_stack(vjust = 0.5),
                           show.legend = FALSE, fill = "white") +
                labs(fill = "Alter HIV status and PrEP use",
                     title = panel.title,
                     x = "") +
                theme(plot.title = element_text(hjust = 0.5, size = 14),
                      axis.title = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_text(size = 12,
                                               face = "bold",
                                               color = "black"),
                      axis.text.y = element_blank(),
                      text = element_text(size = 12,
                                          face = "bold",
                                          color = "black"),
                      strip.text = element_text(size = 12,
                                                face = "bold",
                                                color = "black"),
                      legend.position = "none",
                      legend.text = element_text(size = 12,
                                                 face = "bold",
                                                 color = "black"),
                      strip.background = element_blank(),
                      panel.background = element_blank()) +
                scale_fill_manual(values = c("grey28", mycol4[2], mycol4[1], mycol4[3]))
        
        if (scen == "full") {ps.full <<- fig2.panel}
        if (scen == "cc") {ps.cc <<- fig2.panel}
        if (scen == "reclass") {ps.reclass <<- fig2.panel}
        
}

fig2.panels(scen = "full")
fig2.panels(scen = "cc")
fig2.panels(scen = "reclass")

## Combined plots
(ps.compare <- ggarrange(ps.cc, ps.reclass,
                         ncol = 1))

pdf("DefenseFig-PS1.pdf", height = 4, width = 8)
ps.full
dev.off()

## Combined plots
pdf("DefenseFig-PS2.pdf", height = 5, width = 8)
ps.compare
dev.off()
