rm(list = ls())

# List of all data sets
dfs <- vector("list", 100)
for (i in seq_along(1:100)) {
        dfs[[i]] <- readRDS(paste("imp", i, ".rds", sep = ""))
}

sorting <- vector("list", 100)

for (i in seq_along(1:100)) {
        # HIV prevalence
        sorting[[i]]$hiv.imp.n <- table(dfs[[i]]$p_hiv.imp)
        sorting[[i]]$hiv.imp.p <- prop.table(table(dfs[[i]]$p_hiv.imp))

        # HIV reclassification
        sorting[[i]]$hiv.reclass.n <- table(dfs[[i]]$p_hiv, dfs[[i]]$p_hiv.imp)
        sorting[[i]]$hiv.reclass.p <- prop.table(table(dfs[[i]]$p_hiv, dfs[[i]]$p_hiv.imp), 1)

        # HIV-HIV sorting
        sorting[[i]]$hh.sort.n <- table(dfs[[i]]$hiv2, dfs[[i]]$p_hiv.imp)
        sorting[[i]]$hh.sort.p <- prop.table(table(dfs[[i]]$hiv2, dfs[[i]]$p_hiv.imp), 1)

        # PrEP prevalence, given neg/unk
        sorting[[i]]$prep.imp.n <- table(dfs[[i]]$prep.imp)
        sorting[[i]]$prep.imp.p <- prop.table(table(dfs[[i]]$prep.imp))
        
        # PrEP reclassification
        sorting[[i]]$prep.reclass.n <- table(dfs[[i]]$prep.during.part2, dfs[[i]]$prep.imp)
        sorting[[i]]$prep.reclass.p <- prop.table(table(dfs[[i]]$prep.during.part2, dfs[[i]]$prep.imp), 1)
        
        # HIV-PrEP sorting
        sorting[[i]]$hp.sort.n <- table(dfs[[i]]$hiv2, dfs[[i]]$prep.imp)
        sorting[[i]]$hp.sort.p <- prop.table(table(dfs[[i]]$hiv2, dfs[[i]]$prep.imp),1)
        
        # PrEP-PrEP sorting
        sorting[[i]]$pp.sort.n <- table(dfs[[i]]$prep.during.ego2[dfs[[i]]$hiv2 == 0], dfs[[i]]$prep.imp[dfs[[i]]$hiv2 == 0])
        sorting[[i]]$pp.sort.p <- prop.table(table(dfs[[i]]$prep.during.ego2[dfs[[i]]$hiv2 == 0], dfs[[i]]$prep.imp[dfs[[i]]$hiv2 == 0]), 1)
}

listVec <- lapply(sorting, c, recursive=TRUE)
sorting.results <- as.data.frame(do.call(rbind, listVec))
as.data.frame(sorting.results)

results <- function(x) {
        q <- select(sorting.results, starts_with(x))
        return(t(apply(q, 2, quantile, probs = c(0, 0.025, 0.5, 0.975, 1), na.rm = FALSE)))
}

# HIV prevalence
results("hiv.imp.n")
results("hiv.imp.p")

# HIV reclassification 
results("hiv.reclass.n")
results("hiv.reclass.p")

# HIV-HIV sorting
results("hh.sort.n")
results("hh.sort.p")

# PrEP prevalence among HIV -/?
results("prep.imp.n")
results("prep.imp.p")

# PrEP reclassification
results("prep.reclass.n")
results("prep.reclass.p")

# HIV-PrEP sorting
results("hp.sort.n")
results("hp.sort.p")

# PrEP-PrEP sorting
results("pp.sort.n")
results("pp.sort.p")
