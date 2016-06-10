# Analysis routines for paper:
# Multiobjective Evolutionary Algorithms for Operational Planning Problems 
# in Open-Pit Mining

## Clean up workspace and load packages
rm(list=ls())

# load libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(car)

#=========

# Read data and prepare data frame for analysis
filenames <- dir(path    = "../data",
                 pattern = ".xml.dat")
for (i in seq_along(filenames)){
    ## read file
    tmp <- read.table(file             = paste0("../data/", filenames[i]), 
                      header           = TRUE, 
                      sep              = "\t", 
                      stringsAsFactors = FALSE)
    
    ## select relevant columns and rows
    tmp <- tmp %>%
        select(c(2, 15, 5, 6, 13)) %>%
        filter(ALGORITMO %in% c("NSGA", "SPEA", "MILS"))
    
    names(tmp) <- c("Algorithm", "Instance", "IGD", "Spread", "Runtime.ms")
    
    if (i == 1) mydata <- tmp
    else mydata <- rbind(mydata, tmp)
}

## number of repeated runs in each case
n <- replications(IGD~Algorithm*Instance, data = tmp)[3]

## Aggregate data (to avoid pseudoreplication in inference)
mydata.agg <- mydata %>% 
    mutate(Algorithm = factor(mydata$Algorithm, 
                              labels = c("MILS", "NSGA-II", "SPEA2")),
           Instance = factor(mydata$Instance, 
                             labels = paste0("Mine ", 1:4))) %>%
    group_by(Algorithm, Instance) %>%
    summarise(IGD.mean       = mean(IGD), 
              IGD.se         = sd(IGD) / sqrt(n),
              Spread.mean    = mean(Spread), 
              Spread.se      = sd(Spread) / sqrt(n), 
              Runtime.s.mean = mean(Runtime.ms / 1000),
              Runtime.s.se   = sd(Runtime.ms / 1000) / sqrt(n))

## Recast dataframe for using with ggplot2
mydata.agg.ggplot <- with(mydata.agg, 
                          data.frame(Algorithm = rep(Algorithm, each = 3),
                                     Instance  = rep(Instance, each = 3),
                                     Indicator = rep(c("IGD",
                                                       "Spread",
                                                       "Runtime (s)"),
                                                     times = nrow(mydata.agg)),
                                     Estimate  = melt(t(as.matrix(mydata.agg[, c(3, 5, 7)])))[, 3],
                                     Se        = melt(t(as.matrix(mydata.agg[, c(4, 6, 8)])))[, 3]))

# Plot results
p <- ggplot(data    = mydata.agg.ggplot, 
            mapping = aes(x      = Instance,
                          y      = Estimate, 
                          ymax   = Estimate + Se * qt(0.975, df = 32),
                          ymin   = Estimate - Se * qt(0.975, df = 32),
                          group  = Algorithm,
                          colour = Algorithm,
                          shape  = Algorithm))

p <- p + 
    geom_pointrange(size = 1) + 
    geom_line(alpha = 0.5, size = 1) + 
    facet_grid(Indicator~., scales = "free_y") + 
    theme(axis.text         = element_text(size = 12),
          axis.title        = element_text(size = 14),
          strip.text        = element_text(size = 12),
          legend.title      = element_text(size = 14),
          legend.text       = element_text(size = 12),
          legend.key.size   = unit(.75, units = "mm"),
          legend.key.width  = unit(5, units = "mm"),
          legend.position   = "bottom")

ggsave("../figures/results.png", 
       plot   = p, 
       width  = 20, 
       height = 12, 
       units  = "cm", 
       dpi    = 600)


# Perform inference
test.results        <- vector(mode = "list", length = 3)
names(test.results) <- c("IGD", "Spread", "Runtime (s)")
algos               <- levels(mydata.agg$Algorithm)

par(mfrow = c(3, 3))
for (i in seq_along(algos)){
    tmp <- mydata.agg %>%
        ungroup() %>%
        filter(Algorithm != algos[i]) %>%
        mutate(Algorithm = factor(Algorithm))
    
    comp.algs <- levels(tmp$Algorithm)
    
    test.results$IGD[[i]]     <- with(tmp, 
                                      t.test(IGD.mean ~ Algorithm, 
                                             paired = TRUE,
                                             conf.level = 1 - 0.05/3))
    test.results$Spread[[i]]  <- with(tmp, 
                                      t.test(Spread.mean ~ Algorithm, 
                                             paired = TRUE,
                                             conf.level = 1 - 0.05/3))
    test.results$Runtime[[i]] <- with(tmp, 
                                      t.test(Runtime.s.mean ~ Algorithm, 
                                             paired = TRUE,
                                             conf.level = 1 - 0.05/3))
    
    names(test.results$IGD)[i] <- 
        names(test.results$Spread)[i] <- 
        names(test.results$Runtime)[i] <- 
        paste(comp.algs[1], "vs.",  comp.algs[2])
    
    
    # Residual analysis
    IGD.res <- tmp$IGD.mean[1:4] - tmp$IGD.mean[5:8]
    Spread.res <- tmp$Spread.mean[1:4] - tmp$Spread.mean[5:8]
    Runtime.res <- tmp$Runtime.s.mean[1:4] - tmp$Runtime.s.mean[5:8]
    
    
    car::qqPlot(IGD.res, pch = 16, cex = 2)
    title(main = names(test.results$IGD)[i])
    car::qqPlot(Spread.res, pch = 16, cex = 2)
    title(main = names(test.results$IGD)[i])
    car::qqPlot(Runtime.res, pch = 16, cex = 2)
    title(main = names(test.results$IGD)[i])
}

# test.results$IGD
# test.results$Spread
# test.results$Runtime

