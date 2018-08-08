#!/usr/bin/env Rscript

library(runjags)
library(coda)
library(mcmcplots)

aineisto <- readRDS('/home/juho/drive/work/tutkimus/data/introtopics/modeldata/indicators.rds')

for(varname in c('lang','group','corpustype','subjtype','is_presentational')){
    aineisto[[varname]] <- as.factor(aineisto[[varname]])
}

observations <- xtabs(~ lang + group + corpustype + subjtype + is_presentational + location3, data=aineisto)
totals <- xtabs(~ lang + group + corpustype + subjtype + is_presentational, data=aineisto)

dataList <- list(observations=observations, totals=totals, Nlang = length(unique(aineisto$lang)), Ngroup = length(unique(aineisto$group)), Ncorpustype = length(unique(aineisto$corpustype)), Nsubjtype = length(unique(aineisto$subjtype)), Nis_presentational = length(unique(aineisto$is_presentational)), Nlocation3 = length(unique(aineisto$location3)))

monitor <- c('lang', 'group', 'corpustype', 'subjtype', 'is_presentational', 'std.lang', 'std.group', 'std.corpustype', 'std.subjtype', 'std.is_presentational', 'lang.group', 'lang.corpustype', 'lang.subjtype', 'lang.is_presentational', 'std.lang.group', 'std.lang.corpustype', 'std.lang.subjtype', 'std.lang.is_presentational')

RunJagsModel <- run.jags(data=dataList, monitor=monitor, model='/home/juho/drive/work/tutkimus/data/introtopics/model_specifications/indicators/model.bugs', adapt=10000.0, n.chains=2.0, burnin=5000.0, thin=1.0, sample=20000.0, method='parallel')

post <- as.mcmc.list(RunJagsModel)
saveRDS(post,"/home/juho/drive/work/tutkimus/data/introtopics/modeldata/indicators_post.rds")
mcmcplot(post,dir="/home/juho/projects/work/introtopics/output/mcmc_diagnostics/indicators")
summary(post)