library(worcs)
library(digest)
library(tidyr)
library(MplusAutomation)
library(tidySEM)
tmp <- readModels("Mplus/dsem anger revision.out", what = "input")
df <- read.table("Mplus/03-07-18_paper7.dat", na.strings = ".")
names(df) <- mplus_expand_names(tmp$input$variable$names)

lon <- pivot_longer(df, cols = grep("^([MF](an|co|re|wa)|emo)", names(df), value = TRUE), names_to = "var", values_to = "value")

wid <- pivot_wider(lon, names_from = c("var", "time"), values_from = value)
wid[c("family", "sex")] <- lapply(wid[c("family", "sex")], factor)
set.seed(4618)
syn <- synthetic(wid[,-1], missingness_expression = missRanger::missRanger(data = data))
syn <- cbind(family = 1:nrow(syn), syn)

synthetic <- pivot_longer(syn, 
                          #names_to = "time",
                          cols = grep("^([MF](an|co|re|wa)|emo)", names(syn), value = TRUE),
                          names_pattern = "(.{3})_(.)",
                          names_to = c(".value", "time"))
                          
synthetic$time <- as.integer(synthetic$time)
synthetic <- synthetic[, names(df)]

prepareMplusData(synthetic, "Mplus/synthetic_03-07-18_paper7.dat")