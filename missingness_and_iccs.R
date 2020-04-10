library(MissMech)
df <- read.table("Mplus/03-07-18_paper7.dat", stringsAsFactors = FALSE, na.strings =  ".")
MissMech::TestMCARNormality(df[, -1])

names(df) <- c("family", "sex", "time", "Man", "Fan", "Mco", "Fco", "Min", "Fin", "Mre", "Fre", "Mwa", "Fwa", "emo")

tmp <- df[,c(1,3:7, 10:14)]
tmp <- reshape(tmp, direction = "wide", v.names = names(tmp)[-c(1,2)], timevar = "time", idvar = "family")

iccs <- sapply(gsub("\\.1", "", grep("\\.1", names(tmp), value = T)), function(i){
  psych::ICC(tmp[, grep(i, names(tmp))])$results$ICC[2]
})

write.table(rep(iccs, each = 4), "clipboard", sep  ="\t")
