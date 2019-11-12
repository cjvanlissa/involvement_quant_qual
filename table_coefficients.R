library(MplusAutomation)
library(motley)
source("rfunctions.R")
if(!file.exists("results.RData")){
  results <- readModels("./Mplus", filefilter = "revision")
  save(results, file = "results.RData")
} else {
  load("results.RData")
}

order_rows <- c("Between-family level correlations",
                "MOTH.WITH.EMO", "FATH.WITH.EMO", "MOTH.WITH.FATH",
                "Within-family level correlations",
                "EMO.WITH.MOTH.Within", "FATH.WITH.EMO.Within", "FATH.WITH.MOTH.Within",
                "Intercepts of parenting effects",
                "Intercepts.SEMOMOTH", "Intercepts.SEMOFATH",
                "Intercepts of child effects",
                "Intercepts.SMOTHEMO", "Intercepts.SFATHEMO",
                "Effects of quantitative involvement on parenting and adjustment",
                "MOTH.ON.MEAN", "FATH.ON.MEAN", "EMO.ON.MEAN", 
                "MOTH.ON.MMORE", "FATH.ON.MMORE", "EMO.ON.MMORE", 
                "Moderating effect of quantitative involvement on parenting effects",
                "SEMOMOTH.ON.MEAN", "SEMOFATH.ON.MEAN",
                "SEMOMOTH.ON.MMORE", "SEMOFATH.ON.MMORE",
                "Moderating effect of quantitative involvement on child effects",
                "SMOTHEMO.ON.MEAN", "SFATHEMO.ON.MEAN",
                "SMOTHEMO.ON.MMORE", "SFATHEMO.ON.MMORE",
                "Sex differences in parenting and adjustment",
                "MOTH.ON.MALE", "FATH.ON.MALE", "EMO.ON.MALE", 
                "Sex differences in cross-lagged effects",
                "SEMOMOTH.ON.MALE", "SEMOFATH.ON.MALE",
                "SMOTHEMO.ON.MALE", "SFATHEMO.ON.MALE",
                "Within-family level residual variances",
                "Residual.Variances.MOTH.Within", "Residual.Variances.FATH.Within", "Residual.Variances.EMO.Within",
                "Between-family level residual variances",
                "Residual.Variances.MOTH", "Residual.Variances.FATH", "Residual.Variances.EMO", 
                "Residual.Variances.SEMOMOTH", "Residual.Variances.SEMOFATH", "Residual.Variances.SMOTHEMO", "Residual.Variances.SFATHEMO", 
                "Mean quantitative involvement",
                "Means.MEAN", "Means.MMORE",
                "(Co)variances of quantitative involvement",
                "Variances.MEAN", "Variances.MMORE",
                "MMORE.WITH.MEAN", 
                "Random slopes of stability coefficients",
                "Means.SMOTHMOTH", "Means.SFATHFATH", "Means.SEMOEMO",
                "Random intercepts",
                "Intercepts.MOTH", "Intercepts.FATH", "Intercepts.EMO", 
                "Random intercept variances",
                "Variances.SMOTHMOTH", "Variances.SFATHFATH", "Variances.SEMOEMO",
                "Defined parameters",
                paste0("New.Additional.Parameters.P", 1:20)
)

label_defined_pars <- c(p1 = "F-M Within-family correlations with emo.",
                         p2 = "F-M Within-family residual variance of parenting",
                         p3 = "F-M Sex differences in parenting effects",
                         p4 = "F-M Sex differences in child effects",
                         p5 = "F-M Moderating effects of joint involvement on parenting effects",
                         p6 = "F-M Moderating effects of relative involvement on parenting effects",
                         p7 = "F-M Moderating effects of joint involvement on child effects",
                         p8 = "F-M Moderating effects of relative involvement on child effects",
                         p9 = "F-M Sex differences in parenting and adjustment",
                         p10 = "F-M Effects of joint quantitative involvement on random intercepts of",
                         p11 = "F-M Effects of relative quantitative involvement on random intercepts of",
                         p12 = "F-M Between-family correlations with emo.",
                         p13 = "F-M Between-family residual variance of parenting",
                         p14 = "F-M Variance of parenting stability",
                         p15 = "F-M Variance of parenting effect",
                         p16 = "F-M Variance of child effect",
                         p17 = "Mother effect - child effect on mother",
                         p18 = "Father effect - child effect on father",
                        p19 = "F-M Parent effects on emo.",
                        p20 = "F-M Child effects on parenting")

tab_fun <- function(x, ...){
  tables <- lapply(results, printResultsTable, keepCols = c("label", "est_sig",
                                                            "confint",
                                                            "BetweenWithin",
                                                            "posterior_sd",
                                                            "lower_2.5ci",
                                                            "upper_2.5ci"), ...)

  tables <- lapply(tables, function(x){
    x$label[grepl("^Within", x$BetweenWithin)] <- paste0(x$label[grepl("^Within", x$BetweenWithin)], ".", "Within")
    x <- x[!(x$posterior_sd < .001 & x$lower_2.5ci == x$upper_2.5ci), ]
    x$est_sig <- gsub("\\*+", "*", x$est_sig)
    x[, c("label", "est_sig", "confint")]
  })
  
  tables <- do.call(cbind, tables)
  tables <- tables[, -grep("label", names(tables))[-1]]
  
  addrows <- order_rows[!order_rows %in% tables[,1]]
  
  for(i in addrows){
    tables[nrow(tables)+1, 1] <- i
  }
  
  names(tables) <- gsub("^.+(warmth|anger|reasoning|consistency).+(label|est_sig|confint)$", "\\1 \\2", names(tables))
  names(tables) <- gsub("reasoning", "reason", names(tables))
  tables <-
    tables[match(order_rows, tables[, 1]), c(
      "anger label",
      "warmth est_sig",
      "warmth confint",
      "consistency est_sig",
      "consistency confint",
      "reason est_sig",
      "reason confint",
      "anger est_sig",
      "anger confint"
    )]
  names(tables)[1] <- "Parameter"
  
  tables$Parameter <- rename_pars(tables$Parameter)
  tables$Parameter[match(paste0("New.Additional.Parameters.", toupper(names(label_defined_pars))), tables$Parameter)] <- label_defined_pars
  tables
}

tab_std <- suppressWarnings(tab_fun(results, parameters = "stdyx.standardized"))
tab_std <- tab_std[!tab_std$Parameter %in% c("Defined parameters", label_defined_pars),]
tab_unst <- suppressMessages(tab_fun(results, parameters = "unstandardized"))

# Replace minus because I accidentally reversed F and M for p19/p20 -------

replace_minus <- 
sapply(tab_unst[tab_unst$Parameter %in% c("F-M Parent effects on emo.",
                                          "F-M Child effects on parenting"), ], gsub, pattern = "(^|\\s)0", replacement = "X0", perl = TRUE)
replace_minus <- gsub(pattern = "-0", replacement = " 0", replace_minus)
replace_minus <- gsub(pattern = "X0", replacement = "-0", replace_minus)
tab_unst[tab_unst$Parameter %in% c("F-M Parent effects on emo.",
                                   "F-M Child effects on parenting"), ] <- replace_minus

write.csv(tab_std, "tab_std.csv", row.names = FALSE, na = "")
write.csv(tab_unst, "tab_unst.csv", row.names = FALSE, na = "")


# Split STD ---------------------------------------------------------------
sections <- c(which(rowSums(is.na(tab_std)) == 8), (nrow(tab_std)+1))
tab_std_sections <- mapply(function(starts, ends) {
  tab_std[starts:(ends-1),]
},
starts = sections[-length(sections)],
ends = sections[-1], SIMPLIFY = FALSE)
names(tab_std_sections) <- tab_std$Parameter[sections][-length(sections)]


# Split UNST --------------------------------------------------------------

sections <- c(which(rowSums(is.na(tab_unst)) == 8), (nrow(tab_unst)+1))
tab_unst_sections <- mapply(function(starts, ends) {
  tab_unst[starts:(ends-1),]
},
starts = sections[-length(sections)],
ends = sections[-1], SIMPLIFY = FALSE)
names(tab_unst_sections) <- tab_unst$Parameter[sections][-length(sections)]

save(tab_std, tab_unst, tab_std_sections, tab_unst_sections, file = "table_coefficients.RData")
