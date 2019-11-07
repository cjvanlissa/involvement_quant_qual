library(MplusAutomation)
library(motley)
source("rfunctions.R")
if(!file.exists("results.RData")){
  results <- readModels("c:/git_repositories/involvement_quant_qual/Mplus")
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
                "Variances.SMOTHMOTH", "Variances.SFATHFATH", "Variances.SEMOEMO"             
                
)

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
  
  names(tables) <- gsub(".+_(\\w+)_.+out\\.(.*)$", "\\1 \\2", names(tables))
  
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
  
  tables
}

tab_std <- suppressWarnings(tab_fun(results, parameters = "stdyx.standardized"))
tab_unst <- suppressMessages(tab_fun(results, parameters = "unstandardized"))

write.csv(tab_std, "tab_std.csv", row.names = FALSE)
write.csv(tab_unst, "tab_unst.csv", row.names = FALSE)


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