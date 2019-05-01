rename_pars <- function(x){
  new_labels <- x
  new_labels <- gsub("\\.Within", "", new_labels)
  new_labels <- gsub("S(EMO|MOTH|FATH)(EMO|MOTH|FATH)", "(\\1 on \\2)", new_labels)
  new_labels <- gsub("MEAN", "Joint", new_labels)
  new_labels <- gsub("MMORE", "Relative", new_labels)
  new_labels <- gsub("^(Residual\\.Variances|Residual|Variances|Intercepts|Means)\\.", "", new_labels)
  new_labels <- gsub("FATH", "Father", new_labels)
  new_labels <- gsub("MOTH", "Mother", new_labels)
  new_labels <- gsub("EMO", "Emo.", new_labels)
  new_labels <- gsub("MALE", "sex", new_labels)
  new_labels <- gsub("\\.ON\\.", " on ", new_labels)
  new_labels <- gsub("\\.WITH\\.", " with ", new_labels)
  new_labels
}

order_by_name <- function(x){
  x[sapply(c("warmth", "consistency", "reason", "anger"), grep, names(x))]
}

colspan_fun <- function(x, skip = 0){
  
  num_cols <- ncol(x)-skip
  out <- mapply(
    function(starts, ends) {
      c(starts, ends)
    },
    starts = seq(
      from = 1,
      to = (num_cols - (num_cols / 4) + 1),
      length.out = 4
    ) + skip,
    ends = seq(
      from = (num_cols / 4),
      to = num_cols,
      length.out = 4
    ) + skip
  , SIMPLIFY = FALSE)
  
  
  names(out) <- c("Warmth", "Consistency", "Reason", "Anger")
  out
}

reg_table <- function(x, ...){
  rownames(x) <- NULL
  names(x) <- gsub("^.+est_sig$", "Est.", names(x))
  names(x) <- gsub("^.+confint$", "95\\% HPDI", names(x))
  tab <- capture.output(apa_table(x, format = "latex", 
                                  col_spanners = colspan_fun(x, skip = 1), ...))
  tab <- gsub("^(.+?) & NA & NA & NA & NA & NA & NA & NA & NA\\\\\\\\$", "\\\\multicolumn\\{9\\}\\{l\\}\\{\\1\\}\\\\\\\\", tab)
  cat(tab, sep = "\n")
}