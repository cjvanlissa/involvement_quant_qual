add_letters <- function(contrast_list, tab, tab_unst_sections, superscript = FALSE){
  stars <- tab_unst_sections$`Defined parameters`[match(names(contrast_list),
                                                        tab_unst_sections$`Defined parameters`$Parameter), ]
  
  for(i in 1:nrow(stars)){
    if(any(grepl("\\*", stars[i, ]))){
      for(c in which(grepl("\\*", stars[i, ]))){
        if(superscript){
          tab[contrast_list[[i]], c] <- paste0(tab[contrast_list[[i]], c], "$^{", letters[letter], "}$")
          tab[contrast_list[[i]], c] <- gsub("\\}\\$\\$\\^\\{", "", tab[contrast_list[[i]], c])
        } else {
          tab[contrast_list[[i]], c] <- paste0(tab[contrast_list[[i]], c], letters[letter])
          tab[contrast_list[[i]], c] <- gsub("\\}\\$\\$\\^\\{", "", tab[contrast_list[[i]], c])
        }
      
        letter <<- letter + 1
      }
    }
  }
  tab
}

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
  tab <- apa_table(x, format = "latex", col_spanners = colspan_fun(x, skip = 1), ...)
  tab <- gsub('(^"|"$)', '', tab)
  tab <- gsub("^\\[1\\] ", "", tab)
  
  tab <- gsub("\\n(.+?) & NA & NA & NA & NA & NA & NA & NA & NA\\\\\\\\\\n", "\\\\multicolumn\\{9\\}\\{l\\}\\{\\1\\}\\\\\\\\", tab, perl = TRUE)
  #tab <- gsub("\\\\", "\\", tab)
  #tab <- gsub("\\\\begin", "\\begin", tab)
  #tab <- gsub("\\\\n(?!=orm)", "\\\\", tab, perl = TRUE)
  #tab <- gsub("%", "\\%", tab)
  #cat(tab, sep = "\n")
  #knitr::raw_latex(tab)
  tab
}


reg_table_escape_false <- function(x, ...){
  rownames(x) <- NULL
  names(x) <- gsub("^.+est_sig$", "Est.", names(x))
  names(x) <- gsub("^.+confint$", "95\\% HPDI", names(x))
  tab <- apa_table(x, format = "latex", col_spanners = colspan_fun(x, skip = 1), escape = FALSE, ...)
  browser()
  tab <- gsub('(^"|"$)', '', tab)
  tab <- gsub("^\\[1\\] ", "", tab)
  tab <- gsub("\\n(.+?) & NA & NA & NA & NA & NA & NA & NA & NA\\\\\\\\\\n", "\\\\multicolumn\\{9\\}\\{l\\}\\{\\1\\}\\\\\\\\", tab, perl = TRUE)
  #tab <- gsub("\\\\", "\\", tab)
  #tab <- gsub("\\\\begin", "\\begin", tab)
  #tab <- gsub("\\\\n(?!=orm)", "\\\\", tab, perl = TRUE)
  #tab <- gsub("%", "\\%", tab)
  #cat(tab, sep = "\n")
  #knitr::raw_latex(tab)
  tab
}

reg_table_html <- function(x, ...){
  rownames(x) <- NULL
  names(x) <- gsub("^.+est_sig$", "Est.", names(x))
  names(x) <- gsub("^.+confint$", "95\\% HPDI", names(x))
  multiline_rows <- rowSums(is.na(x)) == 8
  multiline_text <- x[multiline_rows, 1]
  x[multiline_rows, 1] <- ""
  x[is.na(x)] <- ""
  tab <- capture.output(apa_table(x, format = "html", 
                                   ...)[1])
  browser()
  div_line <- grep("^-[ -]+$", tab)
  tab[which(multiline_rows)+div_line] <- sprintf(paste0("%-", nchar(tab[which(multiline_rows)[1]+div_line]), "s"), multiline_text)
  cat(tab, sep = "\n")
}