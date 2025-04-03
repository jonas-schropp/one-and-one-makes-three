#' Correlation Matrix
#' 
#' This function calculates the correlation matrix, using either Pearson's r 
#' or Kendall's tau, depending on variable type. It additionally incorporates 
#' some descriptives. 
#' 
#' @param data the full data frame. Function will fail if it doesn't contain the 
#' specified 13 variables.
#' @param nboot number of bootstrap replicates, by default 2000 (used in paper)
#' @param seed random seed, by default 101
#' @param file.name the name of the output file. Folder is always 'results/tables'
#' 
#' @details uses bootstrapping for p-value calculation
#' 
cor_matrix <- function(
    data, 
    nboot = 2000, 
    seed = 101, 
    file.name = 'correlation-matrix.docx'
    ) {
  
  require(boot)
  require(boot.pval)
  require(gtsummary)
  require(flextable)
  
  set.seed(seed)
  
  lbls <- c("age (F)", "age (M)", "annual net income", 
            "education (F)", "education (M)", "sensitivity (F)",
            "sensitivity (M)", "attachment (F)", "attachment (M)",
            "RF (F, PDI)", "RF (M, PDI)", "RF (F, AAI)", 
            "RF (M, AAI)")
  
  desc <- (
    data %>%
    droplevels() %>%
    tbl_summary(
      type = list(M_Sensitivity ~ 'continuous',
                  V_RF_PDI ~ 'continuous',
                  M_RF_PDI ~ 'continuous'),
      statistic = list(
        all_continuous() ~ "{mean} ({sd})", 
        all_categorical() ~ "{n} ({p}%)"
        )
      )
    )$table_body %>%
    filter(!is.na(stat_0)) %>%
    mutate(
      label = case_when(
        label == '< 32.000€' ~ '<32k€',
        label == '32.000€ - 48.000€' ~ '<32k€',
        label == '48.000€ - 67.000€' ~ '48k€-67k€',
        label == '> 67.000€' ~ '>67k€',
        .default = label
    ),
    label = na_if(label, var_label) ,
    stat = if_else(
      is.na(label), stat_0, NA) 
    ) %>%
    select(var_label, stat) %>%
    unique() %>%
    mutate(
      stat = gsub('\\.0 ', ' ', stat),
      stat = gsub('\\.00 ', ' ', stat),
      stat = gsub('\\.00\\,', '\\,', stat),
      stat = gsub('\\.00\\)', '\\)', stat),
      stat = gsub('\\.0\\,', '\\,', stat),
      stat = gsub('\\.0\\)', '\\)', stat)
    )
  
  desc$var_label <- lbls
  
  res <- matrix(ncol = 12, nrow = 13)
  
  data[,"V_Jahreseinkommen"] <- as.numeric(data[,"V_Jahreseinkommen"])
  data[,"V_Bildung"] <- as.numeric(data[,"V_Bildung"])
  data[,"M_Bildung"] <- as.numeric(data[,"M_Bildung"])
  
  for (i in 1:ncol(data)) {
    for (j in 2:ncol(data)) {
      
      x <- names(data)[i]
      y <- names(data)[j]
      
      if (i >= j) next
      
      ord <- c("V_Jahreseinkommen", "V_Bildung", "M_Bildung")
      if (x %in% ord | y %in% ord) {
        method <- 'kendall'
      } else {
        method <- 'pearson'
      }
      
      dataij <- data[,c(x, y)]
      
      boot_fun <- function(data, indices, method) {
        cor(x = data[indices, ], method = method, use = 'pairwise.complete.obs')[2]
      }
      
      boot_res <- boot(
        data = dataij, 
        statistic = boot_fun, 
        R = nboot,
        method = method
      )
      
      
      p <- boot.pval(
        boot_res = boot_res
      ) 
      
      strs <- character()
      if (p < 0.01) {
        strs <- '**'
      } else if (p < 0.05) {
        strs <- '*'
      }
      
      r <- paste0(
       round(boot_res$t0, 2), strs
      )
      r <- gsub('0\\.', '\\.', r)
      
      res[i,j-1] <- r
      
    }
  }
  

  tbl <- cbind(paste0('[', 1:13, '] ', lbls), desc$stat, res) %>%
    as.data.frame() 
  names(tbl) <- c(' ', '  ', paste0('[', 2:13, ']'))
  
  tbl <- tbl %>%
    flextable() %>%
    add_footer_lines(
      c('Note. Descriptive statistics in the first column are either mean (sd) or n (%).',
        "Correlations are calculated using Pearson's r for continuous and binary variables and Kendall's tau for ordered variables.",
        "*p < .05; **p < .01. p-values refer to 2000 bootstrap replicates. "
      )
    ) %>%
    set_caption('Table 3. Correlations of demographics, attachment representation, general and parental reflective functioning and sensitivity for mothers and fathers.') %>% 
    set_table_properties(width = 1, layout = "autofit")
  
  out.path <- here::here("results/tables", file.name)
  
  save_as_docx(
    tbl,
    path = out.path
  )
  
  return(tbl)
    
  
}
