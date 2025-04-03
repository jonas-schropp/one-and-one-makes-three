#' Provide docx table for lavaan models
#' 
#' The function takes a fitted lavaan model and returns a table of parameter 
#' estimates, confidence intervals and p-values for the model.
#' 
#'  - Confidence intervals are bootstrapped and bias corrected.
#'  - p values are based on the z-statistic and the assumption that it is normally distributed.
#'  - The z-statistic is just the estimate divided by the (bootstrap-based) standard errors.
#' 
#' @param fit the model fit with lavaan::sem
#' @param caption title for the table
#' @param file.name name for the word file to export
#'  
#' @returns
#' This is an impure function. It both returns the table as well as saves it 
#' to `results/tables`.
#' 
model_table <- function(
    fit, 
    caption = "", 
    file.name
    ) {
  
  require(dplyr)
  require(flextable)
  
  fm <- fitmeasures(fit) %>% round(3)
    
  
  tidied <- lavaan::parameterestimates(
    object = fit, 
    ci = TRUE, level = 0.95,
    boot.ci.type = "bca.simple",
    standardized = TRUE
  )
  
  
  fix_names <- function(var) {
    
    require(stringr)
    
    var <- var %>%
      str_replace_all('\\w_RF_AAI', 'Mentalization (T1)') %>%
      str_replace_all('\\w_RF_PDI', 'Mentalization (T2)') %>%
      str_replace_all('\\w_Bindung', 'Secure attachment (T1)') %>%
      str_replace_all('\\w_Sensitivity', 'Parental sensitivity (T2)') 
    
    var[var == ''] <- '(intercept)'
    
    return(var)
    
  }
  
  
  get_parents <- function(lhs, rhs, op) {
    
    par1 <- substr(lhs, 1, 1) 
    par2 <- substr(rhs, 1, 1)
    
    par1[par1 == "V"] <- "father"
    par2[par2 == "V"] <- "father"
    par1[par1 == "M"] <- "mother"
    par2[par2 == "M"] <- "mother"
    
    parents <- paste0(par1, "<-", par2)
    parents[par2 == par1] <- par1[par2 == par1]
    parents[par2 == ''] <- par1[par2 == '']
    parents[op == '<-->'] <- paste0(par1[op == '<-->'], "<->", par2[op == '<-->'])
    
    return(parents)
    
  }
  
  
  get_operator <- function(lhs, rhs) {
    
    l <- length(lhs)
    op <- character(length = l)
    
    for (i in 1:l) {
      
      if (lhs[i] == rhs[i]) {
        op[i] <- "<-->"
      } else if (substr(lhs[i], 3, 10) == substr(rhs[i], 3, 10)) {
        op[i] <- "<-->"
      } else {
        op[i] <- "<--"
      }
      
    }
    
    op
    
  }
  
  
  format_path <- function(outcome, op, predictor) {
    
    path <- paste0(outcome, op, predictor)
    path[predictor == '(intercept)'] <- paste0(
      outcome[predictor == '(intercept)'], ', intercept'
    )
    
    return(path)
    
  }
  
  
  overall <- paste0(
    'Chi-Sq=', fm['chisq'], ' (p=', fm['pvalue'], ')',
    '; CFI=', fm['cfi'], 
    '; TLI=', fm['tli'],
    '; RMSEA=', fm['rmsea'], ' 95% CI [', fm['rmsea.ci.lower'], '-', fm['rmsea.ci.upper'], ']',
    '; N=40'
  )
  
  
  tbl <- tidied %>%
    mutate(
      star = case_when(
        pvalue >= 0.05 ~ '',
        pvalue < 0.05 & pvalue >= 0.01 ~ '*',
        pvalue < 0.01 ~ '**'
      ),
      
      op = get_operator(lhs, rhs),
      parent = get_parents(lhs, rhs, op),
      
      outcome = fix_names(lhs),
      predictor = fix_names(rhs),
      path = format_path(outcome, op, predictor),
      
      beta = paste0(round(std.all, 2), star),
      `b [95% CI]` = paste0(
        round(est, 2), ' [', round(ci.lower, 2), '; ', round(ci.upper, 2), ']'
      ),
      se = round(se, 2),
      p = substr(as.character(round(pvalue, 3)), 2, 5),
      p = if_else(p == '', '<.001', p),
      
    ) %>%
    select(path, parent, beta, `b [95% CI]`, se, p) %>%
    flextable() %>%
    add_footer_lines(
      c(overall,
        'beta is the path coefficient with all variables standardized.',
        'b [95% CI] is the unstandardized path coefficient with a 95% bias-corrected and accelerated confidence interval.',
        'se is the bootstrapped standard error of the estimate.',
        '* p < .05; ** p < .01',
        'all calculations are based on 10,000 bootstrap replicates.'
      )
    ) %>%
    set_caption(caption) %>% 
    set_table_properties(width = 1, layout = "autofit")
  
  out.path <- here::here("results/tables", file.name)
  
  save_as_docx(
    tbl,
    path = out.path
  )
  
  return(tbl)
  
}
