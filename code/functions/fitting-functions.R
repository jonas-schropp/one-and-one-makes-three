fit_full_model <- function(data, n.boot = 2000, seed = 101) {
  
  mod <- '
        V_RF_AAI ~ rf1V*1 + rf1BinV*V_Bindung        # Path 1: RF T1 -> RF T3
        M_RF_AAI ~ rf1M*1 + rf1BinM*M_Bindung
        V_RF_PDI ~ rfV*1 + rfVV*V_RF_AAI + rfMV*M_RF_AAI + rfBinV*V_Bindung        # Path 1: RF T1 -> RF T3
        M_RF_PDI ~ rfM*1 + rfMM*M_RF_AAI + rfVM*V_RF_AAI + rfBinM*M_Bindung
        V_Sensitivity ~ seV*1 + seVV*V_RF_PDI + seMV*M_RF_PDI #+ seBinV*V_Bindung  # Path 2: RF T3 -> Sensitivity T3
        M_Sensitivity ~ seM*1 + seMM*M_RF_PDI + seVM*V_RF_PDI #+ seBinM*M_Bindung  
        V_Bindung ~ mbinv*1         # Mean Bindung T1
        M_Bindung ~ mbinm*1
        V_RF_PDI ~~ vx11*V_RF_PDI     # Variance for RF T3
        M_RF_PDI ~~ vx12*M_RF_PDI           
        V_RF_AAI ~~ vx21*V_RF_AAI     # Variance for RF T1
        M_RF_AAI ~~ vx22*M_RF_AAI           
        V_Sensitivity ~~ ve1*V_Sensitivity  # Error variance for Sensitivity
        M_Sensitivity ~~ ve2*M_Sensitivity                  
        V_Bindung ~~ varBinV*V_Bindung        # Variance for RF T1 CPQ
        M_Bindung ~~ varBinM*M_Bindung
        M_RF_PDI ~~ covRFT3*V_RF_PDI             # Covariance of RF T3
        M_RF_AAI ~~ covRFT1*V_RF_AAI             # Covariance of RF T1
        V_Bindung ~~ covCPQ*M_Bindung
        M_Sensitivity ~~ covSens*V_Sensitivity    # Covariance of errors (Sensitivity)
        
        # indirect effects
        h1a_father := seVV*rfMV*rf1BinV
        h1a_mother := seMM*rfVM*rf1BinM
        
        h1b_father := seVV*rfMV
        h1b_mother := seMM*rfVM
        
        h3_father := rfMV*seVV
        h3_mother := rfVM*seMM
  '
  
  
  fit <- lavaan::sem(
    model = mod, 
    fixed.x = FALSE, 
    data = data, 
    missing = "fiml", 
    estimator = "ML", 
    bootstrap = n.boot, 
    se = 'boot',
    parallel = 'snow',
    ncpus = 6,
    iseed = seed
  )
  
  
  return(fit)
  
  
}




fit_restricted_model <- function(data, n.boot = 2000, seed = 101) {
  
  mod <- '
        V_RF_AAI ~ rf1*1 + rf1B*V_Bindung        # Path 1: RF T1 -> RF T3
        M_RF_AAI ~ rf1*1 + rf1B*M_Bindung
        V_RF_PDI ~ rf2*1 + rfA*V_RF_AAI + rfP*M_RF_AAI + rfBP*V_Bindung        # Path 1: RF T1 -> RF T3
        M_RF_PDI ~ rf2*1 + rfA*M_RF_AAI + rfP*V_RF_AAI + rfBP*M_Bindung
        V_Sensitivity ~ se*1 + seA*V_RF_PDI + seP*M_RF_PDI #+ seB*V_Bindung  # Path 2: RF T3 -> Sensitivity T3
        M_Sensitivity ~ se*1 + seA*M_RF_PDI + seP*V_RF_PDI #+ seB*M_Bindung  
        V_Bindung ~ mB*1         # Mean Bindung T1
        M_Bindung ~ mB*1
        V_RF_PDI ~~ vx1*V_RF_PDI     # Variance for RF T3
        M_RF_PDI ~~ vx1*M_RF_PDI           
        V_RF_AAI ~~ vx2*V_RF_AAI     # Variance for RF T1
        M_RF_AAI ~~ vx2*M_RF_AAI           
        V_Sensitivity ~~ ve*V_Sensitivity  # Error variance for Sensitivity
        M_Sensitivity ~~ ve*M_Sensitivity                  
        V_Bindung ~~ varBin*V_Bindung        # Variance for RF T1 CPQ
        M_Bindung ~~ varBin*M_Bindung
        M_RF_PDI ~~ covRFT3*V_RF_PDI             # Covariance of RF T3
        M_RF_AAI ~~ covRFT1*V_RF_AAI             # Covariance of RF T1
        V_Bindung ~~ covCPQ*M_Bindung
        M_Sensitivity ~~ covSens*V_Sensitivity    # Covariance of errors (Sensitivity)
        
        # indirect effects
        h1a := seA*rfA*rf1B
        h1b := seA*rfA
        
        h3 := rfP*seA
  '
  
  
  fit <- lavaan::sem(
    model = mod, 
    fixed.x = FALSE, 
    data = data, 
    missing = "fiml", 
    estimator = "ML", 
    bootstrap = n.boot, 
    se = 'boot',
    parallel = 'snow',
    ncpus = 6,
    iseed = seed
  )
  
  
  return(fit)
  
  
}




fit_model <- function(data, n.boot = 2000, seed = 101) {
  
  ### Constrained:
  #   - mean attachment
  #   - rf t1 ~ attachment
  #   - rf t2 ~ rf t1 (actor)
  #   - rf t2 ~ rf t1 (partner)
  #   - rf t2 ~ attachment
  
  mod <- '
        V_RF_AAI ~ rf1V*1 + rf1B*V_Bindung        # Path 1: RF T1 -> RF T3
        M_RF_AAI ~ rf1M*1 + rf1B*M_Bindung
        
        V_RF_PDI ~ rf2V*1 + rfA*V_RF_AAI + rfP*M_RF_AAI + rf2BV*V_Bindung        # Path 1: RF T1 -> RF T3
        M_RF_PDI ~ rf2M*1 + rfA*M_RF_AAI + rfP*V_RF_AAI + rf2BM*M_Bindung
        
        V_Sensitivity ~ se*1 + seVV*V_RF_PDI + seVM*M_RF_PDI  # Path 2: RF T3 -> Sensitivity T3
        M_Sensitivity ~ se*1 + seMM*M_RF_PDI + seMV*V_RF_PDI
        
        V_Bindung ~ mB*1         # Mean Bindung T1
        M_Bindung ~ mB*1
        
        V_RF_PDI ~~ V_RF_PDI     # Variance for RF T3
        M_RF_PDI ~~ M_RF_PDI           
        V_RF_AAI ~~ V_RF_AAI     # Variance for RF T1
        M_RF_AAI ~~ M_RF_AAI           
        V_Sensitivity ~~ V_Sensitivity  # Error variance for Sensitivity
        M_Sensitivity ~~ M_Sensitivity                  
        V_Bindung ~~ V_Bindung        # Variance for RF T1 CPQ
        M_Bindung ~~ M_Bindung
        
        M_RF_PDI ~~ V_RF_PDI             # Covariance of RF T3
        M_RF_AAI ~~ V_RF_AAI             # Covariance of RF T1
        V_Bindung ~~ M_Bindung
        M_Sensitivity ~~ V_Sensitivity    # Covariance of errors (Sensitivity)
        
        # indirect effects
        h1a_father := seVV*rfA*rf1B
        h1a_mother := seMM*rfA*rf1B
        
        h1b_father := seVV*rfA
        h1b_mother := seMM*rfA
        
        h3_father := rfP*seVV
        h3_mother := rfP*seMM
        
        
  '
  
  
  fit <- lavaan::sem(
    model = mod, 
    fixed.x = FALSE, 
    data = data, 
    missing = "fiml", 
    estimator = "ML", 
    bootstrap = n.boot, 
    se = 'boot',
    parallel = 'snow',
    ncpus = 6,
    iseed = seed
  )
  
  
  return(fit)
  
  
}



eval_mod <- function(fit) {
  
  require(lavaan)
  require(dplyr)
  
  modificationindices(fit) %>%
    arrange(-mi) %>%
    filter(mi > 2)
  
}
