#### This script holds all custom functions
writeLines("Loading custom functions...")

set.seed(1234)
writeLines("Seed set : 1234")

#### DGP simulation functions ####

## DGP 1 One Treatment Group, Time-Invariant Treatment Effects
dgp_1_sim <- function(nobs = 500, 
                      nperiods = 100,
                      nobsgroups = 50,
                      treated.period = floor(nperiods / 2)) {
  unit <- tibble(
    unit = 1:nobs,
    # create observation groups similar to US states
    obsgroup = sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=0.5
    unit_fe = rnorm(nobs, 0, 0.5),
  )
  
  # Shuffle obsgroup and assign treatment status
  shuffled_groups <- sample(unique(unit$obsgroup))
  half <- length(shuffled_groups) %/% 2
  
  unit <- unit %>%
    mutate(
      
      # gen treatment and control groups
      group = case_when(
        obsgroup %in% shuffled_groups[1:half] ~ treated.period,
        obsgroup %in% shuffled_groups[(half + 1):length(shuffled_groups)] ~ nperiods + 1
      ),
      # Mark Control as never treated
      evertreated = ifelse(group == treated.period, 1, 0),
      
      # avg yearly treatment effects by group
      avg.te = case_when(
        group == treated.period ~ 1,
        TRUE ~ 0
      )) %>%
    # gen unit-specific yearly treatment effects 
    rowwise() %>% 
    mutate(te = rnorm(1, avg.te, .2)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal with SD = 0.5
    period_fe = rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    mutate(error = rnorm(nrow(.), 0, 0.5)) %>% 
    mutate(treat = ifelse(evertreated == 1 & period > treated.period, 1, 0)) %>%
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    group_by(unit) %>%
    mutate(cum.t.eff = cumsum(t.eff)) %>%
    ungroup() %>%
    mutate(y = unit_fe + period_fe + t.eff + error) %>% 
    mutate(group = ifelse(group == nperiods + 1, 0, group)) %>%
    mutate(use_cum_te = F, use_cov = F) %>% 
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything(), -evertreated)
}

## DGP 2 One Treatment Group, Time-Varying Treatment Effects
dgp_2_sim <- function(nobs = 500, 
                  nperiods = 100,
                  nobsgroups = 50,
                  treated.period = floor(nperiods / 2)) {
  
  # Unit Fixed Effects
  unit <- tibble(
    unit = 1:nobs,
    # create observation groups similar to US states
    obsgroup = sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = rnorm(nobs, 0, 0.5),
    # gen treatment and control groups
    group = case_when(
      obsgroup %in% 1:(nobsgroups%/%2) ~ treated.period,
      obsgroup %in% (nobsgroups%/%2 + 1):nobsgroups ~ nperiods + 1
    ),
    # Mark Control as never treated
    evertreated = ifelse(group == treated.period, 1, 0),
    # avg yearly treatment effects by group
    avg.te = case_when(
      group == treated.period ~ .05,
      TRUE ~ 0
    )) %>%
    # gen unit-specific yearly treatment effects 
    rowwise() %>% 
    mutate(te = rnorm(1, avg.te, .025)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = rnorm(nrow(.), 0, 0.5)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(evertreated == 1 & period >= treated.period, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # add everything to get outcome
    group_by(unit) %>% 
    # Cumulative TE
    mutate(cum.t.eff = cumsum(t.eff)) %>% 
    mutate(y = unit_fe + period_fe + cum.t.eff + error) %>%
    mutate(group = ifelse(group == nperiods + 1, 0, group)) %>%
    mutate(use_cum_te = T, use_cov = F) %>% 
    ungroup() %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything(), -evertreated)
}

## DGP 3 Multiple Treatment Groups, Time-Invariant Homogeneous Treatment Effects

dgp_3_sim <- function(nobs = 500, 
                      nperiods = 100,
                      nobsgroups = 50,
                      treatgroups = c(nperiods/5, 2*(nperiods/5), 3*(nperiods/5), 4*(nperiods/5))) {
  
  # Unit Fixed Effects
  unit <- tibble(
    unit = 1:nobs,
    # create observation groups similar to US states
    obsgroup = sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = rnorm(nobs, 0, 0.5),
    # gen treatment and control groups
    group = case_when(
      obsgroup %in% 1:(nobsgroups%/%5) ~ treatgroups[1],
      obsgroup %in% ((nobsgroups%/%5) + 1):(2*nobsgroups%/%5) ~ treatgroups[2],
      obsgroup %in% ((2*nobsgroups%/%5) + 1):(3*nobsgroups%/%5) ~ treatgroups[3],
      obsgroup %in% ((3*nobsgroups%/%5) + 1):(4*nobsgroups%/%5) ~ treatgroups[4],
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods + 1
    ),
    # avg yearly treatment effects by group
    avg.te = case_when(
      group == treatgroups[1] ~ 1,
      group == treatgroups[2] ~ 1,
      group == treatgroups[3] ~ 1,
      group == treatgroups[4] ~ 1,
      TRUE ~ 0
    )) %>%
    # gen unit-specific yearly treatment effects 
    rowwise() %>% 
    mutate(te = rnorm(1, avg.te, .2)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = rnorm(nrow(.), 0, 1)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(period >= group, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    group_by(unit) %>%
    # Cumulative TE
    mutate(cum.t.eff = cumsum(t.eff)) %>%
    ungroup() %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + t.eff + error) %>% 
    mutate(group = ifelse(group == nperiods + 1, 0, group)) %>%
    mutate(use_cum_te = F, use_cov = F) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything())
}

## DGP 4 Multiple Treatment Groups,Time-Invariant Heterogeneous Treatment Effects
dgp_4_sim <- function(nobs = 500, 
                      nperiods = 100,
                      nobsgroups = 50,
                      treatgroups = c(nperiods/5, 2*(nperiods/5), 3*(nperiods/5), 4*(nperiods/5))) {
  
  # Unit Fixed Effects
  unit <- tibble(
    unit = 1:nobs,
    # create observation groups similar to US states
    obsgroup = sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = rnorm(nobs, 0, 0.5),
    # gen treatment and control groups
    group = case_when(
      obsgroup %in% 1:(nobsgroups%/%5) ~ treatgroups[1],
      obsgroup %in% ((nobsgroups%/%5) + 1):(2*nobsgroups%/%5) ~ treatgroups[2],
      obsgroup %in% ((2*nobsgroups%/%5) + 1):(3*nobsgroups%/%5) ~ treatgroups[3],
      obsgroup %in% ((3*nobsgroups%/%5) + 1):(4*nobsgroups%/%5) ~ treatgroups[4],
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods + 1
    ),
    # avg yearly treatment effects by group
    avg.te = case_when(
      group == treatgroups[1] ~ 1.5,
      group == treatgroups[2] ~ 1,
      group == treatgroups[3] ~ 0.5,
      group == treatgroups[4] ~ 0.25,
      TRUE ~ 0
    )) %>%
    # gen unit-specific yearly treatment effects 
    rowwise() %>% 
    mutate(te = rnorm(1, avg.te, .2)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = rnorm(nrow(.), 0, 1)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(period >= group, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    group_by(unit) %>%
    # Cumulative TE
    mutate(cum.t.eff = cumsum(t.eff)) %>%
    ungroup() %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + t.eff + error) %>% 
    mutate(group = ifelse(group == nperiods + 1, 0, group)) %>%
    mutate(use_cum_te = F, use_cov = F) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything())
}

## DGP 5 Multiple Treatment Groups, Time-Varying Homogeneous Treatment Effects
dgp_5_sim <- function(nobs = 500, 
                      nperiods = 100,
                      nobsgroups = 50,
                      treatgroups = c(nperiods/5, 2*(nperiods/5), 3*(nperiods/5), 4*(nperiods/5))) {
  
  # Unit Fixed Effects
  unit <- tibble(
    unit = 1:nobs,
    # create observation groups similar to US states
    obsgroup = sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = rnorm(nobs, 0, 0.5),
    # gen treatment and control groups
    group = case_when(
      obsgroup %in% 1:(nobsgroups%/%5) ~ treatgroups[1],
      obsgroup %in% ((nobsgroups%/%5) + 1):(2*nobsgroups%/%5) ~ treatgroups[2],
      obsgroup %in% ((2*nobsgroups%/%5) + 1):(3*nobsgroups%/%5) ~ treatgroups[3],
      obsgroup %in% ((3*nobsgroups%/%5) + 1):(4*nobsgroups%/%5) ~ treatgroups[4],
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods + 1
    ),
    # avg yearly treatment effects by group
    avg.te = case_when(
      group != nperiods ~ .05,
      TRUE ~ 0
    )) %>%
    # gen unit-specific yearly treatment effects 
    rowwise() %>% 
    mutate(te = rnorm(1, avg.te, .025)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = rnorm(nrow(.), 0, 1)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(period >= group, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # calculate the cumulative sum of treatment effect within each unit
    group_by(unit) %>%
    mutate(cum.t.eff = cumsum(t.eff)) %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + cum.t.eff + error) %>% 
    mutate(group = ifelse(group == nperiods + 1, 0, group)) %>%
    mutate(use_cum_te = T, use_cov = F) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything()) %>% 
    ungroup()
}

## DGP 6  Multiple Treatment Groups, Time-Varying Heterogeneous Treatment Effects
dgp_6_sim <- function(nobs = 500, 
                      nperiods = 100,
                      nobsgroups = 50,
                      treatgroups = c(nperiods/5, 2*(nperiods/5), 3*(nperiods/5), 4*(nperiods/5))) {
  
  # Unit Fixed Effects
  unit <- tibble(
    unit = 1:nobs,
    # create observation groups similar to US states
    obsgroup = sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = rnorm(nobs, 0, 0.5),
    # gen treatment and control groups
    group = case_when(
      obsgroup %in% 1:(nobsgroups%/%5) ~ treatgroups[1],
      obsgroup %in% ((nobsgroups%/%5) + 1):(2*nobsgroups%/%5) ~ treatgroups[2],
      obsgroup %in% ((2*nobsgroups%/%5) + 1):(3*nobsgroups%/%5) ~ treatgroups[3],
      obsgroup %in% ((3*nobsgroups%/%5) + 1):(4*nobsgroups%/%5) ~ treatgroups[4],
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods + 1
    ),
    # avg yearly treatment effects by group
    avg.te = case_when(
      group == treatgroups[1] ~ .05,
      group == treatgroups[2] ~ .075,
      group == treatgroups[3] ~ .1,
      group == treatgroups[4] ~ .2,
      TRUE ~ 0
    )) %>%
    # gen unit-specific yearly treatment effects 
    rowwise() %>% 
    mutate(te = rnorm(1, avg.te, .025)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = rnorm(nrow(.), 0, 0.5)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(period >= group, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # calculate the cumulative sum of treatment effect within each unit
    group_by(unit) %>%
    mutate(cum.t.eff = cumsum(t.eff)) %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + cum.t.eff + error) %>%
    mutate(group = ifelse(group == nperiods + 1, 0, group)) %>%
    mutate(use_cum_te = T, use_cov = F) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything()) %>% 
    ungroup()
}

## DGP 7  Multiple Treatment Groups, Time-Varying Heterogeneous TE, 
# CONDITIONAL PARALLEL TRENDS
dgp_7_sim <- function(nobs = 500, 
                      nperiods = 100,
                      nobsgroups = 50,
                      treatgroups = c(nperiods/5, 2*(nperiods/5), 3*(nperiods/5), 4*(nperiods/5))) {
  
  # Unit Fixed Effects
  unit <- tibble(
    unit = 1:nobs,
    obsgroup = sample(1:nobsgroups, nobs, replace = T),
    unit_fe = rnorm(nobs, 0, 0.5),
    group = case_when(
      obsgroup %in% 1:(nobsgroups%/%5) ~ treatgroups[1],
      obsgroup %in% ((nobsgroups%/%5) + 1):(2*nobsgroups%/%5) ~ treatgroups[2],
      obsgroup %in% ((2*nobsgroups%/%5) + 1):(3*nobsgroups%/%5) ~ treatgroups[3],
      obsgroup %in% ((3*nobsgroups%/%5) + 1):(4*nobsgroups%/%5) ~ treatgroups[4],
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods + 1
    ),
    avg.te = case_when(
      group == treatgroups[1] ~ .05,
      group == treatgroups[2] ~ .075,
      group == treatgroups[3] ~ .1,
      group == treatgroups[4] ~ .2,
      TRUE ~ 0
    )) %>%
    rowwise() %>% 
    mutate(te = rnorm(1, avg.te, .045)) %>% 
    ungroup()
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    period_fe = rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit, period, and nuisance parameter that breaks common trends
  crossing(unit, period) %>%
    mutate(
      nuisance = 0.005 * period * group + rnorm(n(), 0, 0.02),
      error = rnorm(n(), 0, 0.5),
      treat = ifelse(period >= group, 1, 0),
      t.eff = ifelse(treat == 1, te, 0),
      cum.t.eff = ave(t.eff, unit, FUN = cumsum),
      y = unit_fe + period_fe + cum.t.eff + error + nuisance,
      group = ifelse(group == nperiods + 1, 0, group),
      use_cum_te = T,
      use_cov = T) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, nuisance, everything())
  
}

## DGP 8  Multiple Treatment Groups, Time-Varying Heterogeneous TE, 
# NO PARALLEL TRENDS
dgp_8_sim <- function(nobs = 500, nperiods = 100){
  data = dgp_7_sim(nobs = nobs, nperiods = nperiods)
  # set flag to false so that estimators do not use nuisance covariate
  data$use_cov = F 
  
  return(data)
}


#### Helper functions for Estimation ####

timer <- function(func, ...){
  start_time = Sys.time()  # Record the start time
  do.call(func, list(...)) # Run the function
  end_time = Sys.time() # Record the end time
  cat("Execution Time:", round((end_time - start_time), digits = 2), "seconds\n")
}

get_num_periods <- function(df) {
  length(unique(df$period))
}

get_num_units <- function(df) {
  length(unique(df$unit))
}

get_num_groups <- function(df) {
  length(unique(df$group))
}

get_first_treatment <- function(df) {
  sort(unique(df$group))[2] # 0 is control
}

## Function to compute RMSE
# some estimators do not have an estimate of relative period -1, hence na.rm = T
compute_rmse <- function(true, estimated) {
  sqrt(mean((true - estimated) ^ 2, na.rm = T)) 
}

# set number of cores
globalcores = parallel::detectCores()

### Function to extract true static ATT from simulated data
get_true_ATT <- function(data){
  units = get_num_units(data)
  periods = get_num_periods(data)
  treat_times <- sort(get_treat_times(data)) 
  tgroups <- treat_times[-1]
  
  # check whether DGP uses t.eff or cum.t.eff
  use_cum_effect = as.logical(data$use_cum_te[1])
  
  # get number of treated units
  numtreated = data %>% filter(group %in% tgroups) %>%  get_num_units()
  
  # get share of each treatment group
  group_share <- data %>%
    filter(group %in% tgroups) %>%
    group_by(group) %>%
    summarise(share = n() / numtreated)
  
  # calculate true static treatment effect
  group_data <- data %>%
    filter(group %in% tgroups) %>%
    left_join(group_share, by = "group") %>%
    group_by(group) %>%
    summarise(est = mean(avg.te), share = mean(share))
  
  true_att = sum(group_data$est * group_data$share) / sum(group_data$share)
  
  return(true_att)
}

### Helper function to subset relative period ATT estimates
get_rel_indices <- function(data) {
  nperiods = get_num_periods(data)
  negative_index = - get_first_treatment(data) + 1
  positive_index = nperiods + negative_index - 1
  vec_length = positive_index - negative_index + 1
  return(list(
    neg = negative_index, 
    pos = positive_index,
    len = vec_length))
}


### Function to extract true relative period ATT from simulated data
get_true_relative_period_ATT <- function(data) {
  
  indices = get_rel_indices(data)
  negative_index = indices$neg
  positive_index = indices$pos
  
  att_data <- data %>%
    # Create a relative period variable
    group_by(unit) %>%
    filter(group != 0) %>% 
    mutate(relative_period = period - group) %>%
    ungroup() %>%
    filter((relative_period > -100) & (relative_period <= 80)) %>%
    # Group by relative period and compute average treatment effect
    group_by(relative_period) %>% #filter(treat == 1) %>% 
    summarise(ATT = mean(cum.t.eff), .groups = 'drop')
  
  # Extract ATT as a vector
  rel_att <- att_data %>% select(ATT) %>% slice_tail(n = indices$len) %>% pull()
  
  # Assign names to the att vector
  names(rel_att) <- seq(from = negative_index, to = positive_index)
  
  return(rel_att)
}

## Extract true values from data within simulation iteration
est_true <- function(data, true_rel_att, iteration = 0) {
  
  # Check whether to use static ATET or relative periods
  relative = as.logical(data$use_cum_te[1]) 
  
  if(relative) {
    # Subset relative period deviations -10 to 10 inclusive to return
    return_sequence <- as.character(-10:10)
    rel_dev_true <- rep(0, length(return_sequence))
    names(rel_dev_true) <- return_sequence
    
    # Subset dev using names
    out_dev <- rel_dev_true[names(rel_dev_true) %in% return_sequence]
    out = tibble(
      estimator = "TRUE",
      iter = iteration,
      rel_att_0 = true_rel_att["0"],
      rel_rmse = 0,
      rel_bias = 0) %>% 
      bind_cols(as_tibble(t(rel_dev_true))
      )
  } else {
    out = tibble(
      estimator = "TRUE",
      iter = iteration,
      ATET = get_true_ATT(data)
      )
  }
  
  return(out)
}


######## Estimator Wrappers ########
#### Estimate MC-NNM ####
# helper
get_cohorts <- function(data) {
  df = data  %>% as.data.frame()
  cohorts = get.cohort(df, "treat", index = c("unit", "period"))
  cohorts$FirstTreat[cohorts$Cohort == "Cohort:0"] = NA
  cohorts$Time_to_Treatment[cohorts$Cohort == "Cohort:0"] = NA
  cohorts$Cohort[cohorts$Cohort == "Cohort:0"] = "Control"
  
  return(cohorts)
}

est_mc <- function(data, true_rel_att, iteration = 0, k = 2, n_lam = 5){
  nperiods = get_num_periods(data)
  cohorts = get_cohorts(data) # helper function to label cohorts for fect
  # check whether we should use covariates in estimation
  use_covariates = as.logical(cohorts$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7, 8
  if (use_covariates) {
    form = y ~ treat + nuisance
  } else {
    form = y ~ treat
  }
  
  out = suppressMessages(fect::fect(form, data = cohorts, 
                                    method = "mc", index = c("unit","period"), 
                                    force = "two-way", group = 'Cohort',
                                    normalize = T)) # normalize to speed up
  
  # Check whether to use static ATET or relative periods
  relative = as.logical(cohorts$use_cum_te[1]) 
  
  if(relative) {
    # Subset to keep relative period estimates of interest
    indices = get_rel_indices(cohorts)
    negative_index = indices$neg
    positive_index = indices$pos
    rel_att = tail(out$att, indices$len) # get relative period effect estimates incl. 0
    
    names(rel_att) <- seq(from = negative_index, to = positive_index)
    
    # Compute RMSE of relative period ATT estimates
    rel_rmse = compute_rmse(rel_att, true_rel_att)
    
    # Compute absolute deviation
    dev = rel_att - true_rel_att
    
    # Subset relative period deviations -10 to 10 inclusive to return
    return_sequence <- as.character(-10:10)
    
    # Subset dev using names
    out_dev <- dev[names(dev) %in% return_sequence]
    
    mc_output <- tibble(
      estimator = "MC-NNM",
      iter = iteration,
      rel_att_0 = rel_att["0"],
      rel_rmse = rel_rmse,
      rel_bias = mean(dev, na.rm = T)) %>% 
      bind_cols(as_tibble(t(out_dev))
      )
  } else {
    att_avg = out$att.avg # get static effect estimate
    mc_output <- tibble(
      estimator = "MC-NNM",
      iter = iteration,
      ATET = att_avg
    )
  }
  
  return(mc_output)
}

#### Estimate TWFE ####
est_twfe <- function(data, true_rel_att, iteration = 0){
  ## check whether we should use covariates in estimation
  use_covariates = as.logical(data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    form = y ~ treat + nuisance | unit + period
    form_dyn = y ~ i(rel_period, ref=c(-1, Inf)) + nuisance | unit + period
  } else {
    form = y ~ treat | unit + period
    form_dyn = y ~ i(rel_period, ref=c(-1, Inf)) | unit + period
  }
  
  ## Check whether to use static ATET or relative periods
  relative = as.logical(data$use_cum_te[1]) 
  
  if(relative) {
    indices = get_rel_indices(data)
    negative_index = indices$neg
    positive_index = indices$pos
    
    model = suppressMessages(did2s::event_study(data, "y", "unit", "group", 
                                        "period", estimator = "TWFE"))
    
    rel_att_raw = tail(model$estimate, indices$len) # get relative period effect estimates incl. 0
    # Have to manually wrangle vector together as value for -1 is missing
    rel_att_left = rel_att_raw[1:(abs(negative_index) -1)]
    rel_att_middle = NA
    rel_att_right = rel_att_raw[abs(negative_index):(indices$len - 1)]
    rel_att = c(rel_att_left, rel_att_middle, rel_att_right)

    names(rel_att) <- seq(from = negative_index, to = positive_index)
    
    # Compute RMSE of relative period ATT estimates
    rel_rmse = compute_rmse(rel_att, true_rel_att)
    
    # Compute absolute deviation
    dev = rel_att - true_rel_att
    
    # Subset relative period deviations -10 to 10 inclusive to return
    return_sequence <- as.character(-10:10)
    
    # Subset dev using names
    out_dev <- dev[names(dev) %in% return_sequence]
    
    twfe_output <- tibble(
      estimator = "TWFE",
      iter = iteration,
      rel_att_0 = rel_att["0"],
      rel_rmse = rel_rmse,
      rel_bias = mean(dev, na.rm = T)) %>% 
      bind_cols(as_tibble(t(out_dev))
      )
  } else {
    model = fixest::feols(form , data = data)
    if (use_covariates) {
      att_avg = unname(model$coefficients)[1]
    } else {
      att_avg = unname(model$coefficients)
    }
    
    twfe_output <- tibble(
      estimator = "TWFE",
      iter = iteration,
      ATET = att_avg
    )
  }
  
  return(twfe_output)
}

#### Estimate Callaway-Sant'Anna ####
est_cs <- function(data, true_rel_att, iteration = 0){
  # check whether we should use covariates in estimation
  use_covariates = as.logical(data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    mod = suppressWarnings(did::att_gt(yname = "y",
                                       tname = "period",
                                       idname = "unit",
                                       gname = "group",
                                       xformla = ~ 0 + nuisance,
                                       bstrap = F,
                                       data = data,
                                       print_details = F))
  } else {
    mod = did::att_gt(yname = "y",
                      tname = "period",
                      idname = "unit",
                      gname = "group",
                      bstrap = F,
                      data = data,
                      print_details = F)
  }
  
  ## Check whether to use static ATET or relative periods
  relative = as.logical(data$use_cum_te[1]) 
  
  if(relative) {
    indices = get_rel_indices(data)
    negative_index = indices$neg
    positive_index = indices$pos
    
    # model = suppressMessages(did2s::event_study(data, "y", "unit", "group", 
    #                                             "period", estimator = "TWFE"))
    
    model = did::aggte(mod, type = "dynamic", cband = F)
    
    # for DGP 1 and 2, have to manually wrangle vector together as value for -49 is missing
    if (get_num_groups(data) < 5) {
      rel_att_right = tail(model$att.egt, indices$len) # get relative period effect estimates incl. 0
      rel_att_left = NA
      rel_att = c(rel_att_left, rel_att_right)
    } else {
      rel_att = tail(model$att.egt, indices$len)
    }
    
    names(rel_att) <- seq(from = negative_index, to = positive_index)
    
    # Compute RMSE of relative period ATT estimates
    rel_rmse = compute_rmse(rel_att, true_rel_att)
    
    # Compute absolute deviation
    dev = rel_att - true_rel_att
    
    # Subset relative period deviations -10 to 10 inclusive to return
    return_sequence <- as.character(-10:10)
    
    # Subset dev using names
    out_dev <- dev[names(dev) %in% return_sequence]
    
    cs_output <- tibble(
      estimator = "CS",
      iter = iteration,
      rel_att_0 = rel_att["0"],
      rel_rmse = rel_rmse,
      rel_bias = mean(dev, na.rm = T)) %>% 
      bind_cols(as_tibble(t(out_dev))
      )
  } else {
    model = did::aggte(mod, "simple")
    if (use_covariates) {
      att_avg = unname(model$coefficients)[1]
    } else {
      att_avg = unname(model$coefficients)
    }
    
    cs_output <- tibble(
      estimator = "CS",
      iter = iteration,
      ATET = model$overall.att
    )
  }
  
  return(cs_output)
}

#### Estimate Sun and Abraham ####
est_sa <- function(data, true_rel_att, iteration = 0){
  # check whether we should use covariates in estimation
  use_covariates = as.logical(data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    form = y ~ nuisance + sunab(group, period) | unit + period
  } else {
    form = y ~ sunab(group, period) | unit + period
  }
  
  # Estimate
  mod = fixest::feols(form, data)
  
  ## Check whether to use static ATET or relative periods
  relative = as.logical(data$use_cum_te[1]) 
  
  if(relative) {
    indices = get_rel_indices(data)
    negative_index = indices$neg
    positive_index = indices$pos
    
    rel_att_raw_full = aggregate(mod, agg = "period")[,1]
    rel_att_raw = tail(rel_att_raw_full, indices$len) # get relative period effect estimates incl. 0
    # Have to manually wrangle vector together as value for -1 is missing
    rel_att_left = rel_att_raw[1:(abs(negative_index) -1)]
    rel_att_middle = NA
    rel_att_right = rel_att_raw[abs(negative_index):(indices$len - 1)]
    rel_att = c(rel_att_left, rel_att_middle, rel_att_right)
    
    names(rel_att) <- seq(from = negative_index, to = positive_index)
    
    # Compute RMSE of relative period ATT estimates
    rel_rmse = compute_rmse(rel_att, true_rel_att)
    
    # Compute absolute deviation
    dev = rel_att - true_rel_att
    
    # Subset relative period deviations -10 to 10 inclusive to return
    return_sequence <- as.character(-10:10)
    
    # Subset dev using names
    out_dev <- dev[names(dev) %in% return_sequence]
    
    sa_output <- tibble(
      estimator = "SA",
      iter = iteration,
      rel_att_0 = rel_att["0"],
      rel_rmse = rel_rmse,
      rel_bias = mean(dev, na.rm = T)) %>% 
      bind_cols(as_tibble(t(out_dev))
      )
  } else {
    att_avg = aggregate(mod, agg = "att")[1]
    
    sa_output <- tibble(
      estimator = "SA",
      iter = iteration,
      ATET = att_avg
    )
  }

  return(sa_output)
}

#### Estimate de Chaisemartin & D’Haultfœuille using DIDmultiplegt ####
# This estimator can only estimate static ATTs (DGPs 1,3,4)
est_dcdh <- function(data, true_rel_att, iteration = 0){
  
  ## Check whether to use static ATET or relative periods
  relative = as.logical(data$use_cum_te[1])
  if(!relative) {
    mod = DIDmultiplegt::did_multiplegt(
      data, "y", "group", "period", controls = c(),
      "treat", dynamic = 0, average_effect = "simple")
    
    dcdh_output <- tibble(
      estimator = "dCdH",
      iter = iteration,
      ATET = mod$effect
    )
    return(dcdh_output)
  }
}


#### Estimate Borusyak Jaravel Spiess using didimputation ####
est_bjs <- function(data, true_rel_att, iteration = 0){
  # change data because didimputation does not work when depvar is called "y"
  imput_dat = data %>% 
    rename(dep_var = y) %>% as.data.table()
  
  # check whether we should use covariates in estimation
  use_covariates = as.logical(imput_dat$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    fs = ~ nuisance | unit + period
  } else {
    fs = ~ 0 | unit + period
  }
  
  ## Check whether to use static ATET or relative periods
  relative = as.logical(imput_dat$use_cum_te[1]) 
  
  if(relative) {
    indices = get_rel_indices(imput_dat)
    negative_index = indices$neg
    positive_index = indices$pos
    
    model = didimputation::did_imputation(
      data = imput_dat, yname = "dep_var", gname = "group", 
      tname = "period", idname = "unit", first_stage = fs,
      horizon = T)
    
    # DIDImputation only reports positive relative time periods
    # therefore subset true_rel_att for relative periods >= 0
    true_rel_att_nonneg <- true_rel_att[as.numeric(names(true_rel_att)) >= 0]
    
    # use length of this to get right # of estimates
    rel_att = tail(model$estimate, length(true_rel_att_nonneg))
    
    # Set names
    names(rel_att) <- seq(from = 0, to = length(true_rel_att_nonneg) - 1)
    
    # Compute RMSE of relative period ATT estimates
    rel_rmse = compute_rmse(rel_att, true_rel_att_nonneg)
    
    # Compute absolute deviation
    dev = rel_att - true_rel_att_nonneg
    
    # Subset relative period deviations -10 to 10 inclusive to return
    return_sequence <- as.character(-10:10)
    
    # Subset dev using names
    out_dev_right <- dev[names(dev) %in% return_sequence]
    # -10 to -1 not available:
    out_dev_left = rep(NA, 10)
    out_dev = c(out_dev_left, out_dev_right)
    
    names(out_dev) <- return_sequence
    
    bjs_output <- tibble(
      estimator = "BJS",
      iter = iteration,
      rel_att_0 = rel_att["0"],
      rel_rmse = rel_rmse,
      rel_bias = mean(dev, na.rm = T)) %>% 
      bind_cols(as_tibble(t(out_dev))
      )
    
    
  } else {
    att_avg = didimputation::did_imputation(
      data = imput_dat, yname = "dep_var", gname = "group", first_stage = fs,
      tname = "period", idname = "unit")$estimate
    
    bjs_output <- tibble(
      estimator = "BJS",
      iter = iteration,
      ATET = att_avg
    )
  }

  return(bjs_output)
}


#### Functions to run simulations ####
timer <- function(func, ...){
  start_time = Sys.time()  # Record the start time
  do.call(func, list(...)) # Run the function
  end_time = Sys.time() # Record the end time
  cat("Execution Time:", round((end_time - start_time), digits = 2), "seconds\n")
}

## Function to compare estimators in an iteration
run_sim <- function(i, fun, n = 500, t = 100, quiet = T) {
  # print progress
  if (quiet == F) {
    cat(" ", i, "...")
  }
  # make data from function
  # use with_seed to ensure reproducibility
  dt = withr::with_seed(i, fun(nobs = n, nperiods = t))
  
  ## compute true relative period ATTs
  # check whether to use static ATET or relative periods
  relative = as.logical(dt$use_cum_te[1]) 
  
  if (relative) {
    true_rel_att <- get_true_relative_period_ATT(dt)  
  } else {
    true_rel_att <- 0
  }
  
  # Get true values
  true = est_true(data = dt, true_rel_att = true_rel_att, iteration = i)
  # Estimation
  mc = est_mc(data = dt, true_rel_att = true_rel_att, iteration = i)  
  did = est_twfe(data = dt, true_rel_att = true_rel_att, iteration = i)  
  cs = est_cs(data = dt, true_rel_att = true_rel_att, iteration = i)  
  sa = est_sa(data = dt, true_rel_att = true_rel_att, iteration = i)  
  dcdh = est_dcdh(data = dt, true_rel_att = true_rel_att, iteration = i)  
  bjs = est_bjs(data = dt, true_rel_att = true_rel_att, iteration = i)  
  
  if (relative) {
    results_tibble <- rbind(true, mc, did, cs, sa, bjs) %>%
      mutate(nperiods = get_num_periods(dt)) %>% 
      select(iter, everything())
  } else {
    results_tibble <- rbind(true, mc, did, cs, sa, dcdh, bjs) %>%
      mutate(nperiods = get_num_periods(dt)) %>% 
      select(iter, everything())
  }
  
  return(results_tibble)
}

## Function to execute the simulation, not parallelized
run_sim_map <- function(iterations, sim_function, n = 500, t = 100) {
  cat("Simulating ", deparse(substitute(sim_function)), ", ",
      length(iterations), "Iterations\n", "Iteration: ")
  # Use purrr:map() to run simulations 
  start_time = Sys.time()
  out_list <- purrr::map(iterations, ~run_sim(.x, sim_function, 
                                              n = n, t = t, quiet = F))
  
  # Combine the list of data frames into a single data frame
  out_df <- dplyr::bind_rows(out_list)
  end_time = Sys.time()
  cat("\nElapsed time: ", round(end_time - start_time, digits = 1), "\n")
  cat("Simulation of  ", deparse(substitute(sim_function)), " complete \n")
  
  # Return the combined data frame
  return(out_df)
}


## Function to execute the simulation, parallelized using mclapply
run_sim_parallel <- function(iterations, sim_function, n = 500, t = 100) {
  cat("Simulating ", deparse(substitute(sim_function)), ", ",
      length(iterations), "Iterations:\n")
  # Use mclapply() to run simulations in parallel
  start_time = Sys.time()
  out = suppressWarnings(
    mclapply(
      iterations, function(i) run_sim(i, sim_function, 
                                      n = n, t = t,
                                      quiet = F), mc.cores = globalcores)
  )
  
  # Bind all the output data frames into a single data frame
  out_df <- do.call(rbind, out)
    end_time = Sys.time()
    cat("Elapsed time: ", round(end_time - start_time, digits = 1), "\n")
    cat("Simulation of  ", deparse(substitute(sim_function)), " complete \n")
  # Return the combined data frame
  return(tibble(out_df))
}


#### Utility functions for simulating ####
# parallelization leads to errors, which leads to entire iterations missing
# this helper counts and removes those rows
verify_sim_results <- function(input_tibble) {
  error_count <- sum(grepl("error|Error", input_tibble$iter)) 
  
  clean_tibble <- input_tibble[!(grepl("error|Error", input_tibble$iter, ignore.case = TRUE)), ]
  
  cat("Number of corrupted rows:", error_count, "\n")
  return(clean_tibble)
}

verify_iteration_counts <- function(input_tibble) {
  iteration_counts <- table(input_tibble$iter)
  
  results_static <- names(input_tibble)[3] == "ATET"
  if (results_static) {
    incorrect_iterations <- iteration_counts[iteration_counts != 7]
    if (length(incorrect_iterations) == 0) {
      cat("All iterations appear exactly 7 times.\n")
    } else {
      cat("Incorrect iteration counts:\n")
      print(incorrect_iterations)
    }
  } else {
    incorrect_iterations <- iteration_counts[iteration_counts != 6]
    if (length(incorrect_iterations) == 0) {
      cat("All iterations appear exactly 6 times.\n")
    } else {
      cat("Incorrect iteration counts:\n")
      print(incorrect_iterations)
    }
  }
  
}

# function to save sim results to RDS
save_sim_results <- function(input_tibble, file_name = "test") {
  # get the current date and time
  current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  
  # Get number of periods
  nperiods = input_tibble$nperiods[1]
  
  # create the file name with the custom name and current date and time
  full_file_name <- paste0(file_name, "-", nperiods, "-periods_", current_time, ".rds")
  
  # construct the relative file path
  file_path <- file.path('SimResults', full_file_name)
  
  # Check if 'SimResults' directory exists, if not create it
  if (!dir.exists('SimResults')) {
    dir.create('SimResults')
  }
  
  # save tibble as RDS file
  saveRDS(input_tibble, file = file_path)
  
  # print confirmation
  cat("Tibble saved as:", full_file_name, "\n\n")
}

# function to load most recently saved version of each DGP results
load_sim_results <- function(file_name = "test", periods) { 
  # specify directory 
  directory <- 'SimResults'
  
  # use a pattern match to filter 
  # pattern <- paste0('^', file_name, '.*\\.rds$')
  pattern <- paste0('^', file_name, '-', periods, '-periods_.*\\.rds$') 
  
  # list all files in dir that match the pattern
  files <- list.files(path = directory, pattern = pattern)
  
  # if no file exists, handle error
  if (length(files) == 0) {
    cat("No files matching the pattern found\n")
    return(NULL)
  }
  
  # Extract dates from the filenames
  dates <- str_extract(files, "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}")
  dates <- str_replace_all(dates, "-", "")
  dates <- str_replace_all(dates, "_", "")
  dates <- str_replace_all(dates, ":", "")
  
  # Order the files by dates
  sorted_files = files[order(dates, decreasing = TRUE)]
  
  # construct full path 
  most_recent_file = file.path(directory, sorted_files[1])
  
  loaded_data = readRDS(most_recent_file) # load most recent file
  
  # make sure numeric values are numeric
  results_static <- names(loaded_data)[3] == "ATET"
  if (results_static) {
    loaded_data = loaded_data %>% mutate(ATET = as.numeric(ATET),
                           nperiods = as.numeric(nperiods))
  } else {
    loaded_data = loaded_data %>%
      mutate_if(~ names(.x) != "estimator", as.numeric)
    }
  
  cat("Loaded file:", sorted_files[1], "\n") # print confirmation
  
  # return the loaded data
  return(loaded_data)
}

# function to load all results and trim to 500 iterations
load_all_results <- function(){
  writeLines("Loading saved results for 100 periods")
  sim1_100 <<- keep_iterations(load_sim_results("DGP-1", periods = 100), 500)
  sim2_100 <<- keep_iterations(load_sim_results("DGP-2", periods = 100), 500)
  sim3_100 <<- keep_iterations(load_sim_results("DGP-3", periods = 100), 500)
  sim4_100 <<- keep_iterations(load_sim_results("DGP-4", periods = 100), 500)
  sim5_100 <<- keep_iterations(load_sim_results("DGP-5", periods = 100), 500)
  sim6_100 <<- keep_iterations(load_sim_results("DGP-6", periods = 100), 500)
  sim7_100 <<- keep_iterations(load_sim_results("DGP-7", periods = 100), 500)
  sim8_100 <<- keep_iterations(load_sim_results("DGP-8", periods = 100), 500)
  writeLines("Loading saved results for 55 periods")
  sim1_55 <<- keep_iterations(load_sim_results("DGP-1", periods = 55), 500)
  sim2_55 <<- keep_iterations(load_sim_results("DGP-2", periods = 55), 500)
  sim3_55 <<- keep_iterations(load_sim_results("DGP-3", periods = 55), 500)
  sim4_55 <<- keep_iterations(load_sim_results("DGP-4", periods = 55), 500)
  sim5_55 <<- keep_iterations(load_sim_results("DGP-5", periods = 55), 500)
  sim6_55 <<- keep_iterations(load_sim_results("DGP-6", periods = 55), 500)
  sim7_55 <<- keep_iterations(load_sim_results("DGP-7", periods = 55), 500)
  sim8_55 <<- keep_iterations(load_sim_results("DGP-8", periods = 55), 500)
  writeLines("All Results loaded succesfully")
  
}


# function to trim excess iterations
keep_iterations <- function(sim_data, num_iterations = 500) {
  
  if (length(unique(sim_data$iter)) == num_iterations) {
    out = sim_data
  } else if(length(unique(sim_data$iter)) > num_iterations){
    it = rep(1:500, each = 7, length.out = 7 * num_iterations)
    out = sim_data[sim_data$iter %in% it, ]
  } else {
    out = "Not enough complete iterations present"
  }
  
  return(out)
}


#### Functions to analyze sim results ####

# function to create summary tibble
analyze_sim_results <- function(sim_results) { #, type = "static"
  
  results_static <- names(sim_results)[3] == "ATET"
  # keep original order
  sim_results$estimator = factor(sim_results$estimator, 
                                 levels = unique(sim_results$estimator))
  
  # get true values for bias
  true_values = filter(sim_results, estimator == "TRUE")

  if (results_static) {
    # create 'TRUE' row for summary table
    true_summary_static = true_values %>%
      summarise(
        estimator = "TRUE",
        min_est  = min(ATET),
        mean_est = mean(ATET, na.rm = F),
        max_est = max(ATET),
        sd_est = sd(ATET),
        bias = NA,
        rmse = NA,
        .groups = 'drop'
      )
    
    static_summary = sim_results %>% # summary results for ATET ATETimates
      filter(estimator != "TRUE") %>%
      group_by(estimator) %>%
      summarise(
        min_est  = min(ATET),
        mean_est = mean(ATET, na.rm = F),
        max_est = max(ATET),
        sd_est = sd(ATET),
        bias = mean(ATET) - mean(true_values$ATET, na.rm = F), #sqrt(mean((ATET - mean(true_values$ATET, na.rm = F))^2, na.rm = F))
        rmse = compute_rmse(true_values$ATET, ATET),
        .groups = 'drop'  # avoid the grouped_df class for output
      )
    
    final_summary = bind_rows(true_summary_static, static_summary) %>% 
      select("Estimator" = estimator, 
             "Min" = min_est, "Mean" = mean_est, "Max" = max_est, 
             "SD" = sd_est, "Bias" = bias, "RMSE" = rmse)
  } else {
    true_summary_dynamic = true_values %>%
      summarise(
        estimator = "TRUE",
        min_est  = min(rel_att_0),
        mean_est = mean(rel_att_0, na.rm = F),
        max_est = max(rel_att_0),
        sd_est = sd(rel_att_0),
        bias_est = NA,
        rmse_est = NA,
        bias_all_rp = NA,
        rmse_all_rp = NA,
        .groups = 'drop'
      )
    
    dynamic_summary = sim_results %>% # summary results for dynamic-type estimate
      filter(estimator != "TRUE") %>% 
      group_by(estimator) %>%
      summarise(
        min_est  = min(rel_att_0),
        mean_est = mean(rel_att_0, na.rm = F),
        max_est = max(rel_att_0),
        sd_est = sd(rel_att_0),
        bias_est = mean(rel_att_0) - mean(true_values$rel_att_0, na.rm = F),
        rmse_est = compute_rmse(true_values$rel_att_0, rel_att_0),
        rmse_all_rp = mean(rel_rmse),
        bias_all_rp = mean(rel_bias),
        .groups = 'drop'  # avoid the grouped_df class for output
      )
    
    final_summary = bind_rows(true_summary_dynamic, dynamic_summary) %>% 
      select("Estimator" = estimator, 
             "Min" = min_est, "Mean" = mean_est, "Max" = max_est, 
             "SD" = sd_est, "t=0 Bias" = bias_est, "t=0 RMSE" = rmse_est,
             "All Relative Periods Bias" = bias_all_rp,
             "ALL Relative Periods RMSE" = rmse_all_rp)
  }
  
  return(final_summary)
}

# variant of same function to output nice-looking HTML table
results_html <- function(sim_results, type = "static") {
  
  # keep original order
  sim_results$estimator = factor(sim_results$estimator, 
                                 levels = unique(sim_results$estimator))
  
  # get true values for bias
  true_values = filter(sim_results, estimator == "TRUE")
  results_static <- names(sim_results)[3] == "ATET"
  
  if (results_static) {
    # create 'TRUE' row for summary table (static)
    true_summary_static = true_values %>%
      summarise(
        estimator = "TRUE",
        min_est  = min(ATET),
        mean_est = mean(ATET, na.rm = F),
        max_est = max(ATET),
        sd_est = sd(ATET),
        bias = NA,
        rmse = NA,
        .groups = 'drop'
      )
    
    static_summary = sim_results %>% 
      filter(estimator != "TRUE") %>%
      group_by(estimator) %>%
      summarise(
        min_est  = min(ATET),
        mean_est = mean(ATET, na.rm = F),
        max_est = max(ATET),
        sd_est = sd(ATET),
        bias = mean(ATET) - mean(true_values$ATET, na.rm = F),
        rmse = compute_rmse(true_values$ATET, ATET),
        .groups = 'drop'
      )
    
    final_summary = bind_rows(true_summary_static, static_summary) %>% 
      select("Estimator" = estimator, "Min" = min_est, "Mean" = mean_est, "Max" = max_est, 
             "SD" = sd_est, "Bias" = bias, "RMSE" = rmse)
    
  } else {
    # create 'TRUE' row for summary table (dynamic)
    true_summary_dynamic = true_values %>%
      summarise(
        estimator = "TRUE",
        min_est  = min(rel_att_0),
        mean_est = mean(rel_att_0, na.rm = F),
        max_est = max(rel_att_0),
        sd_est = sd(rel_att_0),
        bias_est = NA,
        rmse_est = NA,
        bias_all_rp = NA,
        rmse_all_rp = NA,
        .groups = 'drop'
      )
    
    dynamic_summary = sim_results %>% 
      filter(estimator != "TRUE") %>% 
      group_by(estimator) %>%
      summarise(
        min_est  = min(rel_att_0),
        mean_est = mean(rel_att_0, na.rm = F),
        max_est = max(rel_att_0),
        sd_est = sd(rel_att_0),
        bias_est = mean(rel_att_0) - mean(true_values$rel_att_0, na.rm = F),
        rmse_est = compute_rmse(true_values$rel_att_0, rel_att_0),
        rmse_all_rp = mean(rel_rmse),
        bias_all_rp = mean(rel_bias),
        .groups = 'drop'
      )
    
    final_summary = bind_rows(true_summary_dynamic, dynamic_summary) %>% 
      select("Estimator" = estimator, 
             "Min" = min_est, "Mean" = mean_est, "Max" = max_est, 
             "SD" = sd_est, "t=0 Bias" = bias_est, "t=0 RMSE" = rmse_est,
             "All Relative Periods Bias" = bias_all_rp,
             "ALL Relative Periods RMSE" = rmse_all_rp)
  }
  
  # round numeric columns to 3 digits
  numeric_columns <- sapply(final_summary, is.numeric)
  final_summary <- datatable(final_summary, options = list(autoWidth = TRUE, scrollX = TRUE, dom = 't')) %>% 
    DT::formatRound(columns = which(numeric_columns), digits = 3)
  
  return(final_summary)
}


# function to save results tables to latex
options(knitr.kable.NA = '-')
save_table_results <- function(sumdata, 
                               caption = "", 
                               file_name = "table.tex") {
  align = ifelse(sapply(sumdata, is.numeric), "c", "l") # fix alignment
  t_label = sub("\\.tex$", "", file_name) # strip .tex from filename
  
  sumdata %>%
    kable(format = "latex", booktabs = T, caption = caption,
          digits = ifelse(names(sumdata) == "SD", 3, 2), 
          align = align, linesep = "", label = t_label) %>%
    footnote(general = "Results obtained from 500 iterations", 
             general_title = "") %>% 
    row_spec(1, bold = T) %>% #, bold = T, hline_after = T color = "red"
    kable_styling(latex_options = "hold_position") %>% 
    save_kable(file = file_name)
}


#### Data Plotting Functions ####
# Set Theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# Helper function to get unique treatment start times
get_treat_times <- function(df) {
  unique(df$group)
}

# Basic plotting function
dgp_plot_basic <- function(df, sim_num = NULL) {
  unique_groups <- sort(unique(df$group))
  group_labels <- as.character(unique_groups)
  names(group_labels) <- unique_groups
  group_labels["0"] <- "Control"
  
  if (is.null(sim_num)) {
    # extract the number from the df function name
    sim_num = as.integer(gsub("[^0-9]", "", substitute(df)))
  }
  # set as plot title
  plot_title = paste("Outcome Data from One Draw of Simulation", sim_num)
  
  df %>%
    ggplot(aes(x = period, y = y, group = unit)) +
    geom_line(alpha = 0.1, color = "grey") +
    geom_line(data = df %>%
                group_by(group, period) %>%
                summarize(dep_var = mean(y), .groups = 'drop'), # Add .groups = 'drop'
              aes(x = period, y = dep_var, group = factor(group),
                  color = factor(group)),
              size = 0.5) +
    labs(x = "", y = "Value", color = "Treatment group   ") +
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    ggtitle(plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    scale_color_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F"),
                       labels = group_labels)
}


# More elaborate plotting function
dgp_plot <- function(df, subtitle = "", sim_num = NULL){
  # Define your color palette
  my_palette = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F")
  
  suppressWarnings({
    # get treatment times and remove the control group
    treat_times <- get_treat_times(df)
    treat_times <- treat_times[treat_times != min(treat_times)]
    
    # relabel the control group
    control_label <- as.character(0) # min period considered as control group
    
    if (is.null(sim_num)) {
      # extract the number from the df function name
      sim_num = as.integer(gsub("[^0-9]", "", substitute(df)))
    }
    # set as plot title
    plot_title = paste("Outcome Data from One Draw of Simulation", sim_num)
    cap = "All observations from one draw of the simulation, colored lines represent group means for each treatment group."
    
    p = df %>% 
      ggplot(aes(x = period, y = y, group = unit)) + 
      geom_line(alpha = .1, color = "grey") + 
      geom_line(data = df %>% 
                  group_by(group, period) %>% 
                  summarize(dep_var = mean(y), .groups = "drop"),
                aes(x = period, y = dep_var, group = factor(group),
                    color = factor(group)),
                size = 0.5) + 
      labs(x = "Period", y = expression(Y[italic(i)*","*italic(t)]), color = "Treatment Groups") + 
      theme(legend.position = 'bottom',
            axis.title = element_text(size = 14),
            axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
            axis.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size=12),
            plot.subtitle = element_text(hjust = 0.5),
            legend.title = element_text(size = 12, hjust = 0.5)) +
      guides(color = guide_legend(title.position = "top")) +
      ggtitle(plot_title) +
      labs(subtitle = subtitle, caption = cap) +
      scale_x_continuous(breaks = seq(0, 100, 20))  # ticks at 20, 40, 60, 80, 100
    
    # Add vertical lines for each treatment group
    vlines <- data.frame(xintercept = treat_times, group = treat_times)
    p = p + geom_vline(data = vlines, aes(xintercept=xintercept, color=factor(group)), 
                        linewidth = 0.5, linetype = "dashed", show.legend = FALSE)
    
    # Set color scale and replace treatment group 100 with "Control" in legend
    p = p + scale_color_manual(values = my_palette,
                                labels = function(x) ifelse(x == control_label, "Control", x))
    
    p
  })
}

# Function to plot deviations from true Effect
plot_est_dev <- function(df) {
  dynamic <- names(df)[3] == "rel_att_0"
  
  if (dynamic) {
    # get estimator names
    estimators = c("CS", "SA", "TWFE", "MC-NNM", "BJS", "TRUE") 
    
    # Define your custom color palette
    my_palette = c("TWFE" = "#F28E2B", "CS" = "#E15759", "SA" = "#B07AA1",  #"#4E79A7" "#F28E2B" "#E15759" "#76B7B2" "#59A14F" "#EDC948" "#B07AA1" "#FF9DA7" "#9C755F" "#BAB0AC"
                   "BJS" = scales::alpha("#59A14F", 0.8), "MC-NNM" = "#4E79A7", "TRUE" = "#BAB0AC") 
    
    # Define line sizes and line types
    line_sizes <- c(0.6, 0.4, 0.4, 0.8, 0.4, 0.7)  # Thicker line for MC-NNM and TRUE
    line_types <- c("solid", "solid", "dashed", "solid", "solid", "longdash")  # Dashed line for TRUE
    
    event_study_df <- df %>%
      select(iter, estimator, `-10`:`10`) %>%
      pivot_longer(cols = `-10`:`10`, names_to = "relative_period", values_to = "value") %>%
      mutate(relative_period = as.numeric(relative_period),
             estimator = factor(estimator, levels = estimators)) %>%
      group_by(estimator, relative_period) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
    
    title = "Deviation from True Treatment Effect"
    cap = "Upper Panel shows all observations from one draw of the simulation with group means. \nLower panel shows for each estimator the mean of point estimates of the Treatment Effect in\nRelative Periods -10 to 10. Missing Points mean that an Estimate is not produced for that relative period."
    
    # Build plot
    p <- event_study_df %>%
      ggplot(aes(x = relative_period, y = value, group = estimator, color = estimator)) +
      geom_line(aes(linetype = estimator, size = estimator), na.rm = T) +  # conditional line type and size
      geom_point(aes(shape = estimator), na.rm = T) +
      scale_size_manual(values = line_sizes) + # define line sizes
      scale_linetype_manual(values = line_types) + # define line types
      scale_color_manual(values = my_palette) +  # apply color palette
      theme_minimal() +
      labs(x = "Relative Time Period", y = "Estimated Effect - True Effect", color = "Estimator",
           caption = cap) +
      scale_x_continuous(breaks = -10:10) +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom") +
      guides(color = guide_legend(override.aes = list(size=3)), 
             size = "none", linetype = "none", shape = "none")  # hide size, linetype legends
  } else {
    estimators = c("MC-NNM", "TWFE", "CS", "SA", "dCdH", "BJS")
    # Assign colors
    my_palette = c("MC-NNM" = "#4E79A7", "TWFE" = "#F28E2B", "CS" = "#E15759", 
                   "SA" = "#B07AA1", "dCdH" = "#EDC948", "BJS" = "#59A14F") 
    
  }
}

# function to plot densities of estimates of each estimator
plot_est_dens <- function(df) {
  nperiods = df$nperiods[1]
  dynamic = names(df)[3] == "rel_att_0"
  
  if (dynamic) {
    # get estimator names
    estimators = c("CS", "SA", "TWFE", "MC-NNM", "BJS") #, "TRUE" 
    
    # Define your custom color palette
    my_palette = c("TWFE" = "#F28E2B", "CS" = "#E15759", "SA" = "#B07AA1",  #"#4E79A7" "#F28E2B" "#E15759" "#76B7B2" "#59A14F" "#EDC948" "#B07AA1" "#FF9DA7" "#9C755F" "#BAB0AC"
                   "BJS" = "#59A14F", "MC-NNM" = "#4E79A7") # , "TRUE" = "#BAB0AC"
    true_v = df %>% filter(estimator == "TRUE") %>% 
      summarize(mean_e = mean(rel_att_0), min_e = min(rel_att_0), max_e = max(rel_att_0))
    
    title = "Distributions of Relative Period 0 Effect Estimates"
    plot_title = paste0(title, ", ", nperiods, " Periods")
    cap = "Density of point estimates of the treatment effect in relative period 0 for each estimator. Vertical Red Lines indicate minimum, mean, and maximum of true parameter value."
    
    # filter data
    df = df %>% filter(estimator %in% estimators)
    
    # Create plot variable depending on 'dynamic' argument
    plot_var = "rel_att_0"
    
    # Set factor levels for estimator to control facet order
    df$estimator = factor(df$estimator, levels = estimators)
    
    # Order the dataframe by estimator
    df = df %>% arrange(estimator)
    
    # Build the plot
    p = ggplot(df, aes(x = get(plot_var), color = factor(estimator))) +
      geom_density(fill = "grey", alpha = 0.5) +  # fill color is grey
      scale_color_manual(values = my_palette) +
      geom_vline(aes(xintercept = true_v$min_e), 
                 linetype = "dashed", color = "red", alpha = 0.45) +
      geom_vline(aes(xintercept = true_v$mean_e), 
                 linetype = "dashed", color = "red") +
      geom_vline(aes(xintercept = true_v$max_e), 
                 linetype = "dashed", color = "red", alpha = 0.45) +
      guides(color = "none", fill = "none") +
      facet_wrap(~ estimator, scales = "free") + 
      theme(legend.position = 'bottom',
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 9),
            plot.title = element_text(hjust = 0.5, size=12),
            plot.subtitle = element_text(hjust = 0.5),
            legend.title = element_text(size = 12, hjust = 0.5)) +
      ggtitle(plot_title) + labs(x = "Point Estimate", y = "Density",
                            caption = cap)
    
    } else {
    estimators = c("MC-NNM", "TWFE", "CS", "SA", "dCdH", "BJS")
    # Assign colors
    my_palette = c("MC-NNM" = "#4E79A7", "TWFE" = "#F28E2B", "CS" = "#E15759", 
                   "SA" = "#B07AA1", "dCdH" = "#EDC948", "BJS" = "#59A14F") 
    
    true_v = df %>% filter(estimator == "TRUE") %>% 
      summarize(mean_e = mean(ATET), min_e = min(ATET), max_e = max(ATET))
    
    title = "Distributions of ATET Estimates"
    plot_title = paste0(title, ", ", nperiods, " Periods")
    cap = "Density of point estimates of the ATET for each estimator. Vertical Red Lines indicate minimum, mean, and maximum of true parameter value, if only one is visible, the true value does not vary."
    
    # filter data
    df = df %>% filter(estimator %in% estimators)
    
    # Create plot variable depending on 'dynamic' argument
    plot_var = "ATET"
    
    # Set factor levels for estimator to control facet order
    df$estimator = factor(df$estimator, levels = estimators)
    
    # Order the dataframe by estimator
    df = df %>% arrange(estimator)
    
    # Build the plot
    p = ggplot(df, aes(x = get(plot_var), color = factor(estimator))) +
      geom_density(fill = "grey", alpha = 0.5) +  # fill color is grey
      scale_color_manual(values = my_palette) +
      geom_vline(aes(xintercept = true_v$min_e), 
                 linetype = "dashed", color = "red", alpha = 0.45) +
      geom_vline(aes(xintercept = true_v$mean_e), 
                 linetype = "dashed", color = "red") +
      geom_vline(aes(xintercept = true_v$max_e), 
                 linetype = "dashed", color = "red", alpha = 0.45) +
      guides(color = "none", fill = "none") +
      facet_wrap(~ estimator, scales = "free") + 
      theme(legend.position = 'bottom',
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 9),
            plot.title = element_text(hjust = 0.5, size=12),
            plot.subtitle = element_text(hjust = 0.5),
            legend.title = element_text(size = 12, hjust = 0.5)) +
      ggtitle(plot_title) + labs(x = "Point Estimate", y = "Density",
                            caption = cap)
  }

  return(p)
}

## function to combine dgp plot and densities and save
plot_combined <- function(dgp_number, save = F) {
  # Generate the DGP plot based on the number argument
  dgp_data = switch(dgp_number,
                    dgp_1_sim(),
                    dgp_2_sim(),
                    dgp_3_sim(),
                    dgp_4_sim(),
                    dgp_5_sim(),
                    dgp_6_sim(),
                    dgp_7_sim(),
                    dgp_8_sim())
  dgp_plot = dgp_plot(dgp_data, "",  sim_num = dgp_number)
  
  # Generate the density plot based on the number argument
  sim_data = switch(dgp_number,
                    sim1,
                    sim2,
                    sim3,
                    sim4,
                    sim5,
                    sim6,
                    sim7,
                    sim8)
  plot_est_dens = plot_est_dens(sim_data)
  
  # combine and arrange plots
  plot_combined <- grid.arrange(dgp_plot, plot_est_dens, ncol = 1)
 
  if (save ==T & Sys.info()[7] == "ts") {
    fp = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Figures"
    filename = paste0("Sim_", dgp_number, ".png")
    ggsave(filename, plot = plot_combined, path = fp,
           width = 18, height = 20, units = "cm")
  }
}

writeLines("Done!")
