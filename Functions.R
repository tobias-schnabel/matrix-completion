#### This script holds all custom functions
writeLines("Loading custom functions...")

set.seed(1234)
writeLines("Seed set : 1234")
### DGP functions

## DGP 1 One Treatment Group, Time-Invariant Treatment Effects
dgp_1_sim <- function(nobs = 1000, 
                      nperiods = 100,
                      nobsgroups = 50,
                      treated.period = 50) {
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
        obsgroup %in% shuffled_groups[(half + 1):length(shuffled_groups)] ~ nperiods
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
    mutate(use_cum_te = F, use_cov = F) %>% 
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything(), -evertreated)
}

## DGP 2 One Treatment Group, Time-Varying Treatment Effects
dgp_2_sim <- function(nobs = 1000, 
                  nperiods = 100,
                  nobsgroups = 50,
                  treated.period = 50) {
  
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
      obsgroup %in% (nobsgroups%/%2 + 1):nobsgroups ~ nperiods
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
    mutate(treat = ifelse(evertreated == 1 & period > treated.period, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # add everything to get outcome
    group_by(unit) %>% 
    # Cumulative TE
    mutate(cum.t.eff = cumsum(t.eff)) %>% 
    mutate(y = unit_fe + period_fe + cum.t.eff + error) %>%
    mutate(use_cum_te = T, use_cov = F) %>% 
    ungroup() %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything(), -evertreated)
}

## DGP 3 Multiple Treatment Groups, Time-Invariant Homogeneous Treatment Effects

dgp_3_sim <- function(nobs = 1000, 
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
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods
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
    mutate(use_cum_te = F, use_cov = F) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything())
}

## DGP 4 Multiple Treatment Groups,Time-Invariant Heterogeneous Treatment Effects
dgp_4_sim <- function(nobs = 1000, 
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
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods
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
    mutate(use_cum_te = F, use_cov = F) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything())
}

## DGP 5 Multiple Treatment Groups, Time-Varying Homogeneous Treatment Effects
dgp_5_sim <- function(nobs = 1000, 
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
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods
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
    mutate(use_cum_te = T, use_cov = F) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything()) %>% 
    ungroup()
}

## DGP 6  Multiple Treatment Groups, Time-Varying Heterogeneous Treatment Effects
dgp_6_sim <- function(nobs = 1000, 
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
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods
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
    mutate(use_cum_te = T, use_cov = F) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything()) %>% 
    ungroup()
}

## DGP 7  Multiple Treatment Groups, Time-Varying Heterogeneous TE, 
# CONDITIONAL PARALLEL TRENDS
dgp_7_sim <- function(nobs = 1000, 
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
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods
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
      use_cum_te = T,
      use_cov = T) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, nuisance, everything())
  
}

## DGP 8  Multiple Treatment Groups, Time-Varying Heterogeneous TE, 
# NO PARALLEL TRENDS
dgp_8_sim <- function(){
  data = dgp_7_sim()
  # set flag to false so that estimators do not use nuisance covariate
  data$use_cov = F 
  
  return(data)
}

### Function to extract true ATET and cumulative ATET from simulated data
get_true_values <- function(data){
  units = get_num_units(data)
  periods = get_num_periods(data)
  treat_times <- sort(get_treat_times(data)) 
  tgroups <- treat_times[-length(treat_times)]
  
  # check whether DGP uses t.eff or cum.t.eff
  use_cum_effect = as.logical(data$use_cum_te[1])
  
  # get number of treated units
  numtreated = data %>% filter(group %in% tgroups) %>%  get_num_units()
  
  # get share of each treatment group
  group_share <- data %>%
    filter(group %in% tgroups) %>%
    group_by(group) %>%
    summarise(share = n() / numtreated)
  
  # calculate true dynamic treatment effect
  if (use_cum_effect) {
    ce = data %>%  group_by(period) %>% 
      filter(group %in% tgroups) %>% 
      filter(treat == 1) %>% 
      summarise(mce = mean(cum.t.eff)) %>% 
      summarise(cum_est = sum(mce) / periods)
  } else {
    ce = data %>%  group_by(period) %>% 
      filter(group %in% tgroups) %>% 
      filter(treat == 1) %>% 
      summarise(mce = mean(t.eff)) %>% 
      summarise(cum_est = sum(mce) / periods)
  }
  
  # calculate true static treatment effect
  group_data <- data %>%
    filter(group %in% tgroups) %>%
    left_join(group_share, by = "group") %>%
    # left_join(ce, by = "period") %>%
    group_by(group) %>%
    summarise(est = mean(avg.te), 
              cum_est = mean(as.numeric(ce)), share = mean(share))
  
  return(group_data)
}


#### Estimator wrappers
# helpers
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

relabel_control_0 <- function(df){
  df %>% mutate(group = ifelse(group == max(group), 0, group))
}

# set number of cores
globalcores = parallel::detectCores()

## True values from data
est_true <- function(data, iteration = 0) {
  vals = get_true_values(data)
  weighted_est <- sum(vals$est * vals$share) / sum(vals$share)
  ce = mean(vals$cum_est)
  
  out = list(est = weighted_est, se = 0, cum_est = ce,
             cum_se = 0, iter = iteration, estimator = "TRUE")
  
  return(out)
}

## MC-NNM using fect (gsynth fails when no covariates for some reason)
# helper
get_cohorts <- function(data) {
  df = data  %>% as.data.frame()
  cohorts = get.cohort(df, "treat", index = c("unit", "period"))
  cohorts$FirstTreat[cohorts$Cohort == "Cohort:100"] = NA
  cohorts$Time_to_Treatment[cohorts$Cohort == "Cohort:100"] = NA
  cohorts$Cohort[cohorts$Cohort == "Cohort:100"] = "Control"
  
  return(cohorts)
}

# Estimate Full MC-NNM object
est_mc <- function(data, iteration = 0, b_iter = 100, k = 2, n_lam = 4){
  cohorts = get_cohorts(data)
  # check whether we should use covariates in estimation
  use_covariates = as.logical(data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    form = y ~ treat + nuisance
  } else {
    form = y ~ treat
  }
  
  model = suppressMessages(fect::fect(form, data = cohorts, 
                                      method = "mc", index = c("unit","period"), 
                                      se = T, force = "two-way",
                                      CV = T,  cores = globalcores, parallel = T,
                                      nboots = b_iter, group = 'Cohort',
                                      nlambda = n_lam, k = k))
  
  est_avg = unname(model$est.avg[,1:2]) #get static effect
  est_eff_calendar = c(model$est.eff.calendar[,1],0) # get dynamic point estimate
  est_eff_calendar[is.nan(est_eff_calendar)] = 0 # replace NaN with 0
  est_cal_se = c(model$est.eff.calendar[,2],0) # get dynamic SE
  est_cal_se[is.na(est_cal_se)] = 0 # replace NaN with 0
  
  mc_output = list(est = est_avg[1], se = est_avg[2],
                   cum_est = mean(est_eff_calendar), 
                   cum_se = mean(est_cal_se),
                   iter = iteration, estimator = "MC-NNM")
  
  return(mc_output)
}

### Helpers to set up event study

prep_es <- function(data){
  # Prepare data
  esdat <- data %>%
    mutate(group = ifelse(group == 100, Inf, group)) %>% #mark control
    mutate(rel_period = period - group) %>% # relative time to treatment
    mutate(evertreated = ifelse(group == get_num_periods(data), 0, 1)) %>% 
    dplyr::arrange(group, unit, period) %>% 
    ungroup()
}

## Canonical DiD, using fixest:::feols
est_canonical <- function(data, iteration = 0){
  # check whether we should use covariates in estimation
  use_covariates = as.logical(data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    form = y ~ treat + nuisance | unit + period
    form_dyn = y ~ i(rel_period, ref=c(-1, Inf)) + nuisance | unit + period
  } else {
    form = y ~ treat | unit + period
    form_dyn = y ~ i(rel_period, ref=c(-1, Inf)) | unit + period
  }
  
  static = fixest::feols(form , data = data)
  if (use_covariates) {
    stat_out = list(est = unname(static$coefficients)[1], se = unname(static$se[1]))
  } else {
    stat_out = list(est = unname(static$coefficients), se = unname(static$se[1]))
  }
  
  dyndat = relabel_control_0(data)
  dynamic = suppressMessages(did2s::event_study(dyndat, "y", "unit", "group", 
                                                "period", estimator = "TWFE"))
  # dynamic = suppressMessages(
  #   fixest::feols(form_dyn, es_data))
  # aggregate to ATT dyn = aggregate(dynamic, c("ATT" = "rel_period::[^-]"))
  
  # only keep 100 coefficients to make dynamic estimate comparable
  dyn_est = dynamic$estimate[(length(dynamic$estimate)-99):length(dynamic$estimate)]
  dyn_se = dynamic$std.error[(length(dynamic$std.error)-99):length(dynamic$std.error)]
  
  dyn_out = list(cum_est = mean(dyn_est), 
                 cum_se = mean(dyn_se))
  
  # dyn_out = list(cum_est = sum(dynamic$coefficients[-length(dynamic$coefficients)]), 
  #                cum_se = sum(dynamic$se[-length(dynamic$se)]))
  out = c(stat_out, dyn_out)
  out$iter = iteration
  out$estimator = "DiD"
  
  return(out)
}

## Callaway-Sant'Anna
est_cs <- function(es_data, iteration = 0){
  # check whether we should use covariates in estimation
  use_covariates = as.logical(es_data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    mod = suppressWarnings(did::att_gt(yname = "y",
                                       tname = "period",
                                       idname = "unit",
                                       gname = "group",
                                       xformla = ~ 0 + nuisance,
                                       est_method = "dr",
                                       control_group = "notyettreated",
                                       bstrap = F,
                                       data = es_data,
                                       print_details = F))
  } else {
    mod = did::att_gt(yname = "y",
                      tname = "period",
                      idname = "unit",
                      gname = "group",
                      control_group = "notyettreated",
                      bstrap = F,
                      data = es_data,
                      print_details = F)
  }
  
  
  dynamic = did::aggte(mod, type = "calendar", cband = F)
  
  dyn_est = c(dynamic$att.egt, rep(0, 100 - length(dynamic$att.egt)))
  dyn_se = c(dynamic$se.egt, rep(0, 100 - length(dynamic$se.egt)))
  
  cs_output = list(est = dynamic$overall.att, se = dynamic$overall.se, 
                   cum_est = mean(dyn_est), 
                   cum_se = mean(dyn_se), 
                   iter = iteration, estimator = "CS")
  
  return(cs_output)
}

## Sun and Abraham using fixest
est_sa <- function(es_data, iteration = 0){
  # check whether we should use covariates in estimation
  use_covariates = as.logical(es_data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    form = y ~ nuisance + sunab(group, period) | unit + period
  } else {
    form = y ~ sunab(group, period) | unit + period
  }
  
  mod = fixest::feols(form, es_data)
  static = aggregate(mod, agg = "att")
  dyn = aggregate(mod, agg = "period")
  
  de = dyn[1:(nrow(dyn)), 1] # get point estimates
  dse = dyn[1:(nrow(dyn)), 2] # get SE
  
  if (length(de) < 100){ # make sure that only 100 periods
    dyn_est = c(de, rep(0, 100 - length(de)))
    dyn_se =  c(dse, rep(0, 100 - length(dse)))
  } else {
    dyn_est = de[(length(de)-99):length(de)]
    dyn_se = dse[(length(dse)-99):length(dse)]
  }
  
  sa_output = list(est = static[1], se = static[2],
                   cum_est = mean(de), 
                   cum_se = mean(dse), 
                   iter = iteration, estimator = "SA")
  
  return(sa_output)
}

## de Chaisemartin & D’Haultfœuille using DIDmultiplegt
est_dcdh <- function(data, iteration = 0){
  # check whether we should use covariates in estimation
  use_covariates = as.logical(data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    control = "nuisance"
  } else {
    control = c()
  }
  
  mod = DIDmultiplegt::did_multiplegt(
      data, "y", "group", "period", controls = control,
      "treat", dynamic = 0, average_effect = "simple")
  
  dcdh_output = list(est = mod$effect, se = NA, cum_est = NA, cum_se = NA, 
                     iter = iteration, estimator = "dCdH")
  
  return(dcdh_output)
}

## Borusyak Jaravel Spiess using didimputation
est_bjs <- function(data, iteration = 0){
  # change data because didimputation does not work when depvar is called "y"
  imput_dat = relabel_control_0(data) %>% 
    rename(dep_var = y) %>% as.data.table()
  
  # check whether we should use covariates in estimation
  use_covariates = as.logical(imput_dat$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    fs = ~ nuisance | unit + period
  } else {
    fs = ~ 0 | unit + period
  }
  
  stat = didimputation::did_imputation(
    data = imput_dat, yname = "dep_var", gname = "group", first_stage = fs,
    tname = "period", idname = "unit") #tname = "period", idname = "unit", 
  
  dyn = didimputation::did_imputation(
    data = imput_dat, yname = "dep_var", gname = "group", 
    tname = "period", idname = "unit", first_stage = fs,
    horizon = T)
  
  dyn_est = c(dyn$estimate, rep(0, 100 - length(dyn$estimate))) #dyn$std.error
  dyn_se = c(dyn$std.error, rep(0, 100 - length(dyn$std.error)))
  
  bjs_output = list(est = stat$estimate, se = stat$std.error,
                    cum_est = mean(dyn_est), 
                    cum_se = mean(dyn_se),
                    iter = iteration, estimator = "BJS")
  
  return(bjs_output)
}

timer <- function(func, ...){
  start_time = Sys.time()  # Record the start time
  do.call(func, list(...)) # Run the function
  end_time = Sys.time() # Record the end time
  cat("Execution Time:", round((end_time - start_time), digits = 2), "seconds\n")
}

### Functions to run simulations

## Function to compare estimators in an iteration
run_sim <- function(i, fun, quiet = T) {
  # print progress
  if (quiet == F) {
    cat(" ", i, "...")
  }
  # make data from function
  dt = withr::with_seed(i, fun()) # use with_seed to guarantee reproducibility
  # make event study data
  es = prep_es(dt)
  
  # check if DGP is 8
  if (identical(fun, dgp_8_sim)) {
    dgp8 = T
  } else {
    dgp8 = F
  }
  #true values
  true = est_true(dt, i)
  
  #estimate
  if(dgp8 ==T ){
    # give MC-NNM more CV folds and candidate lambdas for DGP 8
    mc = est_mc(dt, i, k = 4, n_lam = 6) 
  } else {
    mc = est_mc(dt, i)  
  }
  
  did = est_canonical(dt, i)
  cs = est_cs(es, i)
  sa = est_sa(es, i)
  dcdh = est_dcdh(dt, i)
  bjs = est_bjs(dt, i)
  
  results_list <- list(true, mc, did, cs, sa, dcdh, bjs)
  names(results_list) <- c('TRUE', 'MC-NNM', 'DiD', 'CS', 'SA', 'dCdH', 'BJS')
  
  # convert list to long tibble
  results_df <- tibble(
    iteration = i,
    estimator = names(results_list),
    est = vapply(results_list, function(x) x$est, numeric(1)),
    se = vapply(results_list, function(x) x$se, numeric(1)),
    cum_est = vapply(results_list, function(x) x$cum_est, numeric(1)),
    cum_se = vapply(results_list, function(x) x$cum_se, numeric(1))
  )
  
  return(results_df)
}

## Function to execute the simulation, not parallelized
run_sim_map <- function(iterations, sim_function) {
  cat("Simulating ", deparse(substitute(sim_function)), ", ",
      length(iterations), "Iterations\n", "Iteration: ")
  # Use purrr:map() to run simulations 
  start_time = Sys.time()
  out_list <- purrr::map(iterations, ~run_sim(.x, sim_function, quiet = F))
  
  # Combine the list of data frames into a single data frame
  out_df <- dplyr::bind_rows(out_list)
  end_time = Sys.time()
  cat("\nElapsed time: ", round(end_time - start_time, digits = 1), "\n")
  cat("Simulation of  ", deparse(substitute(sim_function)), " complete \n")
  
  # Return the combined data frame
  return(out_df)
}


## Function to execute the simulation, parallelized using mclapply
run_sim_parallel <- function(iterations, sim_function) {
  cat("Simulating ", deparse(substitute(sim_function)), ", ",
      length(iterations), "Iterations:\n")
  # Use mclapply() to run simulations in parallel
  start_time = Sys.time()
  out = suppressWarnings(
    mclapply(
      iterations, function(i) run_sim(i, sim_function, quiet = F), mc.cores = globalcores)
  )
  
  # Bind all the output data frames into a single data frame
  out_df <- do.call(rbind, out)
    end_time = Sys.time()
    cat("Elapsed time: ", round(end_time - start_time, digits = 1), "\n")
    cat("Simulation of  ", deparse(substitute(sim_function)), " complete \n")
  # Return the combined data frame
  return(tibble(out_df))
}

### Utilities for simulating
# parallelization leads to errors, which leads to entire iterations missing
# this helper counts and removes those rows
verify_sim_results <- function(input_tibble) {
  error_count <- sum(grepl("error|Error", input_tibble$iteration)) 
  
  clean_tibble <- input_tibble[!(grepl("error|Error", input_tibble$iteration, ignore.case = TRUE)), ]
  
  cat("Number of corrupted rows:", error_count, "\n")
  return(clean_tibble)
}

verify_iteration_counts <- function(input_tibble) {
  iteration_counts <- table(input_tibble$iteration)
  incorrect_iterations <- iteration_counts[iteration_counts != 7]
  
  if (length(incorrect_iterations) == 0) {
    cat("All iterations appear exactly 7 times.\n")
  } else {
    cat("Incorrect iteration counts:\n")
    print(incorrect_iterations)
  }
}

# function to save sim results to RDS
save_sim_results <- function(input_tibble, file_name = "test") {
  # get the current date and time
  current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  
  # create the file name with the custom name and current date and time
  full_file_name <- paste0(file_name, "_", current_time, ".rds")
  
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
load_sim_results <- function(file_name = "test") {
  # specify directory 
  directory <- 'SimResults'
  
  # use a pattern match to filter 
  pattern <- paste0('^', file_name, '.*\\.rds$')
  
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
  loaded_data$iteration = as.numeric(loaded_data$iteration)
  loaded_data$est = as.numeric(loaded_data$est)
  loaded_data$se = as.numeric(loaded_data$se)
  loaded_data$cum_est = as.numeric(loaded_data$cum_est)
  loaded_data$cum_se = as.numeric(loaded_data$cum_se)
  
  cat("Loaded file:", sorted_files[1], "\n") # print confirmation
  
  # return the loaded data
  return(loaded_data)
}

# function to load all results and trim to 500 iterations
load_all_results <- function(){
  writeLines("Loading saved results")
  sim1 <<- keep_iterations(load_sim_results("Sim_1"), 500)
  sim2 <<- keep_iterations(load_sim_results("Sim_2"), 500)
  sim3 <<- keep_iterations(load_sim_results("Sim_3"), 500)
  sim4 <<- keep_iterations(load_sim_results("Sim_4"), 500)
  sim5 <<- keep_iterations(load_sim_results("Sim_5"), 500)
  sim6 <<- keep_iterations(load_sim_results("Sim_6"), 500)
  sim7 <<- keep_iterations(load_sim_results("Sim_7"), 500)
  sim8 <<- keep_iterations(load_sim_results("Sim_8"), 500)
  writeLines("Results loaded succesfully")
  
}


# function to trim excess iterations
keep_iterations <- function(sim_data, num_iterations = 500) {
  
  if (length(unique(sim_data$iteration)) == num_iterations) {
    out = sim_data
  } else if(length(unique(sim_data$iteration)) > num_iterations){
    it = rep(1:500, each = 7, length.out = 7 * num_iterations)
    out = sim_data[sim_data$iteration %in% it, ]
  } else {
    out = "Not enough complete iterations present"
  }
  
  return(out)
}



### Analyzing sim results

# function to create summary tibble
analyze_sim_results <- function(sim_results, type = "static") {
  
  # keep original order
  sim_results$estimator = factor(sim_results$estimator, 
                                 levels = unique(sim_results$estimator))
  

  
  # get true values for bias
  true_values = filter(sim_results, estimator == "TRUE")
  
  # create 'TRUE' row for summary table
  true_summary_static = true_values %>%
    summarise(
      estimator = "TRUE",
      min_est  = min(est),
      mean_est = mean(est, na.rm = F),
      max_est = max(est),
      sd_est = sd(est),
      mean_se = NA,
      bias = NA,
      rmse = NA,
      .groups = 'drop'
    )
  
  true_summary_dynamic = true_values %>%
    summarise(
      estimator = "TRUE",
      min_est  = min(cum_est),
      mean_est = mean(cum_est, na.rm = F),
      max_est = max(cum_est),
      sd_est = sd(cum_est),
      mean_se = NA,
      bias = NA,
      rmse = NA,
      .groups = 'drop'
    )
  
  static_summary = sim_results %>% # summary results for static-type estimate
    filter(estimator != "TRUE") %>%
    group_by(estimator) %>%
    summarise(
      min_est  = min(est),
      mean_est = mean(est, na.rm = F),
      max_est = max(est),
      sd_est = sd(est),
      mean_se = mean(se, na.rm = F),
      bias = mean(est) - mean(true_values$est, na.rm = F),
      rmse = sqrt(mean((est - mean(true_values$est, 
                                       na.rm = F))^2, na.rm = F)),
      .groups = 'drop'  # avoid the grouped_df class for output
    )
  
  dynamic_summary = sim_results %>% # summary results for dynamic-type estimate
    filter(estimator != "TRUE") %>% filter(estimator != "dCdH") %>% 
    group_by(estimator) %>%
    summarise(
      min_est  = min(cum_est),
      mean_est = mean(cum_est, na.rm = F),
      max_est = max(cum_est),
      sd_est = sd(cum_est),
      mean_se = mean(cum_se, na.rm = F),
      bias = mean(cum_est) - mean(true_values$cum_est, na.rm = F),
      rmse = sqrt(mean((cum_est - mean(true_values$cum_est, 
                                       na.rm = F))^2, na.rm = F)),
      .groups = 'drop'  # avoid the grouped_df class for output
    )
  
  
  if (type == "static") {
    final_summary = bind_rows(true_summary_static, static_summary) %>% 
      select("Estimator" = estimator, 
             "Min" = min_est, "Mean" = mean_est, "Max" = max_est, "SD" = sd_est,
             "Mean SE" = mean_se, "Bias" = bias, "RMSE" = rmse)
  } else if (type == "dynamic"){
    final_summary = bind_rows(true_summary_dynamic, dynamic_summary) %>% 
      select("Estimator" = estimator, 
             "Min" = min_est, "Mean" = mean_est, "Max" = max_est, "SD" = sd_est,
             "Mean SE" = mean_se, Bias = bias, "RMSE" = rmse)
  }
  
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
    save_kable(file = file_name)
}

### Data Plotting Functions
# Set Theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# Helper function to get unique treatment start times
get_treat_times <- function(df) {
  unique(df$group)
}

# Basic plotting function
dgp_plot_basic <- function(df) {
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
    ggtitle("Outcome Data from Simulation") +
    theme(plot.title = element_text(hjust = 0.5, size = 12))
}


# More elaborate plotting function
dgp_plot <- function(df, subtitle = "", sim_num = NULL){
  # Define your color palette
  my_palette = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F")
  
  suppressWarnings({
    # get treatment times and remove the control group
    treat_times <- get_treat_times(df)
    treat_times <- treat_times[treat_times != max(treat_times)]
    
    # relabel the control group
    control_label <- as.character(max(df$period)) # max period considered as control group
    
    if (is.null(sim_num)) {
      # extract the number from the df function name
      sim_num = as.integer(gsub("[^0-9]", "", substitute(df)))
    }
    # set as plot title
    plot_title = paste("Outcome Data from One Draw of Simulation", sim_num)
    
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
      labs(subtitle = subtitle) +
      scale_x_continuous(breaks = seq(0, 100, 20))  # ticks at 20, 40, 60, 80, 100
    
    # Add vertical lines for each treatment group
    vlines <- data.frame(xintercept = treat_times, group = treat_times)
    p = p + geom_vline(data = vlines, aes(xintercept=xintercept, color=factor(group)), 
                        size = 0.5, show.legend = FALSE)
    
    # Set color scale and replace treatment group 100 with "Control" in legend
    p = p + scale_color_manual(values = my_palette,
                                labels = function(x) ifelse(x == control_label, "Control", x))
    
    p
  })
}

# function to plot densities of estimates of each estimator
plot_est_dens <- function(df, dynamic = F) {
  # get estimator names
  estimators = c("MC-NNM", "DiD", "CS", "SA", "dCdH", "BJS")
  
  # get true mean
  if (dynamic == T) {
    true_v = df %>% filter(estimator == "TRUE") %>% 
      summarize(mean_e = mean(cum_est), min_e = min(cum_est),
                max_e = max(cum_est))
    title = "Distributions of Event-Study Parameter Estimates"
    
    # filter out dCdH estimator if dynamic is TRUE 
    if(all(is.na(df$cum_est[df$estimator == "dCdH"]))) {
      estimators = estimators[estimators != "dCdH"]
    }
  } else {
    true_v = df %>% filter(estimator == "TRUE") %>% 
      summarize(mean_e = mean(est), min_e = min(est), max_e = max(est))
    title = "Distributions of ATET Estimates"
  }
  
  # filter data
  df = df %>% filter(estimator %in% estimators)
  
  # Create plot variable depending on 'dynamic' argument
  plot_var = ifelse(dynamic, "cum_est", "est")
  
  # Set factor levels for estimator to control facet order
  df$estimator = factor(df$estimator, levels = estimators)
  
  # Order the dataframe by estimator
  df = df %>% arrange(estimator)
  
  # Assign colors
  my_palette = c("MC-NNM" = "#4E79A7", "DiD" = "#F28E2B", "CS" = "#E15759", 
                  "SA" = "#76B7B2", "dCdH" = "#59A14F", "BJS" = "#EDC948")
  
  # Create the plot
  p = ggplot(df, aes(x = get(plot_var), color = factor(estimator))) +
    geom_density(fill = "grey", alpha = 0.5) +  # fill color is grey
    scale_color_manual(values = my_palette) +
    geom_vline(aes(xintercept = true_v$min_e), 
               linetype = "dashed", color = "red", alpha = 0.35) +
    geom_vline(aes(xintercept = true_v$mean_e), 
               linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = true_v$max_e), 
               linetype = "dashed", color = "red", alpha = 0.35) +
    labs(x = "Point Estimate", y = "Density") +
    guides(color = "none", fill = "none") +
    facet_wrap(~ estimator, scales = "free") + 
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 9),
          plot.title = element_text(hjust = 0.5, size=12),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_text(size = 12, hjust = 0.5)) +
    ggtitle(title)
  
  return(p)
}

## function to combine dgp plot and desnities and save
plot_combined <- function(dgp_number, dynamic, save = F) {
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
  plot_est_dens = plot_est_dens(sim_data, dynamic)
  
  # combine and arrange plots
  plot_combined <- grid.arrange(dgp_plot, plot_est_dens, ncol = 1)
 
  if (save ==T & Sys.info()[7] == "ts") {
    fp = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Figures"
    filename = paste0("Sim_", dgp_number, ".png")
    ggsave(filename, plot = plot_combined, path = fp,
           width = 16, height = 19, units = "cm")
  }
}


writeLines("Ready")
