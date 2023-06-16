#### This script holds all custom functions
writeLines("Loading custom functions...")
# Create Pure Functions dependent on RNG generation to guarantee reproducibility
pure_rnorm <- function(..., seed=globalseed){
  with_seed(seed, rnorm(...))
}

pure_sample <- function(..., seed=globalseed){
  with_seed(seed, sample(...))
}


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
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything()) %>% 
    ungroup()
}

## DGP 7  Multiple Treatment Groups, Time-Varying Heterogeneous TE, NO PARALLEL TRENDS
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
      y = unit_fe + period_fe + cum.t.eff + error + nuisance
    ) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything()) %>% 
    select(-nuisance)
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
dgp_plot <- function(df, subtitle = ""){
  # Define your color palette
  my_palette <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F")
  
  suppressWarnings({
    # get treatment times and remove the control group
    treat_times <- get_treat_times(df)
    treat_times <- treat_times[treat_times != max(treat_times)]
    
    # relabel the control group
    control_label <- as.character(max(df$period)) # max period considered as control group
    
    p <- df %>% 
      ggplot(aes(x = period, y = y, group = unit)) + 
      geom_line(alpha = .1, color = "grey") + 
      geom_line(data = df %>% 
                  group_by(group, period) %>% 
                  summarize(dep_var = mean(y), .groups = "drop"),
                aes(x = period, y = dep_var, group = factor(group),
                    color = factor(group)),
                size = 0.5) + 
      labs(x = "Period", y = "y", color = "Treatment Groups") + 
      theme(legend.position = 'bottom',
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size=12),
            plot.subtitle = element_text(hjust = 0.5),
            legend.title = element_text(size = 12, hjust = 0.5)) +
      guides(color = guide_legend(title.position = "top")) +
      ggtitle("Outcome Data from Simulation") +
      labs(subtitle = subtitle)
    
    # Add vertical lines for each treatment group
    vlines <- data.frame(xintercept = treat_times, group = treat_times)
    p <- p + geom_vline(data = vlines, aes(xintercept=xintercept, color=factor(group)), 
                        size = 0.5, show.legend = FALSE)
    
    # Set color scale and replace treatment group 100 with "Control" in legend
    p <- p + scale_color_manual(values = my_palette,
                                labels = function(x) ifelse(x == control_label, "Control", x))
    
    p
  })
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
  df %>% mutate(group = ifelse(group == 100, 0, group))
}

# set number of cores
globalcores = parallel::detectCores()


## MC-NNM using fect (gsynth fails when no covariates for some reason)
# helper
get_cohorts <- function(data) {
  df = data %>% select(unit, period, y, treat, group) %>% as.data.frame()
  cohorts = get.cohort(df, "treat", index = c("unit", "period"))
  cohorts$FirstTreat[cohorts$Cohort == "Cohort:100"] = NA
  cohorts$Time_to_Treatment[cohorts$Cohort == "Cohort:100"] = NA
  cohorts$Cohort[cohorts$Cohort == "Cohort:100"] = "Control"
  
  return(cohorts)
}

# Estimate Full object
est_mc <- function(data, iteration = 0, b_iter = 100, k = 2, n_lam = 4){
  cohorts = get_cohorts(data)
  model = suppressMessages(fect::fect(y ~ treat, data = cohorts, 
                                      method = "mc", index = c("unit","period"), 
                                      se = T, force = "two-way", r = 0, 
                                      CV = T,  cores = globalcores, parallel = T,
                                      nboots = b_iter, group = 'Cohort',
                                      nlambda = n_lam, k = k))
  
  est_avg = unname(model$est.avg[,1:2]) #get static effect
  est_eff_calendar = model$est.eff.calendar #get dynamic effect
  mc_output = list(est = est_avg[1], se = est_avg[2],
                   cum_est = sum(est_eff_calendar[,1], na.rm = TRUE), 
                   cum_se = sum(est_eff_calendar[,2], na.rm = TRUE),
                   iter = iteration, estimator = "MC-NNM")
  return(mc_output)
}

# (deprecated)
# Point estimate only (no bootstrap, takes around 6 seconds) 
est_mc_point <- function(data, iteration = 0){
  dt = data %>% select(unit, period, y, treat) %>% setDT(.)
  model = fect::fect(y ~ treat, data = dt, 
                     method = "mc", index = c("unit","period"), 
                     se = F, force = "two-way", r = 0, 
                     CV = T,  cores = globalcores,
                     nlambda = 4, k = 2)
  
  mc_output = list(est = model$att.avg, iter = iteration, lambda = model$lambda.cv)
  return(mc_output)
}

# (deprecated)
# Estimates Bootstrap SE (takes around 45 seconds each run) 
est_mc_se <- function(data, boot_iter = 1000, lambda){
  dt = data %>% select(unit, period, y, treat) %>% setDT(.)
  model = fect::fect(y ~ treat, data = dt, 
               method = "mc", index = c("unit","period"), 
               se = T, nboots = boot_iter, r = 0, 
               CV = F, force = "two-way", lambda = lambda,
               parallel = TRUE, cores = globalcores,
               nlambda = 4, k = 2)
  
  out = unname(model$est.avg[,1:2])
  mc_output = list(se = out[2])
  return(mc_output)
}

### Helpers to set up event study

prep_es <- function(data){
  # Prepare data
  esdat <- data %>%
    mutate(rel_period = period - group) %>% # relative time to treatment
    mutate(evertreated = ifelse(group == get_num_periods(data), 0, 1)) %>% 
    mutate(group = ifelse(group == 100, Inf, group)) %>% #mark control
    dplyr::arrange(group, unit, period) %>% 
    ungroup()
}

prep_es_dt <- function(data){ #same as above but return data.table object
  # Prepare data
  esdat <- data %>%
    mutate(rel_period = period - group) %>% # relative time to treatment
    mutate(evertreated = ifelse(group == get_num_periods(data), 0, 1)) %>% 
    mutate(group = ifelse(group == 100, Inf, group)) %>% #mark control
    dplyr::arrange(group, unit, period) %>% 
    ungroup() %>% setDT(.)
}

## Canonical DiD, using fixest:::feols
est_canonical <- function(es_data, iteration = 0){
  static = fixest::feols(y ~ treat | unit + period, cluster = ~obsgroup, data = es_data)
  tidy_model = broom::tidy(static, conf.int = F) %>% mutate(iter = iteration)
  tidy_model$se = tidy_model$std.error
  static_out = list(est = tidy_model$estimate, se = tidy_model$se)
  
  dynamic = suppressMessages(
    fixest::feols(y ~ i(rel_period, ref=c(-1, Inf)) | unit + period, es_data))
  # aggregate to ATT dyn = aggregate(dynamic, c("ATT" = "rel_period::[^-]"))
  
  dyn_out = list(cum_est = sum(dynamic$coefficients[-length(dynamic$coefficients)]), 
                 cum_se = sum(dynamic$se[-length(dynamic$se)]))
  out = c(static_out, dyn_out)
  out$iter = iteration
  out$estimator = "DiD"
  
  return(out)
}

## Callaway-Sant'Anna
est_cs <- function(es_data, iteration = 0){
  mod = did::att_gt(yname = "y",
                    tname = "period",
                    idname = "unit",
                    gname = "group",
                    control_group = "nevertreated",
                    bstrap = F,
                    data = es_data,
                    print_details = F)
  
  dynamic = did::aggte(mod, type = "calendar", cband = F)
  cs_output = list(est = dynamic$overall.att, se = dynamic$overall.se, 
                   cum_est = sum(dynamic$att.egt[-length(dynamic$att.egt)]), 
                   cum_se = sum(dynamic$se.egt[-length(dynamic$se.egt)]), 
                   iter = iteration, estimator = "CS")
  return(cs_output)
}

## Sun and Abraham using fixest
est_sa <- function(es_data, iteration = 0){
  mod = fixest::feols(y ~ sunab(group, period) | unit + period, es_data)
  static = aggregate(mod, agg = "att")
  dyn = aggregate(mod, agg = "period")
  sa_output = list(est = static[1], se = static[2],
                   cum_est = sum(dyn[1:(nrow(dyn)-1),1], na.rm = TRUE), 
                   cum_se = sum(dyn[1:(nrow(dyn)-1),2], na.rm = TRUE), 
                   iter = iteration, estimator = "SA")
  
  return(sa_output)
}

## de Chaisemartin & D’Haultfœuille using DIDmultiplegt
est_dcdh <- function(data, iteration = 0){
  mod = DIDmultiplegt::did_multiplegt(
      data, "y", "group", "period", 
      "treat", dynamic = 0, average_effect = "simple")
  
  dcdh_output = list(est = mod$effect, se = NA, cum_est = NA, cum_se = NA, 
                     iter = iteration, estimator = "dCdH")
  
  return(dcdh_output)
}

## Borusyak Jaravek Spiess using didimputation
est_bjs <- function(data, iteration = 0){
  # change data because didimputation does not work when depvar is called "y"
  imput_dat = relabel_control_0(data) %>% 
    select(unit, period, dep_var = y, group) %>% as.data.table()
  
  stat = didimputation::did_imputation(
    data = imput_dat, yname = "dep_var", gname = "group", 
    tname = "period", idname = "unit")
  
  dyn = didimputation::did_imputation(
    data = imput_dat, yname = "dep_var", gname = "group", 
    tname = "period", idname = "unit", horizon = T)
  
  bjs_output = list(est = stat$estimate, se = stat$std.error,
                    cum_est = sum(dyn[1:(nrow(dyn)-1),3]), 
                    cum_se = sum(dyn[1:(nrow(dyn)-1),4]),
                    iter = iteration, estimator = "BJS")
}

timer <- function(func, ...){
  start_time = Sys.time()  # Record the start time
  do.call(func, list(...)) # Run the function
  end_time = Sys.time() # Record the end time
  cat("Execution Time:", round((end_time - start_time), digits = 2), "seconds\n")
}

estimate <- function(iter_range = 1:5, dgp, ...){
  dat = dgp(...)
  est_canonical(dat)
}

### Functions to run simulations
run_all_estimators <- function(iteration, dgp, ...) {
  dat_i <- dgp(...)
  es_data_i <- prep_es(dat_i)
  # es_data_dt <- prep_es_dt(dat_i)
  
  est_mc_out <- est_mc(dat_i, iteration)
  est_canonical_out <- est_canonical(es_data_i, iteration)
  est_cs_out <- est_cs(es_data_i, iteration)
  est_sa_out <- est_sa(es_data_i, iteration)
  est_dcdh_out <- est_dcdh(dat_i, iteration)
  est_bjs_out <- est_bjs(dat_i, iteration)
  
  results_list <- list(est_mc_out, est_canonical_out, est_cs_out, est_sa_out, est_dcdh_out, est_bjs_out)
  names(results_list) <- c('est_mc', 'est_canonical', 'est_cs', 'est_sa', 'est_dcdh', 'est_bjs')
  
  # convert list to a data frame in long format
  results_df <- data.frame(
    iteration = iteration,
    estimator = names(results_list),
    est = sapply(results_list, function(x) x$est),
    se = sapply(results_list, function(x) x$se),
    cum_est = sapply(results_list, function(x) x$cum_est),
    cum_se = sapply(results_list, function(x) x$cum_se)
  )
  
  return(results_df)
}

run_sim <- function(i, fun) {
  # make data from function
  dt = fun()
  # make event study data
  es = prep_es(dt)
  
  #estimate
  mc = est_mc(dt, i)
  did = est_canonical(es, i)
  cs = est_cs(es, i)
  sa = est_sa(es, i)
  dcdh = est_dcdh(dt, i)
  bjs = est_bjs(dt, i)
  
  results_list <- list(mc, did, cs, sa, dcdh, bjs)
  names(results_list) <- c('MC-NNM', 'DiD', 'CS', 'SA', 'dCdH', 'BJS')
  
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

run_parallel_sim <- function(iterations, sim_function) {
  # Use mclapply() to run the simulations in parallel
  out = mclapply(
    iterations, function(i) run_sim(i, sim_function), mc.cores = globalcores)
  
  # Bind all the output data frames into a single data frame
  out_df <- do.call(rbind, out)
  
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
  incorrect_iterations <- iteration_counts[iteration_counts != 6]
  
  if (length(incorrect_iterations) == 0) {
    cat("All iterations appear exactly 6 times.\n")
  } else {
    cat("Incorrect iteration counts:\n")
    print(incorrect_iterations)
  }
}

save_sim_results <- function(input_tibble, file_name = dgp_1) {

  # get the current date and time
  current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  
  # create the file name with the custom name and current date and time
  full_file_name <- paste0(file_name, "_", current_time, ".RData")
  
  cwd = getwd() # get current WD
  setwd('SimResults') 
  save(input_tibble, file = full_file_name) # save tibble as RData file
  setwd(cwd) # reset current WD
  
  # print confirmation
  cat("Tibble saved as:", full_file_name, "\n")
}

writeLines("Ready")
