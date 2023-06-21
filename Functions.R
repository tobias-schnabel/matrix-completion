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
      y = unit_fe + period_fe + cum.t.eff + error + nuisance,
      use_cum_te = T,
      use_cov = T) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, nuisance, everything())
  
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
      summarise(cum_est = sum(mce))
  } else {
    ce = data %>%  group_by(period) %>% 
      filter(group %in% tgroups) %>% 
      filter(treat == 1) %>% 
      summarise(mce = mean(t.eff)) %>% 
      summarise(cum_est = sum(mce))
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
  est_eff_calendar = model$est.eff.calendar #get dynamic effect
  mc_output = list(est = est_avg[1], se = est_avg[2],
                   cum_est = sum(est_eff_calendar[,1], na.rm = TRUE), 
                   cum_se = sum(est_eff_calendar[,2], na.rm = TRUE),
                   iter = iteration, estimator = "MC-NNM")
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

## Canonical DiD, using fixest:::feols
est_canonical <- function(es_data, iteration = 0){
  # check whether we should use covariates in estimation
  use_covariates = as.logical(es_data$use_cov[1]) 
  # adjust estimation formula to include covariates for dgp 7
  if (use_covariates) {
    form = y ~ treat + nuisance | unit + period
    form_dyn = y ~ i(rel_period, ref=c(-1, Inf)) + nuisance | unit + period
  } else {
    form = y ~ treat | unit + period
    form_dyn = y ~ i(rel_period, ref=c(-1, Inf)) | unit + period
  }
  
  static = fixest::feols(form , cluster = ~obsgroup, data = es_data)
  if (use_covariates) {
    stat_out = list(est = unname(static$coefficients)[1], se = unname(static$se[1]))
  } else {
    stat_out = list(est = unname(static$coefficients), se = unname(static$se[1]))
  }
  
  dynamic = suppressMessages(
    fixest::feols(form_dyn, es_data))
  # aggregate to ATT dyn = aggregate(dynamic, c("ATT" = "rel_period::[^-]"))
  
  dyn_out = list(cum_est = sum(dynamic$coefficients[-length(dynamic$coefficients)]), 
                 cum_se = sum(dynamic$se[-length(dynamic$se)]))
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
  cs_output = list(est = dynamic$overall.att, se = dynamic$overall.se, 
                   cum_est = sum(dynamic$att.egt[-length(dynamic$att.egt)]), 
                   cum_se = sum(dynamic$se.egt[-length(dynamic$se.egt)]), 
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
  sa_output = list(est = static[1], se = static[2],
                   cum_est = sum(dyn[1:(nrow(dyn)-1),1], na.rm = TRUE), 
                   cum_se = sum(dyn[1:(nrow(dyn)-1),2], na.rm = TRUE), 
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
  
  bjs_output = list(est = stat$estimate, se = stat$std.error,
                    cum_est = sum(dyn[1:(nrow(dyn)-1),3]), 
                    cum_se = sum(dyn[1:(nrow(dyn)-1),4]),
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
  dt = fun()
  # make event study data
  es = prep_es(dt)
  
  #true values
  true = est_true(dt, i)
  #estimate
  mc = est_mc(dt, i)
  did = est_canonical(es, i)
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
  cat("Elapsed time: ", round(end_time - start_time, digits = 1), "\n")
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
  
  # sort file names in descending order (most recent first)
  sorted_files <- files[order(files, decreasing = TRUE)]
  
  # construct full path 
  most_recent_file <- file.path(directory, sorted_files[1])
  
  loaded_data <- readRDS(most_recent_file) # load most recent file
  
  cat("Loaded file:", sorted_files[1], "\n") # print confirmation
  
  # return the loaded data
  return(loaded_data)
}

### Analyzing sim results

# function to create summary tibble
analyze_sim_results <- function(results, type = "static") {
  
  sim_results = results
  # make sure numeric values are numeric
  sim_results$iteration = as.numeric(results$iteration)
  sim_results$est = as.numeric(results$est)
  sim_results$se = as.numeric(results$se)
  sim_results$cum_est = as.numeric(results$cum_est)
  sim_results$cum_se = as.numeric(results$cum_se)
  
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
      mean_se = 0,
      bias = 0,
      rmse = 0,
      .groups = 'drop'
    )
  
  true_summary_dynamic = true_values %>%
    summarise(
      estimator = "TRUE",
      min_est  = min(cum_est),
      mean_est = mean(cum_est, na.rm = F),
      max_est = max(cum_est),
      mean_se = 0,
      bias = 0,
      rmse = 0,
      .groups = 'drop'
    )
  
  static_summary = sim_results %>% # summary results for static-type estimate
    filter(estimator != "TRUE") %>%
    group_by(estimator) %>%
    summarise(
      min_est  = min(est),
      mean_est = mean(est, na.rm = F),
      max_est = max(est),
      mean_se = mean(se, na.rm = F),
      bias = mean(est) - mean(true_values$est, na.rm = F),
      rmse = sqrt(mean((est - mean(true_values$est, 
                                       na.rm = F))^2, na.rm = F)),
      .groups = 'drop'  # avoid the grouped_df class for the output
    )
  
  dynamic_summary = sim_results %>% # summary results for dynamic-type estimate
    filter(estimator != "TRUE") %>%
    group_by(estimator) %>%
    summarise(
      min_est  = min(cum_est),
      mean_est = mean(cum_est, na.rm = F),
      max_est = max(cum_est),
      mean_se = mean(cum_se, na.rm = F),
      bias = mean(est) - mean(true_values$cum_est, na.rm = F),
      rmse = sqrt(mean((cum_est - mean(true_values$cum_est, 
                                       na.rm = F))^2, na.rm = F)),
      .groups = 'drop'  # avoid the grouped_df class for the output
    )
  
  if (type == "static") {
    final_summary = bind_rows(true_summary_static, static_summary) %>% 
      select("Estimator" = estimator, 
             "Min" = min_est, "Mean" = mean_est, "Max" = max_est,
             "Mean SE" = mean_se, "Bias" = bias, "RMSE" = rmse)
  } else if (type == "dynamic"){
    final_summary = bind_rows(true_summary_dynamic, dynamic_summary) %>% 
      select("Estimator" = estimator, 
             "Min" = min_est, "Mean" = mean_est, "Max" = max_est,
             "Mean SE" = mean_se, Bias = bias, "RMSE" = rmse)
  }
  
  return(final_summary)
}

# Function to combine static and dynamic summaries into one large table
summarize_sim_results <- function(results,
                                  caption = "", 
                                  note = "", 
                                  file_name = "table.tex"){
  stat = analyze_sim_results(results, "static") 
  dyn = analyze_sim_results(results, "dynamic")
  
  s = cbind(stat, dyn[,-1])
  
  # static_estimates <- s[, c("Min", "Mean", "Max", "MeanSE", "Bias", "RMSE")]
  # dynamic_estimates <- s[, c("Min.1", "Mean.1", "Max.1", "MeanSE.1", "Bias.1", "RMSE.1")]
  align <- ifelse(sapply(data, is.numeric), "c", "l")
  
  s %>%  kable(format = "latex", booktabs = T, caption = caption,
               digits = 2, align = align) %>% 
    add_header_above(c(" ", "Static Estimates"= 6, "Dynamic Estimates"= 6)) %>% 
    kable_styling(latex_options = "hold_position") #%>% 
    # save_kable(file = file_name)
  
  # return(summary)
}

# function to save results tables to latex
save_table_results <- function(sumdata, 
                               caption = "", 
                               note = "", 
                               file_name = "table.tex") {
  align <- ifelse(sapply(sumdata, is.numeric), "c", "l")
  
  sumdata %>%
    kable(format = "latex", booktabs = T, caption = caption,
          digits = 2, align = align) %>%
    footnote(general = note) %>%
    kable_styling(latex_options = c("hold_position")) %>% #"striped",
    save_kable(file = file_name)
}

writeLines("Ready")
