#### This script holds all custom functions

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
    unit_fe = pure_rnorm(nobs, 0, 0.5),
  )
  
  # Shuffle obsgroup and assign treatment status
  shuffled_groups <- pure_sample(unique(unit$obsgroup))
  half <- length(shuffled_groups) %/% 2
  
  unit <- unit %>%
    mutate(
      
      # gen treatment and control groups
      group = case_when(
        obsgroup %in% shuffled_groups[1:half] ~ 1,
        obsgroup %in% shuffled_groups[(half + 1):length(shuffled_groups)] ~ 2
      ),
      # Mark Control as never treated
      evertreated = ifelse(group == 2, 1, 0),
      
      # avg yearly treatment effects by group
      avg.te = case_when(
        group == 2 ~ 1,
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
    period_fe = pure_rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,0.5) error
    mutate(error = pure_rnorm(nrow(.), 0, 0.5)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(evertreated == 1 & period > treated.period, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + t.eff + error) %>% 
    # change column order
    select(unit, period, obsgroup, te, evertreated, everything())
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
    obsgroup = pure_sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = pure_rnorm(nobs, 0, 0.5),
    # gen treatment and control groups
    group = case_when(
      obsgroup %in% 1:(nobsgroups%/%2) ~ 1,
      obsgroup %in% (nobsgroups%/%2 + 1):nobsgroups ~ 2
    ),
    # Mark Control as never treated
    evertreated = ifelse(group == 2, 1, 0),
    # avg yearly treatment effects by group
    avg.te = case_when(
      group == 2 ~ .05,
      TRUE ~ 0
    )) %>%
    # gen unit-specific yearly treatment effects 
    rowwise() %>% 
    mutate(te = pure_rnorm(1, avg.te, .2)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = pure_rnorm(nperiods, 0, 1)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = pure_rnorm(nrow(.), 0, 1)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(evertreated == 1 & period > treated.period, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # add everything to get outcome
    group_by(unit) %>% 
    mutate(cum.t.eff = cumsum(t.eff)) %>% 
    mutate(y = unit_fe + period_fe + cum.t.eff + error) %>% 
    ungroup() %>% 
    # change column order
    select(unit, period, obsgroup, te, evertreated, treat, cum.t.eff, everything())
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
    obsgroup = pure_sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = pure_rnorm(nobs, 0, 0.5),
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
    mutate(te = pure_rnorm(1, avg.te, .2)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = pure_rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = pure_rnorm(nrow(.), 0, 1)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(period >= group, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + t.eff + error) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, everything())
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
    obsgroup = pure_sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = pure_rnorm(nobs, 0, 0.5),
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
    mutate(te = pure_rnorm(1, avg.te, .2)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = pure_rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = pure_rnorm(nrow(.), 0, 1)) %>% 
    # generate treatment dummy
    mutate(treat = ifelse(period >= group, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + t.eff + error) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, everything())
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
    obsgroup = pure_sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = pure_rnorm(nobs, 0, 0.5),
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
    mutate(te = pure_rnorm(1, avg.te, .2)) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = pure_rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = pure_rnorm(nrow(.), 0, 1)) %>% 
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
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything())
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
    obsgroup = pure_sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = pure_rnorm(nobs, 0, 0.5),
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
      group == treatgroups[1] ~ .005,
      group == treatgroups[2] ~ .025,
      group == treatgroups[3] ~ .075,
      group == treatgroups[4] ~ .2,
      TRUE ~ 0
    )) %>%
    # gen unit-specific yearly treatment effects 
    rowwise() %>% 
    mutate(te = pure_rnorm(1, avg.te, .2), 0) %>% 
    ungroup()
  
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    # Sample from Standard Normal
    period_fe = pure_rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = pure_rnorm(nrow(.), 0, 1)) %>% 
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
    select(unit, period, obsgroup, te, group, treat, cum.t.eff, everything())
}

## Diagnostics for simulations

dgp_verify <- function(df) {
  
  # Check number of observations
  num_obs <- nrow(df)
  cat("Number of observations:", num_obs, "\n")
  
  # Check number of periods
  num_periods <- length(unique(df$period))
  cat("Number of periods:", num_periods, "\n")
  
  # Check number of units
  num_units <- length(unique(df$unit))
  cat("Number of Units:", num_units, "\n")
  
  # Check number of nonempty observation groups
  num_nonempty_obs_groups <- length(unique(df$obsgroup))
  cat("Number of nonempty observation groups:", num_nonempty_obs_groups, "\n")
  
  # Check in how many different periods units change treatment status for the first time
  first_treatment_change <- df %>% 
    group_by(unit) %>% 
    mutate(treatment_change = lag(treat) == 0 & treat == 1) %>% 
    ungroup() %>% 
    filter(treatment_change) %>% 
    summarise(num_distinct_periods = n_distinct(period))
  
  cat("Number of distinct treatment adoption periods:", first_treatment_change$num_distinct_periods, "\n")
  
  # Check the percentage of units that are never treated
  never_treated <- df %>% 
    group_by(unit) %>% 
    summarise(treated = max(treat)) %>% 
    summarise(never_treated = mean(treated == 0)) %>% 
    pull(never_treated) * 100
  
  cat("Percentage of units never treated:", never_treated, "%", "\n")
  
  # Check if there is overlap between observation groups
  obsgroup_overlap <- df %>% 
    group_by(unit) %>% 
    summarise(n_obsgroups = n_distinct(obsgroup)) %>% 
    summarise(overlap = mean(n_obsgroups > 1)) %>% 
    pull(overlap) * 100
  
  cat("Individual units contained in multiple observation groups:", obsgroup_overlap, "%", "\n")
  
  # Check if there is overlap between treatment groups
  treatgroup_overlap <- df %>% 
    group_by(unit) %>% 
    summarise(n_treatgroups = n_distinct(group)) %>% 
    summarise(overlap = mean(n_treatgroups > 1)) %>% 
    pull(overlap) * 100
  
  cat("Individual units contained in multiple treatment groups:", treatgroup_overlap, "%", "\n")
  
  # Check if the treatment effect is the same across periods for each unit
  treatment_effect_column <- if ("cum.t.eff" %in% names(df)) "cum.t.eff" else "t.eff"
  
  varying_treatment_effect_units <- df %>%
    filter(!!sym(treatment_effect_column) != 0) %>%
    group_by(unit) %>%
    summarise(var_treat_effect = length(unique(!!sym(treatment_effect_column)))) %>%
    summarise(num = sum(var_treat_effect > 1)) %>%
    pull(num) * 100 / num_units
  
  cat("Percentage of units with varying treatment effects over time:", varying_treatment_effect_units, "%", "\n")
  
  # Check if treatment effect is the same across units
  unique_treatment_effects <- df %>%
    filter(treat == 1) %>%
    group_by(group) %>%
    summarise(n_distinct_te = n_distinct(t.eff)) %>%
    ungroup() %>%
    summarise(total = sum(n_distinct_te)) %>%
    pull(total)
  
  cat("Number of distinct treatment effects across all treatment groups:", unique_treatment_effects, "\n")
  
  # Return a list with all diagnostics
  invisible(list(num_obs = num_obs, 
                 num_periods = num_periods, 
                 num_nonempty_obs_groups = num_nonempty_obs_groups, 
                 first_treatment_change = first_treatment_change,
                 never_treated = never_treated,
                 obsgroup_overlap = obsgroup_overlap,
                 treatgroup_overlap = treatgroup_overlap,
                 unique_treatment_effects = unique_treatment_effects,
                 varying_treatment_effect_units = varying_treatment_effect_units))
}


### Data Plotting Functions
# Set Theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))
# Get colors
plotcol <- tableau_color_pal()(6)  

# Helper function to get unique treatment start times
get_treat_times <- function(df) {
  unique(df$group)
}


plotdgp_basic <- function(df){
  df %>% 
    ggplot(aes(x = period, y = y, group = unit)) + 
    geom_line(alpha = .1, color = "grey") + 
    geom_line(data = df %>% 
                group_by(group, period) %>% 
                summarize(dep_var = mean(y)),
              aes(x = period, y = dep_var, group = factor(group),
                  color = factor(group)),
              size = 0.5) + 
    labs(x = "", y = "Value", color = "Treatment group   ") + 
    theme(legend.position = 'bottom',
          #legend.title = element_blank(), 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))  +
    ggtitle("Outcome Data from Simulation")+
    theme(plot.title = element_text(hjust = 0.5, size=12))
}


dgp_plot <- function(df, subtitle = ""){
  # get treatment times and remove the control group
  treat_times <- get_treat_times(df)
  treat_times <- treat_times[treat_times != max(treat_times)]
  # relabel the control group
  control_label <- as.character(max(df$period)) # max period considered as control group
  
  # create a vector with the color palette
  color_vector <- tableau_color_pal()(length(unique(df$group)))
  
  # assign color to each group
  unique_groups <- sort(unique(df$group))
  names(color_vector) <- as.character(unique_groups)
  
  p <- df %>% 
    ggplot(aes(x = period, y = y, group = unit)) + 
    geom_line(alpha = .1, color = "grey") + 
    geom_line(data = df %>% 
                group_by(group, period) %>% 
                summarize(dep_var = mean(y), .groups = "drop"),
              aes(x = period, y = dep_var, group = factor(group),
                  color = factor(group)),
              size = 0.5) + 
    labs(x = "", y = "", color = "Treatment Groups") + 
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
  vlines <- data.frame(xintercept = treat_times, color = treat_times)
  p <- p + geom_vline(data = vlines, aes(xintercept=xintercept, color=factor(color)), 
                      size = 0.5, show.legend = F) +
  scale_color_manual(values = color_vector) +
  scale_color_discrete(labels = function(x) ifelse(x == control_label, "Control", x)) 
  
  p
}

dgp_plot <- function(df, subtitle = ""){
  suppressWarnings({
    # get treatment times and remove the control group
    treat_times <- get_treat_times(df)
    treat_times <- treat_times[treat_times != max(treat_times)]
    # relabel the control group
    control_label <- as.character(max(df$period)) # max period considered as control group
    
    # create a vector with the color palette
    color_vector <- tableau_color_pal()(length(unique(df$group)))
    
    # assign color to each group
    unique_groups <- sort(unique(df$group))
    names(color_vector) <- as.character(unique_groups)
    
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
    vlines <- data.frame(xintercept = treat_times, color = treat_times)
    p <- p + geom_vline(data = vlines, aes(xintercept=xintercept, color=factor(color)), 
                        size = 0.5, show.legend = FALSE) +
      scale_color_manual(values = color_vector) +
      scale_color_discrete(labels = function(x) ifelse(x == control_label, "Control", x)) 
    
    p
  })
}
