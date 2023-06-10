#### This script holds all custom functions

# Create Pure Functions dependent on RNG generation to guarantee reproducibility
pure_rnorm <- function(..., seed=globalseed){
  with_seed(seed, rnorm(...))
}

pure_sample <- function(..., seed=globalseed){
  with_seed(seed, sample(...))
}


# DGP functions

# treat.group = case_when(
#   obsgroup %in% sample(unique(obsgroup), nobs/ntreatgroups, replace = FALSE) ~ 1,
#   TRUE ~ sample(2:ntreatgroups, nobs, replace = FALSE)
# ),

# treatgroups = c(nperiods/5,2*(nperiods/5),
#                 3*(nperiods/5),4*(nperiods/5))

dgp_1_sim <- function(nobs = 1000, 
                  nperiods = 100,
                  nobsgroups = 50,
                  treated.period = 50) {
  unit <- tibble(
    unit = 1:nobs,
    # create observation groups similar to US states
    obsgroup = sample(1:nobsgroups, nobs, replace = T)
  )
  
  # Shuffle obsgroup and assign treatment status
  shuffled_groups <- pure_sample(unique(unit$obsgroup))
  half <- length(shuffled_groups) %/% 2
  
  unit <- unit %>%
    mutate(
      # sample from Normal Dist. with group-spec. mean and SD=1
      unit_fe = pure_rnorm(nobs, obsgroup/5, 1),
      
      # gen treatment and control groups
      group = case_when(
        obsgroup %in% shuffled_groups[1:half] ~ 1,
        obsgroup %in% shuffled_groups[(half + 1):length(shuffled_groups)] ~ 2
      ),
      # Mark Control as never treated
      evertreated = ifelse(group == 2, 1, 0),
      
      # avg yearly treatment effects by group
      avg.te = case_when(
        group == 2 ~ 2,
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
    period_fe = pure_rnorm(nperiods, 0, 1)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    # generate additive N(0,1) error
    mutate(error = pure_rnorm(nrow(.), 0, 1)) %>% 
    # generate treatment dummy
    mutate(treated = ifelse(evertreated == 1 & period > treated.period, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treated == 1, te, 0)) %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + t.eff + error) %>% 
    # change column order
    select(unit, period, obsgroup, te, evertreated, everything())
}


dgp1_diag <- function(df) {
  
  # Check number of observations
  num_obs <- nrow(df)
  cat("Number of observations:", num_obs, "\n")
  
  # Check number of periods
  num_periods <- length(unique(df$period))
  cat("Number of periods:", num_periods, "\n")
  
  # Check number of nonempty observation groups
  num_nonempty_obs_groups <- length(unique(df$obsgroup))
  cat("Number of nonempty observation groups:", num_nonempty_obs_groups, "\n")
  
  # Check in how many different periods units change treatment status for the first time
  first_treatment_change <- df %>% 
    group_by(unit) %>% 
    mutate(first_treatment_change = which.max(evertreated == 1)) %>% 
    pull(first_treatment_change) %>% 
    unique() %>% 
    length()
  
  cat("Number of distinct treatment adoption periods:", first_treatment_change, "\n")
  
  # Check the percentage of units that are never treated
  never_treated <- df %>% 
    group_by(unit) %>% 
    summarise(treated = max(evertreated)) %>% 
    summarise(never_treated = mean(treated == 0)) %>% 
    pull(never_treated) * 100
  
  cat("Percentage of units never treated:", never_treated, "%", "\n")
  
  # Return a list with all diagnostics
  invisible(list(num_obs = num_obs, 
                 num_periods = num_periods, 
                 num_nonempty_obs_groups = num_nonempty_obs_groups, 
                 first_treatment_change = first_treatment_change,
                 never_treated = never_treated))
}





