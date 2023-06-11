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
        group == 2 ~ 0.5,
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
    unit_fe = pure_rnorm(nobs, obsgroup/5, 1),
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
    unit_fe = pure_rnorm(nobs, obsgroup/5, 1),
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
      group == treatgroups[1] ~ 0.5,
      group == treatgroups[2] ~ 0.5,
      group == treatgroups[3] ~ 0.5,
      group == treatgroups[4] ~ 0.5,
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
    mutate(treat = ifelse(period >= group, 1, 0)) %>%
    # generate treatment effect
    mutate(t.eff = ifelse(treat == 1, te, 0)) %>%
    # add everything to get outcome
    mutate(y = unit_fe + period_fe + t.eff + error) %>% 
    # change column order
    select(unit, period, obsgroup, te, group, treat, everything())
}




