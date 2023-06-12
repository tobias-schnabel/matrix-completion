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
    period_fe = pure_rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit and period FE
  crossing(unit, period) %>% 
    mutate(error = pure_rnorm(nrow(.), 0, 0.5)) %>% 
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
    obsgroup = pure_sample(1:nobsgroups, nobs, replace = T),
    # sample from Normal Dist. with group-spec. mean and SD=1
    unit_fe = pure_rnorm(nobs, 0, 0.5),
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

## DGP 7  Multiple Treatment Groups, Time-Varying Heterogeneous TE, NO PARALLEL TRENDS
dgp_7_sim <- function(nobs = 1000, 
                      nperiods = 100,
                      nobsgroups = 50,
                      treatgroups = c(nperiods/5, 2*(nperiods/5), 3*(nperiods/5), 4*(nperiods/5))) {
  
  # Unit Fixed Effects
  unit <- tibble(
    unit = 1:nobs,
    obsgroup = pure_sample(1:nobsgroups, nobs, replace = T),
    unit_fe = pure_rnorm(nobs, 0, 0.5),
    group = case_when(
      obsgroup %in% 1:(nobsgroups%/%5) ~ treatgroups[1],
      obsgroup %in% ((nobsgroups%/%5) + 1):(2*nobsgroups%/%5) ~ treatgroups[2],
      obsgroup %in% ((2*nobsgroups%/%5) + 1):(3*nobsgroups%/%5) ~ treatgroups[3],
      obsgroup %in% ((3*nobsgroups%/%5) + 1):(4*nobsgroups%/%5) ~ treatgroups[4],
      obsgroup %in% ((4*nobsgroups%/%5) + 1):nobsgroups ~ nperiods
    ),
    avg.te = case_when(
      group == treatgroups[1] ~ .08,
      group == treatgroups[2] ~ .095,
      group == treatgroups[3] ~ .125,
      group == treatgroups[4] ~ .2,
      TRUE ~ 0
    )) %>%
    rowwise() %>% 
    mutate(te = pure_rnorm(1, avg.te, .2)) %>% 
    ungroup()
  
  # generate Time FE
  period <- tibble(
    period = 1:nperiods,
    period_fe = pure_rnorm(nperiods, 0, 0.5)
  )
  
  # interact unit, period, and nuisance parameter that breaks common trends
  crossing(unit, period) %>%
    mutate(
      nuisance = 0.002 * period * obsgroup + pure_rnorm(n(), 0, 0.02),
      error = pure_rnorm(n(), 0, 1),
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
# Get colors
plotcol <- tableau_color_pal()(6)  

set_ggplot_colors <- function() {
  # Define the color palette
  my_palette <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F")
  
  # Change the default ggplot colors for discrete scales
  scale_color_discrete <- function(...) scale_color_manual(values = my_palette, ...)
  scale_fill_discrete <- function(...) scale_fill_manual(values = my_palette, ...)
  
  # Change the default ggplot colors for continuous scales
  scale_color_continuous <- function(...) scale_color_gradientn(colors = my_palette, ...)
  scale_fill_continuous <- function(...) scale_fill_gradientn(colors = my_palette, ...)
}

# To apply the function:
set_ggplot_colors()


# Helper function to get unique treatment start times
get_treat_times <- function(df) {
  unique(df$group)
}


dgp_plot_basic <- function(df){
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





