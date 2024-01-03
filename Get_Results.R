#### This Script carries all simulations

# Function that sets all sim parameters not matter which action user chooses
sim_all <- function() {
  ## 21 periods, treatment times treatgroups = c(11,14,16,18) ##
  # DGP 1
  sim1_21_out = run_sim_parallel(1:505, dgp_1_sim, n = 500, t = 21,
                                 treatgroups = c(11,14,16,18))
  sim1_21 = verify_sim_results(sim1_21_out)
  verify_iteration_counts(sim1_21)
  save_sim_results(sim1_21, "DGP-1")
  
  # DGP 2
  sim2_21_out = run_sim_parallel(1:505, dgp_2_sim, n = 500, t = 21,
                                 treatgroups = c(11,14,16,18))
  sim2_21 = verify_sim_results(sim2_21_out)
  verify_iteration_counts(sim2_21)
  save_sim_results(sim2_21, "DGP-2")
  
  # DGP 3
  sim3_21_out = run_sim_parallel(1:505, dgp_3_sim, n = 500, t = 21,
                                 treatgroups = c(11,14,16,18))
  sim3_21 = verify_sim_results(sim3_21_out)
  verify_iteration_counts(sim3_21)
  save_sim_results(sim3_21, "DGP-3")
  
  # DGP 4
  sim4_21_out = run_sim_parallel(1:505, dgp_4_sim, n = 500, t = 21,
                                 treatgroups = c(11,14,16,18))
  sim4_21 = verify_sim_results(sim4_21_out)
  verify_iteration_counts(sim4_21)
  save_sim_results(sim4_21, "DGP-4")
  
  # DGP 5
  sim5_21_out = run_sim_parallel(1:505, dgp_5_sim, n = 500, t = 21,
                                 treatgroups = c(11,14,16,18))
  sim5_21 = verify_sim_results(sim5_21_out)
  verify_iteration_counts(sim5_21)
  save_sim_results(sim5_21, "DGP-5")
  
  # DGP 6
  sim6_21_out = run_sim_parallel(1:505, dgp_6_sim, n = 500, t = 21,
                                 treatgroups = c(11,14,16,18))
  sim6_21 = verify_sim_results(sim6_21_out)
  verify_iteration_counts(sim6_21)
  save_sim_results(sim6_21, "DGP-6")
  
  # DGP 7
  sim7_21_out = run_sim_parallel(1:505, dgp_7_sim, n = 500, t = 21,
                                 treatgroups = c(11,14,16,18))
  sim7_21 = verify_sim_results(sim7_21_out)
  verify_iteration_counts(sim7_21)
  save_sim_results(sim7_21, "DGP-7")
  
  # DGP 8
  sim8_21_out = run_sim_parallel(1:505, dgp_8_sim, n = 500, t = 21,
                                 treatgroups = c(11,14,16,18))
  sim8_21 = verify_sim_results(sim8_21_out)
  verify_iteration_counts(sim8_21)
  save_sim_results(sim8_21, "DGP-8")
  
  ## 50 periods, treatment times treatgroups = c(11,21,30,40) ##
  # DGP 1
  sim1_50_out = run_sim_parallel(1:505, dgp_1_sim, n = 500, t = 50,
                                 treatgroups = c(11,21,30,40))
  sim1_50 = verify_sim_results(sim1_50_out)
  verify_iteration_counts(sim1_50)
  save_sim_results(sim1_50, "DGP-1")
  
  # DGP 2
  sim2_50_out = run_sim_parallel(1:505, dgp_2_sim, n = 500, t = 50,
                                 treatgroups = c(11,21,30,40))
  sim2_50 = verify_sim_results(sim2_50_out)
  verify_iteration_counts(sim2_50)
  save_sim_results(sim2_50, "DGP-2")
  
  # DGP 3
  sim3_50_out = run_sim_parallel(1:505, dgp_3_sim, n = 500, t = 50,
                                 treatgroups = c(11,21,30,40))
  sim3_50 = verify_sim_results(sim3_50_out)
  verify_iteration_counts(sim3_50)
  save_sim_results(sim3_50, "DGP-3")
  
  # DGP 4
  sim4_50_out = run_sim_parallel(1:505, dgp_4_sim, n = 500, t = 50,
                                 treatgroups = c(11,21,30,40))
  sim4_50 = verify_sim_results(sim4_50_out)
  verify_iteration_counts(sim4_50)
  save_sim_results(sim4_50, "DGP-4")
  
  # DGP 5
  sim5_50_out = run_sim_parallel(1:505, dgp_5_sim, n = 500, t = 50,
                                 treatgroups = c(11,21,30,40))
  sim5_50 = verify_sim_results(sim5_50_out)
  verify_iteration_counts(sim5_50)
  save_sim_results(sim5_50, "DGP-5")
  
  # DGP 6
  sim6_50_out = run_sim_parallel(1:505, dgp_6_sim, n = 500, t = 50,
                                 treatgroups = c(11,21,30,40))
  sim6_50 = verify_sim_results(sim6_50_out)
  verify_iteration_counts(sim6_50)
  save_sim_results(sim6_50, "DGP-6")
  
  # DGP 7
  sim7_50_out = run_sim_parallel(1:505, dgp_7_sim, n = 500, t = 50,
                                 treatgroups = c(11,21,30,40))
  sim7_50 = verify_sim_results(sim7_50_out)
  verify_iteration_counts(sim7_50)
  save_sim_results(sim7_50, "DGP-7")
  
  # DGP 8
  sim8_50_out = run_sim_parallel(1:505, dgp_8_sim, n = 500, t = 50,
                                 treatgroups = c(11,21,30,40))
  sim8_50 = verify_sim_results(sim8_50_out)
  verify_iteration_counts(sim8_50)
  save_sim_results(sim8_50, "DGP-8")
  
  
  ## 100 periods ##
  # DGP 1
  sim1_100_out = run_sim_parallel(1:505, dgp_1_sim, n = 500, t = 100)
  sim1_100 = verify_sim_results(sim1_100_out)
  verify_iteration_counts(sim1_100)
  save_sim_results(sim1_100, "DGP-1")
  
  # DGP 2
  sim2_100_out = run_sim_parallel(1:505, dgp_2_sim, n = 500, t = 100)
  sim2_100 = verify_sim_results(sim2_100_out)
  verify_iteration_counts(sim2_100)
  save_sim_results(sim2_100, "DGP-2")
  
  # DGP 3
  sim3_100_out = run_sim_parallel(1:505, dgp_3_sim, n = 500, t = 100)
  sim3_100 = verify_sim_results(sim3_100_out)
  verify_iteration_counts(sim3_100)
  save_sim_results(sim3_100, "DGP-3")
  
  # DGP 4
  sim4_100_out = run_sim_parallel(1:505, dgp_4_sim, n = 500, t = 100)
  sim4_100 = verify_sim_results(sim4_100_out)
  verify_iteration_counts(sim4_100)
  save_sim_results(sim4_100, "DGP-4")
  
  # DGP 5
  sim5_100_out = run_sim_parallel(1:505, dgp_5_sim, n = 500, t = 100)
  sim5_100 = verify_sim_results(sim5_100_out)
  verify_iteration_counts(sim5_100)
  save_sim_results(sim5_100, "DGP-5")
  
  # DGP 6
  sim6_100_out = run_sim_parallel(1:505, dgp_6_sim, n = 500, t = 100)
  sim6_100 = verify_sim_results(sim6_100_out)
  verify_iteration_counts(sim6_100)
  save_sim_results(sim6_100, "DGP-6")
  
  # DGP 7
  sim7_100_out = run_sim_parallel(1:505, dgp_7_sim, n = 500, t = 100)
  sim7_100 = verify_sim_results(sim7_100_out)
  verify_iteration_counts(sim7_100)
  save_sim_results(sim7_100, "DGP-7")
  
  # DGP 8
  sim8_100_out = run_sim_parallel(1:505, dgp_8_sim, n = 500, t = 100)
  sim8_100 = verify_sim_results(sim8_100_out)
  verify_iteration_counts(sim8_100)
  save_sim_results(sim8_100, "DGP-8")
  
  # Push new results
  push_sim_results()
}

writeLines("Which simulation process should be used?")
writeLines("-----------------------------------------------------------------")
writeLines("Press 0 to load results from disk to replicate tables and figures")
writeLines("-----------------------------------------------------------------")
sim_bool = utils::menu(c("Single-core (takes around 6h per simulation)", 
                          "Parallelized (recommended, substantially faster but takes up 100% CPU)",
                          "Parallelized, simulate only (does not generate tables/ figures, intended for remote machine)"), 
                        title = "Press 0 to abort and load saved data" )
if (sim_bool == 0) {
  load_all_results()
} else if (sim_bool == 3) {
  
  sim_all()

  stop("Simulations Done!")
  
} else if (sim_bool == 2) {
  sim_all()
  
  # load results for further analysis
  load_all_results()
  
} 

