#### This Script carries all simulations

# Function that sets all sim parameters not matter which action user chooses
sim_all <- function() {
  ## 20 periods, treatment times treatgroups = c(11,14,16,18) ##
  # DGP 1
  sim1_20_out = run_sim_parallel(1:505, dgp_1_sim, n = 500, t = 20,
                                 treatgroups = c(11,14,16,18))
  sim1_20 = verify_sim_results(sim1_20_out)
  verify_iteration_counts(sim1_20)
  save_sim_results(sim1_20, "DGP-1")
  
  # DGP 2
  sim2_20_out = run_sim_parallel(1:505, dgp_2_sim, n = 500, t = 20,
                                 treatgroups = c(11,14,16,18))
  sim2_20 = verify_sim_results(sim2_20_out)
  verify_iteration_counts(sim2_20)
  save_sim_results(sim2_20, "DGP-2")
  
  # DGP 3
  sim3_20_out = run_sim_parallel(1:505, dgp_3_sim, n = 500, t = 5,
                                 treatgroups = c(11,14,16,18))
  sim3_20 = verify_sim_results(sim3_20_out)
  verify_iteration_counts(sim3_20)
  save_sim_results(sim3_20, "DGP-3")
  
  # DGP 4
  sim4_20_out = run_sim_parallel(1:505, dgp_4_sim, n = 500, t = 20,
                                 treatgroups = c(11,14,16,18))
  sim4_20 = verify_sim_results(sim4_20_out)
  verify_iteration_counts(sim4_20)
  save_sim_results(sim4_20, "DGP-4")
  
  # DGP 5
  sim5_20_out = run_sim_parallel(1:505, dgp_5_sim, n = 500, t = 20,
                                 treatgroups = c(11,14,16,18))
  sim5_20 = verify_sim_results(sim5_20_out)
  verify_iteration_counts(sim5_20)
  save_sim_results(sim5_20, "DGP-5")
  
  # DGP 6
  sim6_20_out = run_sim_parallel(1:505, dgp_6_sim, n = 500, t = 20,
                                 treatgroups = c(11,14,16,18))
  sim6_20 = verify_sim_results(sim6_20_out)
  verify_iteration_counts(sim6_20)
  save_sim_results(sim6_20, "DGP-6")
  
  # DGP 7
  sim7_20_out = run_sim_parallel(1:505, dgp_7_sim, n = 500, t = 20,
                                 treatgroups = c(11,14,16,18))
  sim7_20 = verify_sim_results(sim7_20_out)
  verify_iteration_counts(sim7_20)
  save_sim_results(sim7_20, "DGP-7")
  
  # DGP 8
  sim8_20_out = run_sim_parallel(1:505, dgp_8_sim, n = 500, t = 20,
                                 treatgroups = c(11,14,16,18))
  sim8_20 = verify_sim_results(sim8_20_out)
  verify_iteration_counts(sim8_20)
  save_sim_results(sim8_20, "DGP-8")
  
  ## 50 periods, treatment times treatgroups = c(11,20,30,40) ##
  # DGP 1
  sim1_50_out = run_sim_parallel(1:505, dgp_1_sim, n = 500, t = 50,
                                 treatgroups = c(11,20,30,40))
  sim1_50 = verify_sim_results(sim1_50_out)
  verify_iteration_counts(sim1_50)
  save_sim_results(sim1_50, "DGP-1")
  
  # DGP 2
  sim2_50_out = run_sim_parallel(1:505, dgp_2_sim, n = 500, t = 50,
                                 treatgroups = c(11,20,30,40))
  sim2_50 = verify_sim_results(sim2_50_out)
  verify_iteration_counts(sim2_50)
  save_sim_results(sim2_50, "DGP-2")
  
  # DGP 3
  sim3_50_out = run_sim_parallel(1:505, dgp_3_sim, n = 500, t = 5,
                                 treatgroups = c(11,20,30,40))
  sim3_50 = verify_sim_results(sim3_50_out)
  verify_iteration_counts(sim3_50)
  save_sim_results(sim3_50, "DGP-3")
  
  # DGP 4
  sim4_50_out = run_sim_parallel(1:505, dgp_4_sim, n = 500, t = 50,
                                 treatgroups = c(11,20,30,40))
  sim4_50 = verify_sim_results(sim4_50_out)
  verify_iteration_counts(sim4_50)
  save_sim_results(sim4_50, "DGP-4")
  
  # DGP 5
  sim5_50_out = run_sim_parallel(1:505, dgp_5_sim, n = 500, t = 50,
                                 treatgroups = c(11,20,30,40))
  sim5_50 = verify_sim_results(sim5_50_out)
  verify_iteration_counts(sim5_50)
  save_sim_results(sim5_50, "DGP-5")
  
  # DGP 6
  sim6_50_out = run_sim_parallel(1:505, dgp_6_sim, n = 500, t = 50,
                                 treatgroups = c(11,20,30,40))
  sim6_50 = verify_sim_results(sim6_50_out)
  verify_iteration_counts(sim6_50)
  save_sim_results(sim6_50, "DGP-6")
  
  # DGP 7
  sim7_50_out = run_sim_parallel(1:505, dgp_7_sim, n = 500, t = 50,
                                 treatgroups = c(11,20,30,40))
  sim7_50 = verify_sim_results(sim7_50_out)
  verify_iteration_counts(sim7_50)
  save_sim_results(sim7_50, "DGP-7")
  
  # DGP 8
  sim8_50_out = run_sim_parallel(1:505, dgp_8_sim, n = 500, t = 50,
                                 treatgroups = c(11,20,30,40))
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

  # Push new results
  push_sim_results()
  stop("Simulations Done!")
  
} else if (sim_bool == 2) {
  sim_all()
  
  # Push new results
  push_sim_results()
  
  # load results for further analysis
  load_all_results()
  
} else {
  ## Simulate using purrr:map 
  # slower but more reliable
  # no corrupted iterations
  
  # DGP 1
  sim1_out = run_sim_map(1:500, dgp_1_sim)
  sim1 = verify_sim_results(sim1_out)
  verify_iteration_counts(sim1)
  save_sim_results(sim1, "Sim_1")
  
  # DGP 2
  sim2_out = run_sim_map(1:500, dgp_2_sim)
  sim2 = verify_sim_results(sim2_out)
  verify_iteration_counts(sim2)
  save_sim_results(sim2, "Sim_2")
  
  # DGP 3
  sim3_out = run_sim_map(1:500, dgp_3_sim)
  sim3 = verify_sim_results(sim3_out)
  verify_iteration_counts(sim3)
  save_sim_results(sim3, "Sim_3")
  
  # DGP 4
  sim4_out = run_sim_map(1:500, dgp_4_sim)
  sim4 = verify_sim_results(sim4_out)
  verify_iteration_counts(sim4)
  save_sim_results(sim4, "Sim_4")
  
  # DGP 5
  sim5_out = run_sim_map(1:500, dgp_5_sim)
  sim5 = verify_sim_results(sim5_out)
  verify_iteration_counts(sim5)
  save_sim_results(sim5, "Sim_5")
  
  # DGP 6
  sim6_out = run_sim_map(1:500, dgp_6_sim)
  sim6 = verify_sim_results(sim6_out)
  verify_iteration_counts(sim6)
  save_sim_results(sim6, "Sim_6")
  
  # DGP 7
  sim7_out = run_sim_map(1:500, dgp_7_sim)
  sim7 = verify_sim_results(sim7_out)
  verify_iteration_counts(sim7)
  save_sim_results(sim7, "Sim_7")
  
  # DGP 8
  sim8_out = run_sim_map(1:500, dgp_8_sim)
  sim8 = verify_sim_results(sim8_out)
  verify_iteration_counts(sim8)
  save_sim_results(sim8, "Sim_8")
  
  # Push new results
  push_sim_results()
  
  # load all results
  load_all_results()

}


