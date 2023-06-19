#### This Script carries all simulations
writeLines("Which simulation process should be used?")
sim_bool = utils::menu(c("Single-core (takes around 6h per simulation)", 
                          "Parallelized (recommended, substantially faster but takes up 100% CPU)"), 
                        title = "Press 0 to abort and load saved data" )
if (sim_bool == 0) {
  writeLines("Loading saved data")
} else if (sim_bool == 2) {
  ## Simulate using mclapply
  # substantially faster
  # but also more dropped iterations
  
  # DGP 1
  sim1_par_out = run_sim_parallel(1:1000, dgp_1_sim)
  sim1_par = verify_sim_results(sim1_par_out)
  verify_iteration_counts(sim1_par)
  save_sim_results(sim1_par, "Sim_1_par")
  
  # DGP 2
  sim2_par_out = run_sim_parallel(1:1000, dgp_2_sim)
  sim2_par = verify_sim_results(sim2_par_out)
  verify_iteration_counts(sim2_par)
  save_sim_results(sim2_par, "Sim_2_par")
  
  # DGP 3
  sim3_par_out = run_sim_parallel(1:1000, dgp_3_sim)
  sim3_par = verify_sim_results(sim3_par_out)
  verify_iteration_counts(sim3_par)
  save_sim_results(sim3_par, "Sim_3_par")
  
  # DGP 4
  sim4_par_out = run_sim_parallel(1:1000, dgp_4_sim)
  sim4_par = verify_sim_results(sim4_par_out)
  verify_iteration_counts(sim4_par)
  save_sim_results(sim4_par, "Sim_4_par")
  
  # DGP 5
  sim5_par_out = run_sim_parallel(1:1000, dgp_5_sim)
  sim5_par = verify_sim_results(sim5_par_out)
  verify_iteration_counts(sim5_par)
  save_sim_results(sim5_par, "Sim_5_par")
  
  # DGP 6
  sim6_par_out = run_sim_parallel(1:1000, dgp_6_sim)
  sim6_par = verify_sim_results(sim6_par_out)
  verify_iteration_counts(sim6_par)
  save_sim_results(sim6_par, "Sim_6_par")
  
  # DGP 7
  sim7_par_out = run_sim_parallel(1:1000, dgp_7_sim)
  sim7_par = verify_sim_results(sim7_par_out)
  verify_iteration_counts(sim7_par)
  save_sim_results(sim7_par, "Sim_7_par")
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
}
