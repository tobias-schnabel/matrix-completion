#### This Script carries all simulations
if (Sys.info()[7] == "ts" | Sys.info()[7] == "tobiasschnabel") {
  # library(pushoverr)
  # set_pushover_user(user = "")
  # set_pushover_app(token = "")
}
writeLines("Which simulation process should be used?")
writeLines("-----------------------------------------------------------------")
writeLines("Press 0 to load results from disk to replicate tables and figures")
writeLines("-----------------------------------------------------------------")
sim_bool = utils::menu(c("Single-core (takes around 6h per simulation)", 
                          "Parallelized (recommended, substantially faster but takes up 100% CPU)"), 
                        title = "Press 0 to abort and load saved data" )
if (sim_bool == 0) {
  load_all_results()
} else if (sim_bool == 2) {
  ## Simulate using mclapply
  
  ## 55 periods ##
  # DGP 1
  sim1_55_out = run_sim_parallel(1:505, dgp_1_sim, n = 500, t = 55)
  sim1_55 = verify_sim_results(sim1_55_out)
  verify_iteration_counts(sim1_55)
  save_sim_results(sim1_55, "DGP-1")
  
  # DGP 2
  sim2_55_out = run_sim_parallel(1:505, dgp_2_sim, n = 500, t = 55)
  sim2_55 = verify_sim_results(sim2_55_out)
  verify_iteration_counts(sim2_55)
  save_sim_results(sim2_55, "DGP-2")
  
  # DGP 3
  sim3_55_out = run_sim_parallel(1:505, dgp_3_sim, n = 500, t = 55)
  sim3_55 = verify_sim_results(sim3_55_out)
  verify_iteration_counts(sim3_55)
  save_sim_results(sim3_55, "DGP-3")
  
  # DGP 4
  sim4_55_out = run_sim_parallel(1:505, dgp_4_sim, n = 500, t = 55)
  sim4_55 = verify_sim_results(sim4_55_out)
  verify_iteration_counts(sim4_55)
  save_sim_results(sim4_55, "DGP-4")
  
  # DGP 5
  sim5_55_out = run_sim_parallel(1:505, dgp_5_sim, n = 500, t = 55)
  sim5_55 = verify_sim_results(sim5_55_out)
  verify_iteration_counts(sim5_55)
  save_sim_results(sim5_55, "DGP-5")
  
  # DGP 6
  sim6_55_out = run_sim_parallel(1:505, dgp_6_sim, n = 500, t = 55)
  sim6_55 = verify_sim_results(sim6_55_out)
  verify_iteration_counts(sim6_55)
  save_sim_results(sim6_55, "DGP-6")
  
  # DGP 7
  sim7_55_out = run_sim_parallel(1:505, dgp_7_sim, n = 500, t = 55)
  sim7_55 = verify_sim_results(sim7_55_out)
  verify_iteration_counts(sim7_55)
  save_sim_results(sim7_55, "DGP-7")
  
  # DGP 8
  sim8_55_out = run_sim_parallel(1:505, dgp_8_sim, n = 500, t = 55)
  sim8_55 = verify_sim_results(sim8_55_out)
  verify_iteration_counts(sim8_55)
  save_sim_results(sim8_55, "DGP-8")
  
  
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
  
  # load results
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
  
  # load all results
  load_all_results()

}


