#### This Script carries out the main analysis
# Test parallel simulation function: DGP1 took 60 min @ 350 corrupted iters
sim1_start = Sys.time()
dgp1_raw = run_parallel_sim(1:500, dgp_1_sim)
sim1__end = Sys.time()
print(paste0("Elapsed time: ", (sim1_end - sim1_start)))
dgp_1 = verify_sim_results(dgp1_raw)
verify_iteration_counts(dgp_1)
save_sim_results(dgp_1)

# DGP 2
sim2_start = Sys.time()
dgp2_raw = run_parallel_sim(1:500, dgp_2_sim)
sim2_end = Sys.time()
print(paste0("Elapsed time: ", (sim2_end - sim2_start)))
dgp_2 = verify_sim_results(dgp2_raw)
verify_iteration_counts(dgp_2)
save_sim_results(dgp_2)

# DGP 3
sim3_start = Sys.time()
dgp3_raw = run_parallel_sim(1:500, dgp_3_sim)
sim3_end = Sys.time()
print(paste0("Elapsed time: ", (sim3_end - sim3_start)))
dgp_3 = verify_sim_results(dgp3_raw)
verify_iteration_counts(dgp_3)
save_sim_results(dgp_3)

# DGP 4
sim4_start = Sys.time()
dgp4_raw = run_parallel_sim(1:500, dgp_4_sim)
sim4_end = Sys.time()
print(paste0("Elapsed time: ", (sim4_end - sim4_start)))
dgp_4 = verify_sim_results(dgp4_raw)
verify_iteration_counts(dgp_4)
save_sim_results(dgp_4)

# DGP 5
sim5_start = Sys.time()
dgp5_raw = run_parallel_sim(1:500, dgp_5_sim)
sim5_end = Sys.time()
print(paste0("Elapsed time: ", (sim5_end - sim5_start)))
dgp_5 = verify_sim_results(dgp5_raw)
verify_iteration_counts(dgp_5)
save_sim_results(dgp_5)

# DGP 6
sim6_start = Sys.time()
dgp6_raw = run_parallel_sim(1:500, dgp_6_sim)
sim6_end = Sys.time()
print(paste0("Elapsed time: ", (sim6_end - sim6_start)))
dgp_6 = verify_sim_results(dgp6_raw)
verify_iteration_counts(dgp_6)
save_sim_results(dgp_6)

# DGP 7
sim7_start = Sys.time()
dgp7_raw = run_parallel_sim(1:500, dgp_7_sim)
sim7__end = Sys.time()
print(paste0("Elapsed time: ", (sim7_end - sim7_start)))
dgp_7 = verify_sim_results(dgp7_raw)
verify_iteration_counts(dgp_7)
save_sim_results(dgp_7)


