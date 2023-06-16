#### This Script carries out the main analysis
# Test parallel simulation function: DGP1 took 40 min @ 260 corrupted iters
par_start = Sys.time()
dgp1_raw = run_parallel_sim(1:700, dgp_1_sim)
par_end = Sys.time()
print(paste0("Elapsed time: ", (par_end - par_start)))
dgp_1 = verify_sim_results(dgp1_raw)
save_sim_results(dgp1)


