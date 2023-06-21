### This Script produces all Tables

## DGP 1
# Small Table
save_table_results(analyze_sim_results(sim1, "static"), 
                   caption = "Simulaltion 1", note = "1000 iterations",
                   file_name = "Sim1_short")