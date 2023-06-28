To replicate all results, tables, and figures, follow these steps:
1. Clone this repo into an IDE (I used Rstudio) and start a fresh R session. 
2. After this is done, set your working directory to the location of the project folder on your machine.
3. The two functions that save and load simulation results use relative paths, which is why step 2 is important.
4. Source the script (Setup.R), this will prompt you to use renv::restore() to install exact versions of all packages I used (recommended)
   Alternatively, the script will give you the optino of installing the newest version of each package from source.
   The latter is not recommended, as it may lead to compatibility issues
5. After package installation is done, the script will automatically prompt you to load my saved results (recommended), or
   resimulate yourself. The later can be done single-core (takes around 52h) or multicore, which may be quicker, but will
   utilise 100% of your CPU for around 24h and might produce parallelisation errors.
   I used withr::with_seed() to ensure that numerical results will be identical if you choose to resimulate. However, as noted by
   the maintainers of the MCPanel package, the random number generation used in the MC-NNM C++ implementation is platform-dependent,
   so  exact results for this estimator might vary marginally.
7. Once results are loaded, the (Setup.R) script will automatically produce all tables and figures.
8. The console output of the exact simulation runs I used to obtain the final results in my thesis is saved as a text file
   in the SimResults folder.
10. The "Export.R" script is only executed on my machine and contains hard-coded absolute file paths. It produces no new objects and
   is exclusively used to save tables into my Overleaf.

I appreciate suggestions for improvement :)
