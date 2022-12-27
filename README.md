# honey_bee_loss_US_scirep

Data and methods to replicate the findings presented in:

L. Insolia, R. Molinari, S.R. Rogers, G.R. Williams, F. Chiaromonte and M. Calovi.
"*Honey bee colony loss linked to parasites, pesticides and extreme weather across the United States*".
Scientific Reports volume 12, Article number: 20787 (2022)

This repository contains the source code to process the data and replicate our analyses. 

- "main.R": can be run as is to reproduce our results. 
It sequentially sources the scripts contained in the main folder (for data extraction, pre-processing, and analysis).
The user should only change the path of the variable "mainwd" at the beginning of the script (pointing to the main "code" folder).

- "aux_functions": contains auxiliary functions.
    - "MIPdiagnostics.R": custom regression diagnostics for MIP;
    - "MIPplots.R": custom plotting function for MIP results;
    - "usda_data.R": processing of USDA honeybee data;
    - "cdl_data.R": processing of CDL land-use data;
    - "prism_data.R": processing of PRISM weather data;
    - "combine_data.R": combines all datasets.

- "data" should contain raw data in the "dataUSDA", "dataCDL", "dataPRISM" sub-folders (please see the main text for references to download these data).
By default processed data are automatically stored in the "tmp" sub-folder.
To store such datasets the user should set "savdat=1" at the beginning of the "main.R" script.
The full dataset "bee_data.csv" covering 2015-2021 is provided in the Supplementary Information.

- "julia" contains the code to reproduce MIP and sparseLTS results.  
In order to replicate our analyses, the user should only change the file path at the beginning of the .pbs, .py files and "mainAppBee.jl" (see "CODE" sub-folder).
It contains the following sub-folders:
    - "CODE": contains our implementations and auxiliary functions.
        - "mainAppBee.jl": main Julia code;
        - "MIPbee.jl": performs our MIP proposal based on the trimmed L-2 loss and using information criteria to tune k_p across various trimming levels k_n;
        - "est_prel.jl": computes sparseLTS estimates;
        - "julia.pbs": sends jobs to the HPC through the "submit.py" script and contains the options for the cluster in use;
        - "submit.py": Python script to submit all jobs based on PBS file, which loops through the input parameters listed in "file.csv";
        - "file.csv": csv file listing data and MIP options;
        - "inst_R_lib.jl": install all required R packages;
        - "beeLM.RData": should contain the data to analyze.
    - "output": default path to store the output (estimates of beta, phi, etc.).
        - "tuning": default path to store tuning results for each problem solved by MIP (based on information criteria).
    - "LOG": default path to save log files for various jobs.

- "MIPres" should contain cross-validated results provided by MIP. 
    - "tuning": should contain MIP results for the full dataset.

- "img" default path to store all plots presented in the paper in .eps format.
The user should set set "savfig = 1" in "main.R" script.

This code is implemented in Julia (v.0.6.0), Gurobi (v.8.1.1) and R (v.3.5.1 and v.3.6.2).

If you need help using the code, experience any bugs or have any suggestions, please contact Luca Insolia (Luca.insolia@sns.it).
