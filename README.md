# 'MasterThesis' - Master thesis project at UMC Utrecht
This repository contains all material related to my master thesis for the program 'Methodology and Statistics for the Behavioral, Biomedical and Social Sciences' at Utrecht University, The Netherlands.

- Name: Daphne Weemering
- Project's name: *Towards adequately powered series of N-of-1 trials*
- Date created: January 3, 2022
- Email: dnweemering@gmail.com / d.n.weemering@students.uu.nl

## What is the project about?
For this master thesis project, I am investigating the usefulness of interim sample size reestimation for combined N-of-1 trials, under the supervision of Peter van de Ven at the UMC Utrecht. Combined N-of-1 trials recently gained interest for studying the effectiveness of interventions in rare diseases. Rare diseases pose a major challenge for the identification of effective treatments as conventional randomized controlled trials generally require a infeasible number of subjects. Combining seperate N-of-1 trials allows for estimation of the population treatment effect. The number of trials necessary is determined beforehand by means of sample size calculations. These calculations are particularly challenging because relevant parameters are generally unknown at the design stage of a study. 

With this master thesis project, I investigate the incorporation of interim sample size reestimation to estimate the required sample size in N-of 1 trials. Simulation studies are used to assess to what extent the misspecification of each of the unknown parameters affects the required sample size, power and type I error rate in a series of N-of-1 trials.

## What is in this repository?
The repository contains the following folders:
- Documentation: This folder includes all the documentation for the research proposal and the research report. The proposal was produced to introduce the project and to get approval from the the Board of Studies at the Faculty of Social and Behavioral Sciences at Utrecht University to continue this project. The research report is an interim product of the master thesis that is presented as a mini-thesis. The proposal folder includes at .tex file, a corresponding .bib file with the references, and a pdf. The research report folder includes a .tex file and a .Rmd file of which the content is the same. The ama.csl file is used for formatting the references in the .Rmd file. The .pdf file is the product of the .Rmd file. 
- Figures: This folder contains the code to produce the figures that are presented in the documentation. For now, only one figure (which is presented in the research report) is available in this folder. 
- Functions: In this folder, the functions that are created for performing the simulations are stored. The following `r` packages need to be installed to run the code: `lme4`, `pwr`, and `lmerTest`. The following files are to be found in the 'functions' folder:
  - 'Initial and final sample size functions.R' includes two functions, one for calculating the initial sample size under hypothesized (i.e., assumed) parameter values for parameters that are unknown at the start of the studies, and one for calculating the final sample size after sample size reestimation is performed. These functions take the following arguments: 
    - x: A list including the value for the hypothesized residual, a value for the hypothesized random treatment effect, and a value for the fraction of the initial sample size on which the interim sample size reestimation will be based (should be entered in this order!);
    - n_cycles: The number of cycles within each single N-of-1 trial. This is set to 3; 
    - y: A list including the true value for the residual (for simulating the data), the true value for the random treatment effect (for simulating the data) and the fraction of the sample size calculated from the function 'initsampsize' (should be entered in this order!);
    - N: Number of simulations. This is set to 1000;
    - avg_treatment: The average treatment effect. This is set to 1;
    - seed: A seed for reproducibility. This is set to 3239480. 
  - 'Interim n-reestimation n-of-1 trial.R' includes a function that evaluates a whole trial that incorporates interim sample size reestimation. First, the initial sample size under hypothesized parameter values are calculated. A fraction (which can be specified by the user) of that initial sample size is observed and after observing those subjects, the parameters are estimated. The interim estimates are used to reestimate the parameters. A new sample size rolls out, and the remaining number of subjects are observed. Then, parameters are estimated for the full data. This process is repeated N (N = number of iterations) times. After N simulations, power is calculated as the number of iterations that the fixed effect was significantly different from zero. This function takes the following arguments: 
    - x: A list including the value for the hypothesized residual, a value for the hypothesized random treatment effect, a true value for the random treatment effect (for simulating the data), a true value for the residual (for simulating the data), and a value for the fraction of the initial sample size on which the interim sample size reestimation will be based (should be entered in this order!); 
    - n_cycles: The number of cycles within each single N-of-1 trial. This is set to 3; 
    - avg_treatment: The average treatment effect. This is set to 1;
    - N: Number of simulations. This is set to 5000;
    - seed: A seed for reproducibility. This is set to 3239480. 
- Simulations: This folder contains the file that runs the simulation under different parameter settings (different values for the hypothesized and true values of the residual and the random treatment effect). This file runs the simulations for the research report, where the functions from the file 'Initial and final sample size functions.R' are used to calculate the initial sample size and final sample size (the sample size after interim reestimation of the sample size). For this project I ran simulations where the hypothesized and true random treatment effect are c(0.5, 1, 2), where the hypothesized and true residuals are c(0.25, 0.5, 1), and where the fractions of the initial sample size are c(0.25, 0.5, 0.75). 


This project is ongoing and new folders and files will eventually be added to this repository!
