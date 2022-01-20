# 'MasterThesis' - Master thesis project at UMC Utrecht
This repository contains all material (manuscripts, code) related to my master thesis for the program 'Methodology and Statistics for the Behavioral, Biomedical and Social Sciences' at Utrecht University, The Netherlands.

- Name: Daphne Weemering
- Project's name: *Towards adequately powered series of N-of-1 trials*
- Date created: January 3, 2022
- Email: dnweemering@gmail.com / d.n.weemering@students.uu.nl

## What is the project about?
For this master thesis project, I am investigating the usefulness of interim sample size reestimation for combined N-of-1 trials under the supervision of Peter van de Ven at the UMC Utrecht. Combined N-of-1 trials recently gained interest for studying the effectiveness of interventions in rare diseases. Rare diseases pose a major challenge for the identification of effective treatments as conventional randomized controlled trials generally require a infeasible number of subjects. Comining seperate N-of-1 trials allows for estimation of the population treatment effect. The number of trials necessary is determined beforehand by means of sample size calculations. These calculations are particularly challenging because relevant parameters are generally unknown at the design stage of a study. 

With this master thesis project, I investigate the incorporation of interim sample size reestimation to estimate the required sample size in N-of 1 trials. Simulation studies are used to assess to what extent the misspecification of each of the unknown parameters affects the required sample size, power and type I error rate in a series of N-of-1 trials.

## What is in this repository?
The repository contains the following folders:
- Documentation: This folder includes all the documentation (in .tex and corresponding .bib files) for the 1) research proposal and 2) research report. The proposal was produces to introduce the project to the Board of Studies at the Faculty of Social and Behavioral Sciences at Utrecht University. The research report is an interim product of the master thesis that is presented as a mini-thesis. 
- Figures: This folder contains all the figures that are included in the documentation, and the code to produce these figures. For now, only one figure (which is presented in the research report) is available in this folder. 
- Functions: In this folder, the functions that are created for performing the simulations are stored.
  - 'Initial and final sample size functions.R' includes two functions, one for calculating the initial sample size under hypothesized (i.e., assumed) parameter values for parameters that are unknown at the start of the studies. These functions take the following arguments: 
    1. x: A list including the value for the hypothesized residual, a value for the hypothesized random treatment effect, and a value for the fraction of the initial sample size on which the interim sample size reestimation will be based (should be entered in this order!);
    2. n_cycles: The number of cycles within each single N-of-1 trial. This is set to 3; 
    3. y: A list including the true value for the residual (for simulating the data), the true value for the random treatment effect (for simulating the data) and the fraction of the sample size calculated from the function 'initsampsize' (should be entered in this order!);
    4. N: Number of simulations. This is set to 1000;
    5. avg_treatment: The average treatment effect. This is set to 1;
    6. seed: A seed for reproducibility. This is set to 3239480. 
  - 'Interim n-reestimation n-of-1 trial.R' includes a function that evaluates a whole trial that incorporates interim sample size reestimation. First, the initial sample size under hypothesized parameter values are calculated. A fraction (which can be specified by the user) of that initial sample size is observed and after observing those subjects, the parameters are estimated. The interim estimates are used to reestimate the parameters. A new sample size rolls out, and the remaining number of subjects are observed. Then, parameters are estimated for the full data. This process is repeated N (N = number of iterations) times. After N simulations, power is calculated as the number of iterations that the fixed effect was significantly different from zero. This function takes the following arguments: 
    1. x: A list including the value for the hypothesized residual, a value for the hypothesized random treatment effect, a true value for the random treatment effect (for simulating the data), a true value for the residual (for simulating the data), and a value for the fraction of the initial sample size on which the interim sample size reestimation will be based (should be entered in this order!); 
    2. n_cycles: The number of cycles within each single N-of-1 trial. This is set to 3; 
    3. avg_treatment: The average treatment effect. This is set to 1;
    4. N: Number of simulations. This is set to 5000;
    5. seed: A seed for reproducibility. This is set to 3239480. 
- Simulations: This folder contains the file that runs the simulation under different parameter settings (different values for the hypothesized and true values of the residual and the random treatment effect). This file runs the simulations for the research report, where the functions from the file 'Initial and final sample size functions.R' are used to calculate the initial sample size and final sample size (the sample size after interim reestimation of the sample size). For this project I ran simulations where the hypothesized and true random treatment effect are c(0.5, 1, 2), where the hypothesized and true residuals are c(0.25, 0.5, 1), and where the fractions of the initial sample size are c(0.25, 0.5, 0.75). 
