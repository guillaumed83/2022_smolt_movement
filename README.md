The contents of this repository allow the reproducibility of the analysis carried out in the MS: *Navigating complexity: channel selection of Atlantic salmon smolts through an island network during their migration to the marine environment*


2022_smolt_project.Rproj is the R project file

+ Figures
Contains various figures and graphical files generated by files located in the Scripts folder
+ Model
Contains the JAGS model file called in Run_state-space_model.R
+ MS
Contains manuscript and supplementary material
+ Output
Contains various tables with model output of interest
+ Rdata
Contains Rdata files needed to run the JAGS model and generate some figures; Jags_output.Rdata contains MCMC files so user doesn’t have to re-run the model.
Also contains, two images used in some of the figures
+ Scripts
Contains 3 files
Run_state-space_model.R
Set up of the JAGS files and running the mcmc for the model , also generated some diagnostics and tables containing output of interest
Figures_MS_script.R


Network.R  This script is called in the Figures_MS_Script. R file, its purpose is to generate the schematic network of channels and associated average probabilities to be in a given channel. The script works but its super messy – I was learning things about library(igraph) and haven’t got around cleaning it.
