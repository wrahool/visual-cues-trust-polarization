# Visual Cues, Media Trust, and Affective Polarization

This repo contains replication scripts for the study "The Effects of Visual Cues on Media Trust and Affective Polarization"

You can run the final analysis on the anonymised pre-processed data (`data/visual-cues_anonymised-data.csv`) by simply executing the following files.

- `06_descriptives.R` which plots the descriptive statistics of the data and the main variables of interest.
- `07_analysis-prereg.R` which runs the preregistered analysis of the data (as reported in the main text).
- `08a_analysis-males.R` which produces the result for only the male subset
- `08b_analysis-females.R` which produces the result for only the female subset
- `08c_analysis-dems.R` which produces the result for only the Democrat subset
- `08d_analysis-reps.R` which produces the result for only the Republican subset

The purpose for the other files in the `scripts` folder are as follows:

- `01_sample-size-simulation.R` runs simulations to identify the optimum sample size for the pilot studies
- `02_pilot-study1-analysis.R` runs the analysis of the first pilot study
- `03_pilot-study2-analysis.R` runs the analysis of the second pilot study
- `04_main-study-sample-size-simulation.R` runs simulations to identify the optimum sample size for the main study
- `05_main-study-preprocess-prolific.R` runs the preprocessing pipeline of the raw main study data collected on Prolific and produces the `auxiliary/preprocessed-final-prolific-for-analysis.csv` file (which is not tracked on Github).
- The `data/visual-cues_anonymised-data.csv` is identical to the file `auxiliary/preprocessed-final-prolific-for-analysis.csv` but anonymised for public release.

All scripts have been tested on R version 4.3.1.
