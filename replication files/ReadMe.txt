DATA:

- Article for GEC (political economy of disaster damage).dta: from Neumayer et al. (2014), retrived from http://www2.lse.ac.uk/dataFiles/geographyAndEnvironment/Replication/Article%20for%20GEC%20(political%20economy%20of%20disaster%20damage).dta

- UNdata_GDP_Per_Capita.csv: World Bank 2015, retrieved from https://data.un.org/Data.aspx?q=GDP&d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a1

- UNdata_GDP.csv: World Bank 2015, retrieved from https://data.un.org/Data.aspx?q=GDP+per+capita&d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a1

CODE:

The following R packages are required:
library(foreign)
library(data.table)
library(sampleSelection)
library(stargazer)
library(countrycode)
library(lattice)
library(foreign)
library(methods)
library(knitr)
# install.packages('devtools')
# library(devtools)
# install_github('IQSS/Zelig')
library(Zelig) # require Zelig 5 from GitHub

- code_1_data_cleaning.R: creates
dis_cyclones_final_merge_inflation_corrected.csv
dis_floods_final_merge_inflation_corrected.csv
dis_quakes_final_merge_inflation_corrected.csv

- code_2_selection_model_tables.R: creates (dynamically) the tables of the paper (via the R package knitr)

- code_3_graphs.: creates PDF of

plot_lattice_plot_sub.pdf
plot_lattice_plot_1.pdf
plot_lattice_plot_2.pdf
plot_zelig_cyclone_step1_1.pdf
plot_zelig_cyclone_step1_2.pdf
plot_zelig_cyclone_step2_1.pdf
plot_zelig_cyclone_step2_2.pdf
plot_zelig_flood_step1_1.pdf
plot_zelig_flood_step1_2.pdf
plot_zelig_flood_step2_1.pdf
plot_zelig_flood_step2_2.pdf
plot_zelig_quake_step1_1.pdf
plot_zelig_quake_step1_2.pdf
plot_zelig_quake_step2_1.pdf
plot_zelig_quake_step2_2.pdf

PAPER:

- paper.pdf

- paper_anonymous.pdf

- paper.bib
