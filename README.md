
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A Multi-Modelling Approach for Informing the Conservation of a Cold-Adapted Terrestrial Amphibian in the Face of Climate Change

This repository contains the code used for the study on the climate
impacts on the Alpine salamander. You can access the publication [here](https://doi.org/10.1111/jbi.15005).

## Project Structure

The project is organized into the following main directories:

- data_processing/: Scripts for initial data preparation and cleaning.  
- explore/: Exploratory data analysis scripts.  
- figures/: Code to generate figures used in the manuscript.  
- functions/: Custom R functions used across various analysis scripts.  
- model/: Scripts pertaining to the fitting and projecting species
  distribution modeling (SDM).  
- niche_overlap/: Analysis pertaining to the niche overlap between
  subspecies.  
- main_setup.R: The main R script to set up the project environment,
  such as file and folder paths, and parameter settings.

## Automated Workflow

The entire analysis workflow is controlled via a Makefile that is
orchestrated via [GNU Make](https://www.gnu.org/software/make/) to
execute the scripts in a specified sequence. See the
[documentation](https://www.gnu.org/software/make/manual/make.html) on
Make and Makefiles for more info.

### Using the Makefile

To run the entire analysis pipeline or specific parts of it, you can use
commands specified in the Makefile. For example:

- `make fit_model taxon=points_atra` will run the model fitting for the
  specified taxon.  
- `make project_future runtype=slurm` will project the biomod models to
  future climate scenarios using a SLURM scheduler for parallel
  processing.

## Data Availability

All original data sources are cited within the manuscript. More
specifically, climate variables data you can retrieve from [CHELSA
climate website](https://chelsa-climate.org/). Species occurrence data
you can retrieve from [digital supporting
information](https://atra-climate.shinyapps.io/atra-climate/).

## Contact

For any additional questions or requests, please contact the
corresponding author at <m.cengic@science.ru.nl>.


