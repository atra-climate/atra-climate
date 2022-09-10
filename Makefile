# Makefile setup for atra climate impacts

# This approach could be used, if all variable/data names are kept constant
#%.dat : books/%.txt countwords.py
#	python countwords.py $< $*.dat
# ------------ helpfile ----------------------
# make help
# Define help
# This will recognize and double # as a help command for make help
.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<

  
# ----------- Models ---------------------- 
# ----------- fit model -----------------------
## ------ Checks ------
# Check files
## checks : Check how many files were created

.PHONY : checks
checks :  
	Rscript R/functions/helpers_makefile.R
##
## ------ niche overlap ------

# Fit biomod models. Solve for .tif and .gpkg files
## niche_overlap : Run niche overlap. Argument $runtype can be `slurm`. make niche_overlap runtype=slurm nichetype=spatial groupA=points_all groupB=points_atra  
# depends file 
#
.PHONY : niche_overlap
niche_overlap : misc/makefile_helpers/num_niche_overlap_xx.makedat

# Get niche overlap changes; # Define script for target file
misc/makefile_helpers/num_niche_overlap_xx.makedat	:	
	bash bash/run_niche_overlap.sh $(runtype) $(nichetype) $(groupA) $(groupB)
	
# ----------- fit model -----------------------
##
## ------ Fit model ------
## Fit and project current models. Seems to work.
# Fit biomod models. Solve for .tif and .gpkg files
## fit_model : Fit biomod model. Argument $runtype can be `slurm`. Pass argument taxon eg: make fit_model taxon=points_atra Options: points_all, points_atra, points_prenjensis
# depends file num_PP_projections_XX.makedat is made by helpers_makefile.R
# this script creates a file where XX is number of created projection files
.PHONY : fit_model
fit_model : misc/makefile_helpers/num_current_projections_45.makedat

# Get landcover changes; # Define script for target file
misc/makefile_helpers/num_current_projections_45.makedat	:	
	bash bash/run_fit_models.sh $(taxon) $(runtype)
##
## ------ Project models ------
# ----------- models past -----------------------
# 
# Project past biomod models. Solve for .tif and .gpkg files
## project_past : Project biomod models to past climate. Arguments taxon and runtype.
# depends file num_PP_projections_XX.makedat is made by helpers_makefile.R
# this script creates a file where XX is number of created projection files
.PHONY : project_past
project_past : misc/makefile_helpers/num_past_projections_84.makedat

# Get landcover changes; # Define script for target file
misc/makefile_helpers/num_past_projections_84.makedat	:	
	bash bash/run_project_past_models.sh $(taxon) $(runtype)
  
# ----------- models future -----------------------
# 
# Project future biomod models. Solve for .tif and .gpkg files
## project_future : Project biomod models to future climate. Arguments taxon and runtype.
# depends file num_PP_projections_XX.makedat is made by helpers_makefile.R
# this script creates a file where XX is number of created projection files
.PHONY : project_future
project_future : misc/makefile_helpers/num_future_projections_45.makedat

# Get landcover changes; # Define script for target file
misc/makefile_helpers/num_future_projections_45.makedat	:	
	bash bash/run_project_future_models.sh $(taxon) $(runtype)
 
# Store binary projections ----------------
## binary_projections : Store binary projection from biomod2 folder to user folder

.PHONY : binary_projections
binary_projections : misc/makefile_helpers/num_binary_projections_28.makedat 

# depends script depends on helpers_makefile.R
# 28 is testing number TODO change number
misc/makefile_helpers/num_binary_projections_192.makedat	:	
	Rscript R/analyze_output/load_biomod_projections.R

## ------ Bivariate maps ------
# Check files
## bivariate_maps : Make bivariate maps

.PHONY : bivariate_maps
bivariate_maps :  
	bash bash/run_make_bivariate_map.sh

# ----------- Cleaning and deleting stuff ---------------------- 
##
## ------ Cleaning stuff ------
## !Add stuff
# ----------- Delete presences ---------------------- 
# Delete model stuff
## new_projections_current : Remove models
new_projections_current	:
	bash bash/remove_current_projections.sh
  
##  
## ------ tests ------
# ----------- pass arguments check -----------------------
# 
# Project future biomod models. Solve for .tif and .gpkg files
## pass_args : Project biomod models to future climate
.PHONY : pass_args
pass_args : misc/makefile_helpers/myfile.makedat

# Get landcover changes; # Define script for target file
misc/makefile_helpers/myfile.makedat	:	
	bash bash/mytest.sh $(period)
    
# ----------- slurm pass arguments check -----------------------
# 
# Project future biomod models. Solve for .tif and .gpkg files
## slurm_args : Project biomod models to future climate eg: make slurm_args period=current
.PHONY : slurm_args
slurm_args : misc/makefile_helpers/myfile2.makedat

# Get landcover changes; # Define script for target file
misc/makefile_helpers/myfile2.makedat	:	
	bash bash/slurm_test.sh $(type)
	
# ----------- slurm pass arguments check -----------------------
# 
# Project future biomod models. Solve for .tif and .gpkg files
## slurm_args : Project biomod models to future climate eg: make slurm_args period=current
.PHONY : slurm_test
slurm_test : misc/makefile_helpers/blblbl.makedat

# Get landcover changes; # Define script for target file
misc/makefile_helpers/blblbl.makedat	:	
	bash bash/run_project_past_models2.sh $(runtype)
  
