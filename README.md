## Pyromania: Fire transforms effects of terrestrial subsidies on aquatic ecosystem structure and function  
<a href="https://zenodo.org/doi/10.5281/zenodo.10045314"><img src="https://zenodo.org/badge/435987538.svg" alt="DOI"></a>  
CB Wall<sup>1</sup>, CJ Spiegel<sup>1</sup>, EM Diaz<sup>1</sup>, CH Tran<sup>1</sup>, A Fabiani<sup>1</sup>, TY Broe<sup>1</sup>, E Perez-Coronel<sup>1</sup>, SL Jackrel<sup>1</sup>, N Mladenov<sup>2</sup>, CC Symons<sup>3</sup>, JB Shurin<sup>1</sup>. (2023) _Global Change Biology_.  

<sup>1</sup>University of California, San Diego   
<sup>2</sup>San Diego State University  
<sup>3</sup>University of California, Irvine  
  

<p align="center">
  <img align="center" src="https://github.com/cbwall/Pyromania/blob/main/output/Fig1.%20Pyro%20schematic.jpg" width="65%" height="60%">
</p>
  
  
The goal of the *Pyromania Project* is to test how terrestrial subsides (plant biomass loading or "browning") and burning influence aquatic productivity, water quality/chemistry, and trophic transfer. We used a manipulative experiment to assess a range of plant material quantities (0-400g per tank) and fire treatment (burned vs unburned material) and the non-linearity of these effects on aquatic systems through 4 time-point samplings. We used 400L aquatic mesocosms and ran the experiment for ~90d in 2021-2022.  
  

## File Directory  
The file directory contains folders and scripts (Rmd) to be run in RStudio. The folders house data, output, and raw + polished figures.  
   - *Pyromania.Rproj* = the R project for the analysis, load this first to allow code to run from correct directory in R Studio
   - *Pyromania Episode 1. Jump in the Fyya.Rmd* = the scripts and annotated code chunks here will walk through analyses and produce outputs
   - *Pyromania Episode 1. Jump in the Fyya.html* = the knit output of the Rmd file. Open this in any browser.
 
   - Folders
     - *data* = contains raw and processed data files
     - *figures* = contains raw exported figures from code and processed/cleaned figures in the *final.published* folder
     - *output* = contains *final tables* and *final figures* subfolders and schematic images  

## Metadata
Important data files to generate maps, figures, and run models can be found in the *data folder*. The key data files are:  
  - *data/DOC.TN/* The csv files here show the DOC and TN data for each time point 
  - *data/GHG.gases/* The csv files here show the input greenhouse gas (GHG) data input for the script and the output with calculated concentrations 
  - *data/Isotopes/* The csv files here show the replicate samples and the isotope data for C and N
  - *Pyro_plant material_elemental.csv* for the burned and unburned plant material elemental analysis prior to adding detritus to tanks
  - *Pyro_water.phosph.csv* for the phosphate analysis of water
  - *Pyro_YSI.csv* is the YSI data for oxygen, temperature, and pH of tank water
  - *treatment.IDs.csv* are the tank identity with regards to treatments
