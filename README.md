# Code and Data for The Global Jukebox: A Public Database of Performing Arts and Culture; Wood et al. (2022)

[![DOI](https://zenodo.org/badge/267221085.svg)](https://zenodo.org/badge/latestdoi/267221085)

This repository contains the data and code used to create the analysis and figures in Wood et al. (2021). 
A Preprint of this article is availalb at https://psyarxiv.com/4z97j/.
Data for the Global Jukebox can be found at https://github.com/theglobaljukebox. 

This repository contains code on the three analytical sections of Wood et al. (2021).

1. The correlational analysis in the main text. Code under correlations/
2. The inter-rater reliability scores in Supplementary section 5.1. Code under 'Inter-rater reliability'/
3. The computational coding validataion in Supplementary section 6.0. Code under /validation

Analyses can be run using the `Makefile`

To update the data for these analyses, you will need access to the Google Spreadsheets that have all the canto coding data and metadata.
If you have access, these can be downloaded via  `make get_data`

The data used for this analyses are archived in this repository. 

To run the correlation analyses run `make correlations` 

To run the Inter-rater reliability analyses run `make interrater`

The analyses for Inter-rater reliability score relies on `GJBIRRPreReg.R`. This file contains all code for exploratory analyses used to prepare the pregistered inter-rater reliability analyses, including power analysis and prepared code for the eventual confirmatory analysis. This code reads data directly from a password-protected Google Sheet, but an Excel version of that spreadsheet as of 2020-09-04 was downloaded and uploaded for those without password access.

To run the computational coding validation run `make validate`

### Installation Guide/Dependencies

The analyses here is performed on a combination of python and R. Below is some notes to guide reproducing the analyses. 
All package requirements for R and Python are installed with `make install`

#### R version 4.1 

This analyses was performed on R 4.1. To check the R version you are running, type `sessionInfo()` into the R terminal. 

R Packages used are listed in `requirements.R`. 

#### Python Version 3.4

You must have a working version of Python 3 installed.
You may download it here https://www.python.org/downloads/

To check to see if your Python has been added to your path open terminal and type:

`python --version`

Ensure that your pip distribution is for Python 3.

`pip --version`

If you get and error, refer to online forums on how to get around this, as the solutions are installation specific, and vary depedning on your operating system.

If all is well until now, you may proceed to install dependencies

`pip install -r requirements.txt`

In order to update the data to it's most recent state run the script:
`python3 generate_modal_profiles.py`

If you are on linux and run into issues installing cartopy, ensure you have the following packages installed first:

```
sudo apt-get install libproj-dev proj-data proj-bin  
sudo apt-get install libgeos-dev  
sudo pip install cython  
sudo pip install cartopy  
```
