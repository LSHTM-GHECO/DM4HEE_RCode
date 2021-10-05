# Decision Modelling for Health Economic Evaluation R Code

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
## About

This repository was originally created to write R code that was complementary to the "Decision Modelling for Health Economic Evaluation" course. See this website for further information about the course: https://modelling.he-evalcourses.com/

Excel versions of the models used throughout the course are available here (versions aligned with the corresponding book chapters): https://www.herc.ox.ac.uk/downloads/decision-modelling-for-health-economic-evaluation 

We then used this code to aid in writing our tutorial paper entitled "Extensions of Health Economic Evaluation in R for Microsoft Excel Users: A Tutorial for incorporating heterogeneity and conducting value of information analyses". 

## Repo Layout

This repo has the original course code, including exercise questions, templates and solutions. It also has summary R scripts that are cited in the aforementioned paper.

This directory has the following structure:

```
         Foundations_Course ├── which has all Foundation Course code & data
                            
            Advanced_Course ├── which has all Advanced Course code & data
         
                Manuscript  ├── which has all manuscript code & data
```

### Course Workflow

The Foundation Course materials cover the following topics: 

```
        ├── Foundations_Course 
            └── F0 Reader for Foundation Course 
            └── F2 Decision Trees
            └── F3 Markov Models 
            └── F4 Value of Diagnostic Information                             
```

The Advanced Course materials cover the following:

```
        ├── Advanced Course 
            └── A0 Reader for Advanced Course 
            └── A1 Advanced Markov Modelling
            └── A2 Making Models Probabilistic 
            └── A3 Presenting Simulation Results
            └── A4 Value of Information                               
```

Each Module (excluding the Reader) has the following:

* An instruction PDF (e.g. “F2.3.1_Decision_Trees_Instructions.pdf” which refers to Foundation Module 2, R stream, File 1): This provides a step-by-step guide for each exercise, providing important information on parameters and hints & tips on how to code your models. This should be used in conjunction with. ..

* A template R script (e.g. “F2.3.2_Decision_Trees_Template.R” which refers to Foundation Module 2, R stream, File 2): This provides an outline of the model code, with key variables, functions and/or loops to define. There are blank spaces to fill whilst following the Instructions pdf.

* A solutions R script (e.g. “F2.3.1_Decision_Trees_Solutions.pdf” which refers to Foundation Module 2, R stream, File 3) which provides a completed model code script, filled in for you. If you run this R script (having in mind the section below about directory set up), this will provide the fully functional models and results.

Note: For Modules A3 and A4, the Template and Solution files are split into ‘Part 1’ and ‘Part 2’. The Instructions PDF walks you through when to use which.


### Manuscript Workflow

The manuscript folder contains:

* The original THR model R script: THR_Model.R

* An R script providing code to perform Value of Information analyses on the THR model: THR_Model_VOI.R

* All necessary data files to run the relevant R scripts: life-table.csv, hazardfunction.csv, cov55.csv.

* The excel version of the model which was downloaded from "https://www.herc.ox.ac.uk/downloads/decision-modelling-for-health-economic-evaluation" written by Andrew Briggs, Mark Sculpher and Karl Claxton [last accessed 25/08/2021]

### How to Cite this Work

Prior to the release of the corresponding manuscript, please cite any R code used from the course or the repository as per stated within our License: "Jack Williams[1], Nichola R. Naylor[1] and Andew Briggs, 2021, "Decision Making for Health Economic Evaluation R Code. [1]Equal contribution/Joint First Author"

Once the manuscript has been published, please use this citation. [This will be updated once the citation is finalised].

### Other information

Please do not attempt to make changes to the original repo. 
Please contact chilgithub@lshtm.ac.uk, either Nichola or Jack can then respond to any queries. 

