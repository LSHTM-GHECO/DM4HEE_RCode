# Decision Modelling for Health Economic Evaluation R Code

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) 

## About

This repository was originally created to write R code that was complementary to the "Decision Modelling for Health Economic Evaluation" course. See this website for further information about the course:

Excel versions of the models used throughout the course are available here (versions aligned with the corresponding book chapters): https://www.herc.ox.ac.uk/downloads/decision-modelling-for-health-economic-evaluation 

We then used this code to aid in writing our tutorial paper entitled "Extensions of Health Economic Evaluation in R for Microsoft Excel Users: A Tutorial for incorporating survival analysis, heterogeneity and value of information". 

### Repo Layout

This repo has the original course code, including exercise questions, templates and solutions. It also has summary R scripts that are cited in the aforementioned paper.

This directory has the following structure:

```
         Foundations_Course ├── which has all Foundation Course code & data
                            
            Advanced_Course ├── which has all Advanced Course code & data
         
                Manuscript  ├── which has all manuscript code & data
```


#### Course Workflow
1. The Foundation Course (covering decision trees,  an introduction into Markov models and value of diagnostic information) materials are in the "Foundation_Course" folder. 

Each Module (excluding Module 1 which does not have an R exercise component) has the following:
• An instruction pdf (e.g. “F2.3.1_Decision_Trees_Instructions.pdf” which refers to Foundation Module 2, R stream, File 1): This provides a step-by-step guide for each exercise, providing important information on parameters and hints & tips on how to code your models. This should be used in
conjunction with. ..
• A template R script (e.g. “F2.3.2_Decision_Trees_Template.R” which refers to Foundation Module 2, R stream, File 2): This provides an outline of the model code, with key variables, functions and/or loops to define. There are blank spaces to fill whilst following the Instructions pdf.
• A solutions R script (e.g. “F2.3.1_Decision_Trees_Solutions.pdf” which refers to Foundation Module 2, R stream, File 3) which provides a completed model code script, filled in for you. If you run this R script (having in mind the section below about directory set up), this will provide the fully functional
models and results.

2. The Advanced Course (covering probablistic sensitivty analyses, graphics and value of information analysis) materials are in the "Advanced_Course" folder.

Each Module has the following:
• An instruction pdf (e.g. “A1.3.1_Advanced_Markov_Modelling_Instructions.pdf” which refers to Advanced
Module 1, R stream, File 1): This provides a step-by-step guide for each exercise, providing important information on parameters and hints & tips on how to code your models. This should be used in conjunction with. . .
• A template R script (e.g. “A1.3.2_Advanced_Markov_Modelling_Template.R” which refers to Advanced Module 1, R stream, File 2): This provides an outline of the model code, with key variables, functions and/or loops to define - there are blank spaces to fill whilst following the Instructions pdf.
• A solutions R script (e.g. “A1.3.1_Advanced_Markov_Modelling_Solutions.pdf” which refers to Advanced Module 2, R stream, File 3) which provides a completed model code script, filled in for you. If you run this R script (having in mind the section below about directory set up), this will provide the
fully functional models and results.

For Modules 3 and 4, the Template and Solution files are split into ‘Part 1’ and ‘Part 2’, and the Instructions pdf walk you through when to use which.

#### Manuscript Workflow

### How to Cite this Work
Prior to the release of the corresponding manuscript, please cite any code used from the course or the repository as per stated within our License: "Jack Williams[1], Nichola R. Naylor[1] and Andew Briggs, 2021, "Decision Making for Health Economic Evaluation R Code. [1]Equal contribution/Joint First Author"

Once the manuscript has been published, please use this citation. [This will be updated once the citation is finalised].

### Other information
Please do not attempt to make changes to the original repo. Please contact chilgithub@lshtm.ac.uk and either Nichola or Jack will respond. 

