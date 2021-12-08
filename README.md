# Medical Data Science R shiny Toolbox
This repo collects 2 R shiny apps, written by Jiaxing Joy Qiu in Center of Advanced Medical Analytics, School of Medicine, UVA. 

## MLViz
The main utilitiy codes in Machine Learning Visualization (MLViz) folder, follows Frank Harrell's Regression Modeling Strategy (RMS) methodologies, sourced rms package in R as the core infrastructure. For binary classification, this tool uses logistic regression (ridge) and for continuous reponse regression, linear regression will be used.

#### Goal of MLViz
The goal of this tool is to provide a "baseline" model for projects who's aiming at using advanced modeling strategies or tool to compare with, such as Randam Forrest or Deep Learning, by changing a few UI front end codes and replacing the project dataset. Data Engineering, feature selection, model performance and predictor importance rank are automatically ran by simple clicks on a R shiny app, and results and processes are visualized as much as possible. 

#### User cases of MLViz
So far, this tool with identical utilities has been succesfully applied to 3 projects:
- Respiritory unfavorable outcomes in pre-term infants in NICU
- Sepsis in NICU (de-identified data)  https://joy-cama-uva.shinyapps.io/MLViz_PreMo/
- Blood Stream Infection in Adult ICU (de-identified data) https://joy-cama-uva.shinyapps.io/BSI_Modeling/

## EDAViz
The main utilitiy codes in Exploratory Data Analysis (EDAViz) folder, offers interative visualization on any dictionary-oriented dataset structure (created by Jiaxing Qiu). Visualization utilities includes 1D statistic curves, 2D heapmap and "death star" plots for the trajectories of certain patient cohort. (Yep, Come to the Dark Side ...)

#### Goal of EDAvIZ
EDAViz tool is built to help non-tech researchers visualize main charactoristics of a certion variable, explore the relationship between 2 or more variables, and generate research hypothesis such as finding the potential explanors for a certain responce variable(main charactoristics).

#### User cases of EDAViz
EDAViz tool has been used in PreVent NIH research to efficiently bridge the communication between clinician principal investigators, researchers and data scientists in the study.
