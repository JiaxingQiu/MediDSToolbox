# Medical Data Science Toolbox
*For tools in the box, we think outside the box.* <br/>

Jiaxing (Joy) Qiu, M.S., Data Science, School of Data Science, University of Virginia  <br/>

Thanks to Center of Advanced Medical Analytics (CAMA), School of Medicine, University of Virginia, the helpful comments and professional expriences inspire these creative tools in the box!<br/>

## Objectives
The objectives of this toolbox include but not limited to --
- facilitate medical data science research hypothesis generation
- bridge communitations between clinicians and technicians interactively
- visualize biomedical research data and especially longitudinal datasets
- provide baseline predictive models by supervised learning, such as logistic and linear regression
- provide reliable model evaluation and rubost inference results that adjust for repeated measures commonly seen in biomedical research
- provide clustering results by unsupervised learning, potentially reduce cost of medical data science research

### Exploratory Data Analysis
"Exploring your data does as much harm as not to explore it at all. -- Frank E Harrell" <br/>
Interactively visualize:
- the major statistical charactoristics of 1 variable
- the relationship between 2 or more variables
- the trajectory of each subject in a medical research cohort (maximum population 1000)

### Supervised Machine Learning
Following Regression Modeling Strstegy by Frank E Harrell, compute restricted cubic spline regression that satisfys most user cased in medical machine learning.
- train restricted cubic spline logistic regression if binary response is selected;
- train restricted cubic spline linear regression if continuous numeric response is selected;
- train non-linear univariate regression, and plot effect probablity against the percentile of each variable in heatmap.
- explore clues of predictor variables in terms of predicting the response variable, including correlation information, redundancy analysis, missingness and spearman squared correlation information each predictor carry;
- model development and evaluation use subject-wise 5-10 fold cross-validation;
- train multiple models every defined step size using given window size, report model performance and featuer importance overtime;

### Unsupervised Machine Learning
Unsupervise machine learning is designed to --
- cluster row-wise record, i.e. infant daily records, by optimizing the objective functions of a given mothed such as k-means, based on customized high-dimensional input variables
- detect outliers, by splitting minor cluster(s) from major cluster(s)
- explore and describe distinct charactoristics / reason of in clustered groups
