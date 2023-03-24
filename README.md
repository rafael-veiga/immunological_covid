# immunological_covid
code used in paper "Systems vaccinology identifies medical and immunological correlates of SARS-CoV-2 protection in organ transplantation recipients"

## Table of contents
* [Files description](#Files-description)
* [Necessary repositories](#Necessary-repositories)


## Files description
* **Tools.py** :  Set containing machine learning tools. The tools were parallelized and suitable for performing nested cross-validation. These implementations can be easily adapted and reused in different pipelines. A complete and current version can be found in the repository [https://github.com/rafael-veiga/tools_ML.git](https://github.com/rafael-veiga/tools_ML.git).
* **Clean_data.py** : All stages of cleaning and building the study database.
* **feature_selection.py** : Performing all feature selection steps by machine learning for each outcome.
* **features_eval.py** : Creates the machine learning models, evaluates the models to predict the outcomes, and extracts by measuring the most important features to predict.
* **figs.ipynb** : Constructions of figures 1, 3 and 5.
* **process_figs_1_2.py** : Analysis and data extraction to create figures 1 and 2.
* **get_filter_data.py** : Extract from the database clean the data needed for all analyzes and filter for the specific question.
* **table1.R** : Performs the analysis and builds table 1.
* **table2a.R** : Performs the analysis and builds table 2.
* **table2b.R** : Performs the analysis and builds table 2.
* **table2c.R** : Performs the analysis and builds table 2.
* **Figure4.R** : Performs the necessary analyzes and creates figure 4.

## Necessary repositories

* **plotly** : version 5.13.1
* **python** : version 3.9.16
* **pandas** : version  1.5.3
* **sklearn** : version 1.2.1
