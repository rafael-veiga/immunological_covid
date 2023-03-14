# immunological_covid
code used in paper "Systems vaccinology identifies medical and immunological correlates of SARS-CoV-2 protection in organ transplantation recipients"

## Table of contents
* [Files description](##Files-description)

## Files description
* **Tools.py** :  Set containing machine tool learning techniques. The tools were parallelized and suitable for performing nested cross-validation. These implementations can be easily adapted and reused in different pipelines. A complete and current version can be found in the repository [https://github.com/rafael-veiga/tools_ML.git](https://github.com/rafael-veiga/tools_ML.git).
* **Clean_data.py** : All stages of cleaning and building the study database.
* **feature_selection.py** : Performing all feature selection steps by machine learning for each outcome.
* **features_eval.py** : Creates the machine learning models, evaluates the models to predict the outcomes, and extracts by measuring the most important features to predict.
* **figs.ipynb** : Constructions of figures 1, 3 and 5.
* 
