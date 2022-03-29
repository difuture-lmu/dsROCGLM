---
title: 'dsROCGLM: Conducting distributed ROC analysis using DataSHIELD'
tags:
  - R
  - DataSHIELD
  - distributed Computing
  - medical tests
  - ROC-GLM
authors:
  - name: Daniel Schalk
    orcid: 0000-0003-0950-1947
    affiliation: "1, 3"
  - name: Verena Sophia Hoffmann
    affiliation: "2, 3"
  - name: Bernd Bischl
    affiliation: 1
  - name: Ulrich Mansmann
    affiliation: "2, 3"
affiliations:
 - name: Department of Statistics, LMU Munich, Munich, Germany
   index: 1
 - name: Institute for Medical Information Processing, Biometry and Epidemiology, LMU Munich, Munich, Germany
   index: 2
 - name: DIFUTURE (DataIntegration for Future Medicine, www.difuture.de), LMU Munich, Munich, Germany
   index: 3
date: 23 March 2022
bibliography: paper.bib
---

# Summary

Our `R` [@rcore] package `dsROCGLM` implements the methodology explained by @schalk2022rocglm. It extends the ROC-GLM [@pepe2000interpretation] to distributed data by using techniques of differential privacy [@dwork2006calibrating] and the idea of sharing highly aggregated values only. Using the package allows us to evaluate a prognostic model based on a binary outcome using the DataSHIELD [@gaye2014datashield] framework. Therefore, the main functionality makes it able to 1) compute the ROC curve using the ROC-GLM from which 2) the AUC is derived. Furthermore, 3) confidence intervals after @delong1988comparing are estimated to conduct hypothesis testing of the estimated AUC. Visualizing the approximated ROC curve, the AUC, and the confidence intervals is also supported based on [`ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot.html). Examples can be found in the [README](https://github.com/difuture-lmu/dsROCGLM) file of the repository.

# Statement of need

Privacy protection of patient data plays a major role for a variety of tasks in medical research. Uncontrolled release of health information may imply personal disadvantages for individuals. The individual patient needs to be protected that personal features become visible to people not authorized to know them.

In statistics or machine learning, one of these tasks is to gain insights by building statistical or prognostic models. Prognosis on the development of severe health conditions or covariates coding critical health information like genetic susceptibility need to be handled with care. Furthermore, using confidential data comes with administrative burdens and mostly requires a consent about using the data. Additionally, the data can be distributed over multiple sites (e.g. hospitals) which makes their access even more challenging. Modern approaches in distributed analysis allow to work on distributed confidential data by providing frameworks that allow retrieval of information without sharing sensitive information. These techniques aleviate many of the administrative, ethical and legal requirements on medical research.

One of these frameworks for privacy protected analysis is DataSHIELD [@gaye2014datashield]. It allows the analysis of data in a non-disclosive setting. The framework already provides, among others, techniques for descriptive statistics, basic summary statistics, or basic statistical modeling. Within a multiple sklerosis use-case to enhance patient medication in the DIFUTURE consortium of the German Medical Informatics Initiative [@prasser2018difuture], a prognostic model was developed on individual patient data. This model is to be validated using ROC analysis on patient data distributed across five hospitals using DataSHIELD. Distributed ROC analysis is currently not available in DataSHIELD.

In this package we close the gap between distributed model building and conducting ROC analysis also on the distributed data. Therefore, our package seamlessly integrates into the DataSHIELD framework.

__Technical details:__ To ensure the functioning of our package on DataSHIELD, it is constantly unit tested on an active DataSHIELD [test instance](opal-demo.obiba.org). The reference, username, and password are available at the [OPAL documentation](opaldoc.obiba.org/en/latest/resources.html) in the "Types" section.

__Related software:__ We also implemented the Brier score and calibration curves to assess the model calibration within the DataSHIELD framework. These functions are available in the [`dsCalibration`](https://github.com/difuture-lmu/dsCalibration) package. To upload models to the DataSHIELD servers and calculate predictions can be done using our [`dsPredictBase`](https://github.com/difuture-lmu/dsPredictBase) package.

# Acknowledgements

This work was supported by the German Federal Ministry of Education and Research (BMBF)
under Grant No. 01IS18036A and Federal Ministry for Research and Technology (BMFT) under
Grant No. 01ZZ1804C (DIFUTURE, MII). The authors of this work take full responsibilities
for its content.

# References

