---
title: 'Gala: A Python package for galactic dynamics'
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
  - name: Verena S. Hoffmann
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

Our package implements the methodology explained by @schalk2022rocglm. It extends the
ROC-GLM [@pepe2000interpretation] to distributed data by using techniques of differential
privacy [@dwork2006calibrating] and the idea of just sharing highly aggregated values. Using
the package allows to evaluate a prognostic model based on an binary outcome. Therefore,
the main functionality makes it able to compute 1) the ROC curve using the ROC-GLM from
which 2) the AUC is derived. Furthermore, 3) confidence intervals based on
@delong1988comparing are estimated to conduct hypotheses testing of the estimated AUC.
Visualizing the approximated ROC curve and the described parts is also supported based on
[`ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot.html). Examples can be found in
the [README](https://github.com/difuture-lmu/dsROCGLM) file of the repository.

# Statement of need

Confidential data, such as patient data in medical research, plays a major role for
a variety of tasks. One of the main tasks of statistics or machine learning
is to gain insights by building statistical or prognostic models. But, using
confidential data comes with administrative burdens and mostly requires a consent
about using the data. Furthermore, the data can be distributed over multiple sites
(e.g. hospitals) which further complicates access to them. Modern approaches in
distributed computing allow to work on distributed confidential data by providing
frameworks that allow to directly work on the data without sharing information.

One of these frameworks is DataSHIELD [@gaye2014datashield] which allows to analyse
data in a non-disclosive private setting. The framework already provides, among others,
techniques for descriptive statistics, basic summary statistics, or basic statistical
modelling. Within a multiple sklerosis use-case to enhance patient medication in the
DIFUTURE consortia [@prasser2018difuture], we aim to develop a prognostic model and
validate it based on patient data distributed over five hospitals using DataSHIELD.
The missing part on DataSHIELD for this very general model developing-evaluation is the
evaluation which is planned to use ROC analysis.

In this package we close the gap between distributed model building and conducting ROC
analysis also on the distributed data. Therefore, our package seamlessly integrates into
the DataSHIELD framework.

__Technical Details:__ To ensure the functioning of our package on DataSHIELD, it is
constantly unit tested on an active DataSHIELD [test instance](opal-demo.obiba.org). The
reference, username, and password are available at the
[OPAL documentation](opaldoc.obiba.org/en/latest/resources.html) in the "Types" section.

__Related software:__ For assessing the model calibration in a distributed fashion, another
package for DataSHIELD called [dsCalibration](https://github.com/difuture-lmu/dsCalibration)
can be used. To upload models to the DataSHIELD servers and calculate predictions can be done
using [dsPredictBase](https://github.com/difuture-lmu/dsPredictBase).

# Acknowledgements

This work was supported by the German Federal Ministry of Education and Research (BMBF)
under Grant No. 01IS18036A and Federal Ministry for Research and Technology (BMFT) under
Grant No. 01ZZ1804C (DIFUTURE, MII). The authors of this work take full responsibilities
for its content.

# References
