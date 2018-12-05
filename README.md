# Modeling-Final-Project
Individual Final Project for IDS702 (Modeling and Representation of Data) course. 

## Introduction
As the regulatory landscape in the United States shifts towards a value-based care system, CMS (Center for Medicare and Medicaid Services) has begun implementing reimbursement models that rewards provider systems for delivering better care, as documented by their outcomes data. This transition is still in progress, and the metrics for various services are being created and modified constantly. 

In the nursing home system, quality has often been difficult to measure and improve. While some improvement has occurred in recent years, researchers have found that there is still a long way to go¹. Part of the difficulty lies in the fact that nursing homes know which factors are related to positive health broadly, but do not know how these factors directly relate to specific outcomes for patients and how to adjust their care management resources accordingly. This can be a key difference, especially between outcomes for short stay patients and long stay patients, who have very different health characteristics and require different resources. 

Previous research in this area looked at certain outcome measures and used linear regression techniques to investigate correlations of predictors with the outcomes. While their model had mixed performance, the researchers concluded that the quality measures contributed unique information and incorporated structural factors of healthcare, leading them to validate these measures. However, they also mentioned that there is a need to further understand nursing home behavior and characteristics in order to improve performance². 

## Objectives
The purpose of this research project was to understand more about the factors that influence various health outcomes in nursing homes, and how these factors differ between short-stay and long-stay measures.

In order to do this, we built various models and calculated the variable importance for each outcome. The top 5 variables were then ranked and weighted, giving scores across short stays, long stays, and total (both types). By understanding which factors are most related to better health outcomes, nursing homes can adjust their resources and strategy to focus on their weaknesses. Collecting data and creating a feedback cycle will ultimately benefit patients and bring in increased revenue from CMS reimbursement. 

This project focused on 6 outcome measures defined by the CMS: 3 long-stay and 3 short-stay. CMS uses length of stay to differentiate short stays (≤100 days) from long stays (> 100 days), and while research has been conducted to investigate other definitions, we will use this definition for our research purposes, since this is how CMS organizes their value-based reimbursements³.

These outcomes are listed below, along with their measure code:

Long Stay 
410: Percentage of long-stay residents experiencing one or more falls with major injury 
402: Percentage of long-stay residents who self-report moderate to severe pain 
551: Number of hospitalizations per 1000 long-stay resident days*   

Short Stay
424: Percentage of short-stay residents who self-report moderate to severe pain 
521: Percentage of short-stay residents who were rehospitalized after a nursing home admission* 
523: Percentage of short-stay residents who were successfully discharged to the community*

* Five Star Quality Measures

## Methodology

Three datasets were used from the Medicare Nursing Home Compare data repository:

Medicare Claims Quality Measures: Quality measures displayed on Nursing Home Compare that are based on Medicare claims data. Each row contains a specific quality measure for a specific nursing home and includes the risk-adjusted score.
MDS Quality Measures: Quality measures displayed on Nursing Home Compare that are based on the resident assessments that make up the nursing home Minimum Data Set (MDS). Each row contains a specific quality measure for a specific nursing home and includes the 4-quarter score average and scores for each individual quarter.
Provider Info: General information on currently active nursing homes, including number of certified beds, quality measure scores, staffing and other information. Data are presented as one row per nursing home.

15,613 nursing home provider organizations were included in the analysis, distributed all across the United States. 

## Results
The figures in the research poster show the weighted averages for the predictor variables in the logistic and multinomial logistic regression models. The linear regression models had high standard errors for the coefficients and low total R-squared values (< 0.1), which is why those coefficients were not ranked. For the logistic regression models, the AUC values ranged from 0.5479 to 0.6424, and binned residual plots validated the multinomial logistic regression models. Further analysis and supplementary plots can be seen in the full R Markdown report.

To calculate variable importance, coefficient values (with standard error) were used for logistic regression, and varIMP values (from the caret package in R) were used for the multinomial logistic regression, which takes into account model information when calculating variable importance. Ranking and weight calculations were performed in Excel, and complete calculations can be seen in the Variable Importance Excel report.

## Conclusions
While the linear regression model did not have high accuracy, the logistic and multinomial logistic regression models performed better, as shown by AUC scores and residual diagnostics. By ranking the predictors in these models and looking at how these vary by outcome type, we can see some clear patterns.

Short Stay
From our logistic regression model, we can see that LPN (Licensed Practical Nurse) and RN (Registered Nurse) staffing were the most important predictors, with Health Inspection Rating being important as well. From the multinomial logistic regression model, the predictors are more mixed in their importance, but RN staffing is still the most important predictor.

Long Stay
From our logistic regression model, we can see that RN and CNA (Certified Nursing Assistant) staffing were the most predictors, with Number of Certified Beds following closely behind. In the multinomial logistic regression model, having a Family Counsel is by far the most important predictor, with RN staffing and Most Severe Deficiency Cited: Potential for Minimal Harm being the next most important predictors.

Discussion
Not only did the short stay and long stay outcome models have different important predictors, but the nature of the outcome (ex. Top 50% vs. Grade) also changed the predictor rankings. This indicates that improving quality of care requires understanding which factors are most important for specific health outcomes, as well as identifying target goals for one’s own organization, which lines up with our initial hypothesis. 

Broadly speaking, increased staffing can help to improve short-stay measures, while having a family council can help to improve long-stay measures. But nursing homes might like to be more granular in their approach to care management, so by analyzing each model separately, organizations can understand the relationships between their factors and the health outcomes that matter most to their population. This project established and tested a framework for this purpose, and can be further modified in the future to include more predictors and health outcomes. Modeling can also be adapted and improved to include different predictors that could potentially improve the performance of the models.  

All additional materials and code are published on Github, including documentation and datasets (https://github.com/vkumaresan/Nursing-Home-Analysis). 

## References
Castle, Nicholas G and Jamie C Ferguson. “What is nursing home quality and how is it measured?” Gerontologist vol. 50,4 (2010): 426-42. 

Saliba, Debra et al. “Examination of the New Short-Stay Nursing Home Quality Measures: Rehospitalizations, Emergency Department Visits, and Successful Returns to the Community” Inquiry : a journal of medical care organization, provision and financing vol. 55 (2018): 46958018786816. 

Goodwin, James S et al. “Comparison of methods to identify long term care nursing home residence with administrative data” BMC health services research vol. 17,1 376. 30 May. 2017, doi:10.1186/s12913-017-2318-9

## Acknowledgements
Social Science Research Institute, Duke University
Master in Interdisciplinary Data Science Program, Duke University
Dr. Jerry Reiter, Duke University


