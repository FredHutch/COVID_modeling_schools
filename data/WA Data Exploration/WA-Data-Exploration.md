---
title: "Washington Data Exploration"
author: "Mia Moore"
date: "4/5/2021"
output: 
  bookdown::word_document2:
    fig_caption: yes
    keep_md: yes
---

#Introduction
The purpose of this analysis is to perform a "quick" analysis of the King County Line Data to extract as many relevant model parameters as possible. Due to the simplifying assumptions made here, many of the parameters will have to be refit.


```
## png 
##   2
```

```
## png 
##   2
```



# Hospitalization Fatality Rate

First we estimate hospitalization fatality rate vs time using logistic regression. We assume
that the HFR depends on both age and the month of hospitalization.

\begin{align}
\text{logit(HFR)} = \beta_{\text{AGE}} + \beta_{\text{Month Admitted}}
\end{align}

The resulting fit shows a steadily decreasing HFR, although it has been largely stable since the summer \@ref(fig:HFRPlot). Note that I did not account for possible censoring so it is better to use the numbers from December than January (as the deaths from January admits will be incomplete)

![(\#fig:HFRPlot)Hospital fatality rate vs week. Dots show the proportion of individual admitted to hospital in a particular week with a recorded death data. Line shows the monthly fit from logistic regression.](WA-Data-Exploration_files/figure-docx/HFRPlot-1.png)







Table: (\#tab:unnamed-chunk-1)Estimated HFR by month (%)

|Month Name      |Age GR 1 |Age GR 2 |Age GR 3 |Age GR 4 |
|:---------------|:--------|:--------|:--------|:--------|
|March, 2020     |2.5      |5.8      |19.4     |47.7     |
|April, 2020     |2.3      |5.5      |18.6     |46.3     |
|May, 2020       |2.3      |5.5      |18.5     |46.1     |
|June, 2020      |2        |4.8      |16.5     |42.6     |
|July, 2020      |2.1      |5        |17.3     |44.1     |
|August, 2020    |1.5      |3.6      |12.8     |35.6     |
|September, 2020 |1.4      |3.4      |12.3     |34.6     |
|October, 2020   |1.5      |3.5      |12.6     |35.2     |
|November, 2020  |1.5      |3.5      |12.6     |35.3     |
|December, 2020  |1.4      |3.3      |11.9     |33.7     |
|January, 2021   |1.2      |3        |10.8     |31.3     |
|February, 2021  |1.1      |2.6      |9.4      |28.2     |

# Time from Hospitalization to Death

The negative slopes of the lines in \@ref(fig:hospdeath) give the sum of recovery and death ($\delta_H + \rho_H$). The steeper slope among older people potentially suggests a faster death rate rather than a lower recovery rate. When choosing parameters, note that

\begin{align}
\delta_H = \text{HFR} * \text{Slope}\\
\rho_H = (1 - \text{HFR}) * \text{Slope}
\end{align}

![(\#fig:hospdeath)Time from hospitalization to death](Time_to_death.png)


# Time from Symptom Onset to hospitalization

The negative slopes of the lines in \@ref(fig:symptomhosp) give the sum of death and hospitalization (note that we assume that recovery is not possible for these people) ($\delta_{NH} + \eta$). I have not yet investigated what is behind the change in the fraction of people dying outside of hospitals.



![(\#fig:symptomhosp)Time from symptom onset to hospitalization](Time_to_hospitalization.png)



# Time from Symptom Onset to diagnosis
The positive slopes of the lines in \@ref(fig:symptomdiag) is $\gamma_2 + \Delta_A$ whereas the negative slopes are $\rho + \Delta$.

We can also extract the ratio $\Delta_A/\Delta$ using a biexponential model. (NEED TO SHOW) Note that this analysis assumes that all of these parameters are constant in time, which we shall see shortly is likely a reasonable approximation, but is not exact.

![(\#fig:symptomdiag)Time from symptom onset to diagnosis](Days_since_symptoms.png)



# Diagnostic Rate
By analyzing the waiting times plotted in \@ref(fig:symptomdiag) we can derive a time verying diagnosis rate \@ref(fig:diagrate). This approximation relies on exponentially distributed waiting time (which seems to be empirically true) as well as a slow recovery rate (a value of roughly 1/7 was assumed). A higher recovery rate would distort these rates.

![(\#fig:diagrate)Diagnosis rate by month and age group](Diagnosis.png)




Table: (\#tab:unnamed-chunk-2)Diagnostic rate (1/day)

|SEVERITY     |Month Name      |Age 1 |Age 2 |Age 3 |Age 4 |
|:------------|:---------------|:-----|:-----|:-----|:-----|
|MILD         |March, 2020     |0.13  |0.12  |0.11  |0.09  |
|MILD         |April, 2020     |0.17  |0.16  |0.14  |0.12  |
|MILD         |May, 2020       |0.18  |0.17  |0.15  |0.13  |
|MILD         |June, 2020      |0.22  |0.2   |0.18  |0.16  |
|MILD         |July, 2020      |0.24  |0.23  |0.2   |0.18  |
|MILD         |August, 2020    |0.23  |0.21  |0.19  |0.17  |
|MILD         |September, 2020 |0.21  |0.2   |0.18  |0.16  |
|MILD         |October, 2020   |0.24  |0.22  |0.2   |0.18  |
|MILD         |November, 2020  |0.27  |0.25  |0.23  |0.2   |
|MILD         |December, 2020  |0.27  |0.25  |0.23  |0.2   |
|MILD         |January, 2021   |0.31  |0.29  |0.26  |0.23  |
|MILD         |February, 2021  |0.29  |0.27  |0.25  |0.22  |
|MILD         |March, 2021     |0.29  |0.27  |0.25  |0.21  |
|SEVERE_EARLY |March, 2020     |0.24  |0.18  |0.18  |0.2   |
|SEVERE_EARLY |April, 2020     |0.28  |0.21  |0.2   |0.23  |
|SEVERE_EARLY |May, 2020       |0.29  |0.22  |0.21  |0.23  |
|SEVERE_EARLY |June, 2020      |0.32  |0.24  |0.23  |0.26  |
|SEVERE_EARLY |July, 2020      |0.34  |0.26  |0.25  |0.28  |
|SEVERE_EARLY |August, 2020    |0.33  |0.25  |0.24  |0.27  |
|SEVERE_EARLY |September, 2020 |0.31  |0.24  |0.23  |0.26  |
|SEVERE_EARLY |October, 2020   |0.33  |0.25  |0.24  |0.27  |
|SEVERE_EARLY |November, 2020  |0.36  |0.28  |0.27  |0.29  |
|SEVERE_EARLY |December, 2020  |0.36  |0.28  |0.27  |0.3   |
|SEVERE_EARLY |January, 2021   |0.39  |0.3   |0.29  |0.32  |
|SEVERE_EARLY |February, 2021  |0.38  |0.3   |0.29  |0.32  |
|SEVERE_EARLY |March, 2021     |0.38  |0.3   |0.29  |0.32  |
|SEVERE_LATE  |March, 2020     |0.44  |0.41  |0.43  |0.47  |
|SEVERE_LATE  |April, 2020     |0.41  |0.38  |0.39  |0.43  |
|SEVERE_LATE  |May, 2020       |0.33  |0.31  |0.32  |0.35  |
|SEVERE_LATE  |June, 2020      |0.36  |0.33  |0.34  |0.38  |
|SEVERE_LATE  |July, 2020      |0.4   |0.37  |0.38  |0.42  |
|SEVERE_LATE  |August, 2020    |0.34  |0.32  |0.33  |0.36  |
|SEVERE_LATE  |September, 2020 |0.31  |0.28  |0.29  |0.32  |
|SEVERE_LATE  |October, 2020   |0.27  |0.26  |0.26  |0.29  |
|SEVERE_LATE  |November, 2020  |0.39  |0.36  |0.37  |0.41  |
|SEVERE_LATE  |December, 2020  |0.44  |0.41  |0.42  |0.46  |
|SEVERE_LATE  |January, 2021   |0.47  |0.44  |0.46  |0.5   |
|SEVERE_LATE  |February, 2021  |0.64  |0.61  |0.62  |0.68  |
|SEVERE_LATE  |March, 2021     |0.74  |0.7   |0.72  |0.77  |

# Fraction Severe
Probably the trickiest parameter to extract from the data is the fraction of symptomatic infections which are severe ($1 - m$ in the model). Severe cases are defined to be those with a hospitalization and/or death event. We can readily extract the fraction of cases with a severe event \@ref(fig:SEVPlot) but this will be an overestimate of the severe fraction due to underascertainment of mild cases and asymptomatic cases.




![(\#fig:SEVPlot)Hospitalization fraction vs week. Dots show the proportion of symptomatic infections in a particular week which result in hospitalization and/or death. Line shows the monthly fit from logistic regression.](WA-Data-Exploration_files/figure-docx/SEVPlot-1.png)


Using the diagnostic rates from the previous sections, as well as an estimate of the proportion of cases which are asymptomatic, I derived the month-by-month estimates of severe fraction below.




Table: (\#tab:unnamed-chunk-3)Fraction of symptomatic infection which is severe (%)

|Month Name      |Age GR 1 |Age GR 2 |Age GR 3 |Age GR 4 |
|:---------------|:--------|:--------|:--------|:--------|
|March, 2020     |0.5      |1.5      |7.1      |32.3     |
|April, 2020     |0.5      |1.5      |6.9      |31.8     |
|May, 2020       |0.4      |1.2      |5.5      |27.1     |
|June, 2020      |0.3      |1        |4.7      |24.1     |
|July, 2020      |0.3      |0.9      |4.3      |22.5     |
|August, 2020    |0.3      |0.9      |4.3      |22.2     |
|September, 2020 |0.3      |0.8      |3.9      |20.8     |
|October, 2020   |0.2      |0.7      |3.4      |18.4     |
|November, 2020  |0.2      |0.7      |3.1      |17.1     |
|December, 2020  |0.2      |0.7      |3.2      |17.4     |
|January, 2021   |0.2      |0.7      |3.4      |18.6     |
|February, 2021  |0.3      |0.9      |4.3      |22.3     |
