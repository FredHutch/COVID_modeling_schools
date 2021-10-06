Hospitalization Data
================

CDC Hospitalization data
------------------------

Hospitalization Rates and Characteristics of Patients Hospitalized with Laboratory-Confirmed Coronavirus Disease 2019 — COVID-NET, 14 States, March 1–30, 2020

Accessed from <https://www.cdc.gov/mmwr/volumes/69/wr/mm6915e3.htm?s_cid=mm6915e3_w> on April 17, 2020.

"Among 167 patients with available data, the median interval from symptom onset to admission was 7 days."

This paper is stored in the references directory as mm6915e3\_H.pdf.

The paper references data that has been published on the following site. <https://gis.cdc.gov/grasp/covidnet/COVID19_3.html>

Currently, the script CDC\_HospData.R compiles the downloaded data locally and stores a slim csv for plotting.

![](HospitalizationData_files/figure-markdown_github/CDCplot-1.png)

IHME Hospitalization predictions
--------------------------------

IHME predictions were pulled on three separate dates in April: April 4, 9, and 12.

Currently, the script IHME\_HospPreds.R compiles the data locally and stores a slim csv for plotting only the states available in the CDC data.

![](HospitalizationData_files/figure-markdown_github/IHMEplot-1.png)
