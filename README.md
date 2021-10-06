# COVID_modeling

Develop and parameterize a mathematical model of COVID-19 transmission in King County and use it to test the effectiveness of different test-and-treat interventions. 

Curently uploaded:
1. *HutchCOVID*: Our R package. These core files should not be edited without permission of Chloe or Mia. See instruction below on how to build.
2. *R-proj*: Some important core scripts including instructions on how to run the model, and some default parameter sets. Should not be edited without permission of Chloe or Mia.
3. *data*: The data used to calibrate the model parameters and set initial conditions. In addition contains some simple data analysis and parameter estimation.
4. *App_Forecasts*: Scripts necessary to build our "pre-run" scenarios for our shiny app.
5. *App_Function*: Development of function to be run natively on the App.
6. *Long Term Projections*: Scripts for investigating long-term projections of COVID.
7. *refs*: An out-of-date list of references.
8. *Vaccine Rollout*: Scripts for exploring interaction of school reopening and childhood vaccinations.
9. *Variant Project*: Scripts for exploring interactions between variants.
10. *Vaccine Test*: Scripts for testing proposed changes to vaccination rates.

Note that *matlab* is no longer supported.


# Using the model (for novices)

## Cloning the Repo
1. Create a new project : File --> New project --> Version Control  , then use this URL:  https://github.com/FredHutch/COVID_modeling.git 
2. Once the new project is created in R studio : go to the Git tab in the upper right, and then open the shell
3. Look at all branches : git branch -a
4. Switch to a different branch of your choice (in this case version-2.0) :  git checkout version-2.0

## Installing the package

There are several ways to do this.

### Use devtools

1. install.packages("devtools") [do once]
2. library(devtools)
3. install_github("FredHutch/COVID_modeling", ref = "version-2.0", subdir = "/HutchCOVID")
