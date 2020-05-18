# covid-19-analysis

## About full_dataset_for_modelling.csv

This csv was updated on 18 May 2020 at 10:30am

It merges the following datasets for each country into one file:

  1)  Covid-19 tests, cases, and deaths data from https://github.com/owid/covid-19-data/tree/master/public/data
  2)  A Covid-19 "age adjusted risk" factor, as calculated by using population pyramids from https://www.populationpyramid.net/ in conjunction with data on the spread of Coronavirus deaths by 10-year age group.
  3)  Percentage of people that are obese (BMI >30) per country, as obtained from the WHO website
  4)  GDP per capita (PPP) data from the IMF
  5)  Population density, and an inequality of density distribution measure* as calculated by Differential Capital (country_density_dat.csv)
  6)  Government response to Covid-19 data from https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker
  
And creates a number of new fields, based on the above data:

  7)  days_since_outbreak - days since first case was registered in the given country
  8)  days_since_deaths_crossed_0.1_per_million
  9)  days_since_10_deaths
  10) gov_response_at_0.1_deaths_per_million - StringencyIndex at the point where Deaths-per-million crossed 0.1
  11) deaths_5_day_averages, infections_5_day_averages - Average of deaths per day and average of cases per day over previous 4 days and current day.
  12) peak_deaths_already_occurred_indicator (& peak_cases_already_occurred_indicator) - 1 if deaths_5_day_averages (& infections_5_day_averages) had its maximum at least 4 days before last day of data, 0 otherwise.

If you wish to update the data yourself to include new data from sources (1) and (6), simply run the first 3 scripts in this repository. This will fetch the latest data from those sources and recalculate (7) - (12).

*This inequality measure (ineq) ranges from 0 to 1 in country_density_dat.csv, with 0 being completely equal spread and 1 being completely unequal spread. In full_dataset_for_modelling.csv this measure has been transformed to be 1 / (1 - ineq).

## About the Simple Regression Model

This script shows the results of some linear models that attempt to explain the cumulative number of deaths at the time of the peak for a given country, using some combination of fields from our dataset.