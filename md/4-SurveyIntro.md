# 4-SurveyIntro


``` r
library(tidyverse)
library(srvyr)
```

# Survey Analysis Process

There is a general process for analyzing data to create estimates with
{srvyr} package:

1.  Create a tbl_svy object (a survey object) using: as_survey_design()
    or as_survey_rep()
2.  Subset data (if needed) using filter() (to create subpopulations)
3.  Specify domains of analysis using group_by()
4.  Within summarize(), specify variables to calculate, including means,
    totals, proportions, quantiles, and more

# Load the data

``` r
load("data/anes_2020.rda")
```

``` r
anes_2020 %>%
  select(-matches("V\\d")) %>%
  glimpse()
```

    Rows: 7,453
    Columns: 21
    $ CaseID                  <dbl> 200015, 200022, 200039, 200046, 200053, 200060…
    $ InterviewMode           <fct> Web, Web, Web, Web, Web, Web, Web, Web, Web, W…
    $ Weight                  <dbl> 1.0057375, 1.1634731, 0.7686811, 0.5210195, 0.…
    $ VarUnit                 <fct> 2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 1, 1, 2, 2, 2, 2…
    $ Stratum                 <fct> 9, 26, 41, 29, 23, 37, 7, 37, 32, 41, 22, 7, 3…
    $ CampaignInterest        <fct> Somewhat interested, Not much interested, Some…
    $ EarlyVote2020           <fct> NA, NA, NA, NA, NA, NA, NA, NA, Yes, NA, NA, N…
    $ VotedPres2016           <fct> Yes, Yes, Yes, Yes, Yes, No, Yes, No, Yes, Yes…
    $ VotedPres2016_selection <fct> Trump, Other, Clinton, Clinton, Trump, NA, Oth…
    $ PartyID                 <fct> Strong republican, Independent, Independent-de…
    $ TrustGovernment         <fct> Never, Never, Some of the time, About half the…
    $ TrustPeople             <fct> About half the time, Some of the time, Some of…
    $ Age                     <dbl> 46, 37, 40, 41, 72, 71, 37, 45, 70, 43, 37, 55…
    $ AgeGroup                <fct> 40-49, 30-39, 40-49, 40-49, 70 or older, 70 or…
    $ Education               <fct> Bachelor's, Post HS, High school, Post HS, Gra…
    $ RaceEth                 <fct> "Hispanic", "Asian, NH/PI", "White", "Asian, N…
    $ Gender                  <fct> Male, Female, Female, Male, Male, Female, Fema…
    $ Income                  <fct> "$175,000-249,999", "$70,000-74,999", "$100,00…
    $ Income7                 <fct> $125k or more, $60k to < 80k, $100k to < 125k,…
    $ VotedPres2020           <fct> NA, Yes, Yes, Yes, Yes, Yes, Yes, NA, Yes, Yes…
    $ VotedPres2020_selection <fct> NA, Other, Biden, Biden, Trump, Biden, Trump, …

``` r
load("data/recs_2020.rda")
```

``` r
recs_2020 %>%
  select(-matches("^NWEIGHT")) %>%
  glimpse()
```

    Rows: 18,496
    Columns: 39
    $ DOEID            <dbl> 100001, 100002, 100003, 100004, 100005, 100006, 10000…
    $ ClimateRegion_BA <fct> Mixed-Dry, Mixed-Humid, Mixed-Dry, Mixed-Humid, Mixed…
    $ Urbanicity       <fct> Urban Area, Urban Area, Urban Area, Urban Area, Urban…
    $ Region           <fct> West, South, West, South, Northeast, South, South, So…
    $ REGIONC          <chr> "WEST", "SOUTH", "WEST", "SOUTH", "NORTHEAST", "SOUTH…
    $ Division         <fct> Mountain South, West South Central, Mountain South, S…
    $ STATE_FIPS       <chr> "35", "05", "35", "45", "34", "48", "40", "28", "11",…
    $ state_postal     <fct> NM, AR, NM, SC, NJ, TX, OK, MS, DC, AZ, CA, TX, LA, M…
    $ state_name       <fct> New Mexico, Arkansas, New Mexico, South Carolina, New…
    $ HDD65            <dbl> 3844, 3766, 3819, 2614, 4219, 901, 3148, 1825, 4233, …
    $ CDD65            <dbl> 1679, 1458, 1696, 1718, 1363, 3558, 2128, 2374, 1159,…
    $ HDD30YR          <dbl> 4451, 4429, 4500, 3229, 4896, 1150, 3564, 2660, 4404,…
    $ CDD30YR          <dbl> 1027, 1305, 1010, 1653, 1059, 3588, 2043, 2164, 1407,…
    $ HousingUnitType  <fct> Single-family detached, Apartment: 5 or more units, A…
    $ YearMade         <ord> 1970-1979, 1980-1989, 1960-1969, 1980-1989, 1960-1969…
    $ TOTSQFT_EN       <dbl> 2100, 590, 900, 2100, 800, 4520, 2100, 900, 750, 760,…
    $ TOTHSQFT         <dbl> 2100, 590, 900, 2100, 800, 3010, 1200, 900, 750, 760,…
    $ TOTCSQFT         <dbl> 2100, 590, 900, 2100, 800, 3010, 1200, 0, 500, 760, 1…
    $ SpaceHeatingUsed <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,…
    $ ACUsed           <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE…
    $ HeatingBehavior  <fct> Set one temp and leave it, Turn on or off as needed, …
    $ WinterTempDay    <dbl> 70, 70, 69, 68, 68, 76, 74, 70, 68, 70, 72, 74, 74, 7…
    $ WinterTempAway   <dbl> 70, 65, 68, 68, 68, 76, 65, 70, 60, 70, 70, 74, 74, 7…
    $ WinterTempNight  <dbl> 68, 65, 67, 68, 68, 68, 74, 68, 62, 68, 72, 74, 74, 6…
    $ ACBehavior       <fct> Set one temp and leave it, Turn on or off as needed, …
    $ SummerTempDay    <dbl> 71, 68, 70, 72, 72, 69, 68, NA, 72, 74, 77, 77, 74, 6…
    $ SummerTempAway   <dbl> 71, 68, 68, 72, 72, 74, 70, NA, 76, 74, 77, 77, 74, 6…
    $ SummerTempNight  <dbl> 71, 68, 68, 72, 72, 68, 70, NA, 68, 72, 77, 77, 74, 6…
    $ BTUEL            <dbl> 42723.28, 17889.29, 8146.63, 31646.53, 20027.42, 4896…
    $ DOLLAREL         <dbl> 1955.06, 713.27, 334.51, 1424.86, 1087.00, 1895.66, 1…
    $ BTUNG            <dbl> 101924.43, 10145.32, 22603.08, 55118.66, 39099.51, 36…
    $ DOLLARNG         <dbl> 701.8300, 261.7348, 188.1400, 636.9100, 376.0400, 439…
    $ BTULP            <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,…
    $ DOLLARLP         <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,…
    $ BTUFO            <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,…
    $ DOLLARFO         <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,…
    $ BTUWOOD          <dbl> 0, 0, 0, 0, 0, 3000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ TOTALBTU         <dbl> 144647.71, 28034.61, 30749.71, 86765.19, 59126.93, 85…
    $ TOTALDOL         <dbl> 2656.8900, 975.0048, 522.6500, 2061.7700, 1463.0400, …

# Design Objects

## ANES

``` r
library(censusapi)
```

Get March 2020 census data for population.

``` r
cps_state_in <- getCensus(
  name = "cps/basic/mar",
  vintage = 2020,
  region = "state",
  vars = c(
    "HRMONTH", "HRYEAR4",
    "PRTAGE", "PRCITSHP", "PWSSWGT"
  ),
  key = Sys.getenv("CENSUS_KEY")
)

cps_state <- cps_state_in %>%
  as_tibble() %>%
  mutate(across(
    .cols = everything(),
    .fns = as.numeric
  ))
```

``` r
glimpse(cps_state)
```

    Rows: 104,878
    Columns: 6
    $ state    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ HRMONTH  <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3…
    $ HRYEAR4  <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2…
    $ PRTAGE   <dbl> 75, 26, 58, 4, 25, 17, 34, 17, 15, 13, 58, 69, 68, 85, 69, 35…
    $ PRCITSHP <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ PWSSWGT  <dbl> 1892.810, 2547.077, 2233.935, 4069.284, 2788.198, 2955.912, 2…

``` r
cps_state %>% distinct(HRMONTH, HRYEAR4)
```

    # A tibble: 1 × 2
      HRMONTH HRYEAR4
        <dbl>   <dbl>
    1       3    2020

Only include those 18 years or older and only US citizens.

``` r
cps_narrow_resp <- cps_state %>%
  filter(
    PRTAGE >= 18,
    PRCITSHP %in% c(1:4)
  )
```

To calculate the U.S. population from the filtered data, we sum the
person weights (PWSSWGT):

``` r
target_pop <- cps_narrow_resp %>%
  pull(PWSSWGT) %>%
  sum()
scales::comma(target_pop)
```

    [1] "231,034,125"

Using the anes_2020 data, we adjust the weighting variable (V200010b)
using the population of interest we just calculated (targetpop). We
determine the proportion of the total weight for each individual weight
(V200010b / sum(V200010b)) and then multiply that proportion by the
calculated population of interest.

``` r
anes_adj_wgt <- anes_2020 %>%
  mutate(weight = V200010b / sum(V200010b) * target_pop)
```

Once we have the adjusted weights, we can refer to the rest of the
documentation to create the survey design. The documentation indicates
that the study uses a stratified cluster sampling design. Therefore, we
need to specify variables for strata and ids (cluster) and fill in the
nest argument. The documentation provides guidance on which strata and
cluster variables to use depending on whether we are analyzing pre- or
post-election data. In this book, we analyze post-election data, so we
need to use the post-election weight V200010b, strata variable V200010d,
and Primary Sampling Unit (PSU)/cluster variable V200010c. Additionally,
we set nest=TRUE to ensure the clusters are nested within the strata.

``` r
anes_des <- anes_adj_wgt %>%
  as_survey_design(
    weights = weight,
    strata = V200010d,
    ids = V200010c,
    nest = T
  )
anes_des
```

    Stratified 1 - level Cluster Sampling design (with replacement)
    With (101) clusters.
    Called via srvyr
    Sampling variables:
      - ids: V200010c 
      - strata: V200010d 
      - weights: weight 
    Data variables: 
      - V200001 (dbl), CaseID (dbl), V200002 (hvn_lbll), InterviewMode (fct),
        V200010b (dbl), Weight (dbl), V200010c (dbl), VarUnit (fct), V200010d
        (dbl), Stratum (fct), V201006 (hvn_lbll), CampaignInterest (fct), V201023
        (hvn_lbll), EarlyVote2020 (fct), V201024 (hvn_lbll), V201025x (hvn_lbll),
        V201028 (hvn_lbll), V201029 (hvn_lbll), V201101 (hvn_lbll), V201102
        (hvn_lbll), VotedPres2016 (fct), V201103 (hvn_lbll),
        VotedPres2016_selection (fct), V201228 (hvn_lbll), V201229 (hvn_lbll),
        V201230 (hvn_lbll), V201231x (hvn_lbll), PartyID (fct), V201233 (hvn_lbll),
        TrustGovernment (fct), V201237 (hvn_lbll), TrustPeople (fct), V201507x
        (hvn_lbll), Age (dbl), AgeGroup (fct), V201510 (hvn_lbll), Education (fct),
        V201546 (hvn_lbll), V201547a (hvn_lbll), V201547b (hvn_lbll), V201547c
        (hvn_lbll), V201547d (hvn_lbll), V201547e (hvn_lbll), V201547z (hvn_lbll),
        V201549x (hvn_lbll), RaceEth (fct), V201600 (hvn_lbll), Gender (fct),
        V201607 (hvn_lbll), V201610 (hvn_lbll), V201611 (hvn_lbll), V201613
        (hvn_lbll), V201615 (hvn_lbll), V201616 (hvn_lbll), V201617x (hvn_lbll),
        Income (fct), Income7 (fct), V202051 (hvn_lbll), V202066 (hvn_lbll),
        V202072 (hvn_lbll), VotedPres2020 (fct), V202073 (hvn_lbll), V202109x
        (hvn_lbll), V202110x (hvn_lbll), VotedPres2020_selection (fct), weight
        (dbl)

## Residential Energy Consumption Survey

The RECS documentation (U.S. Energy Information Administration 2023b)
provides information on the survey’s sampling and weighting implications
for analysis. The documentation shows the 2020 RECS uses Jackknife
weights, where the main analytic weight is NWEIGHT, and the Jackknife
weights are NWEIGHT1-NWEIGHT60. We can specify these in the weights and
repweights arguments in the survey design object code, respectively.

With Jackknife weights, additional information is required: type, scale,
and mse. Chapter 10 discusses in depth each of these arguments; but to
quickly get started, the RECS documentation lets us know that type=JK1,
scale=59/60, and mse = TRUE.

``` r
recs_des <- recs_2020 %>%
  as_survey_rep(
    weights = NWEIGHT,
    repweights = NWEIGHT1:NWEIGHT60,
    type = "JK1",
    scale = 59 / 60,
    mse = T
  )
recs_des
```

    Call: Called via srvyr
    Unstratified cluster jacknife (JK1) with 60 replicates and MSE variances.
    Sampling variables:
      - repweights: `NWEIGHT1 + NWEIGHT2 + NWEIGHT3 + NWEIGHT4 + NWEIGHT5 +
        NWEIGHT6 + NWEIGHT7 + NWEIGHT8 + NWEIGHT9 + NWEIGHT10 + NWEIGHT11 +
        NWEIGHT12 + NWEIGHT13 + NWEIGHT14 + NWEIGHT15 + NWEIGHT16 + NWEIGHT17 +
        NWEIGHT18 + NWEIGHT19 + NWEIGHT20 + NWEIGHT21 + NWEIGHT22 + NWEIGHT23 +
        NWEIGHT24 + NWEIGHT25 + NWEIGHT26 + NWEIGHT27 + NWEIGHT28 + NWEIGHT29 +
        NWEIGHT30 + NWEIGHT31 + NWEIGHT32 + NWEIGHT33 + NWEIGHT34 + NWEIGHT35 +
        NWEIGHT36 + NWEIGHT37 + NWEIGHT38 + NWEIGHT39 + NWEIGHT40 + NWEIGHT41 +
        NWEIGHT42 + NWEIGHT43 + NWEIGHT44 + NWEIGHT45 + NWEIGHT46 + NWEIGHT47 +
        NWEIGHT48 + NWEIGHT49 + NWEIGHT50 + NWEIGHT51 + NWEIGHT52 + NWEIGHT53 +
        NWEIGHT54 + NWEIGHT55 + NWEIGHT56 + NWEIGHT57 + NWEIGHT58 + NWEIGHT59 +
        NWEIGHT60` 
      - weights: NWEIGHT 
    Data variables: 
      - DOEID (dbl), ClimateRegion_BA (fct), Urbanicity (fct), Region (fct),
        REGIONC (chr), Division (fct), STATE_FIPS (chr), state_postal (fct),
        state_name (fct), HDD65 (dbl), CDD65 (dbl), HDD30YR (dbl), CDD30YR (dbl),
        HousingUnitType (fct), YearMade (ord), TOTSQFT_EN (dbl), TOTHSQFT (dbl),
        TOTCSQFT (dbl), SpaceHeatingUsed (lgl), ACUsed (lgl), HeatingBehavior
        (fct), WinterTempDay (dbl), WinterTempAway (dbl), WinterTempNight (dbl),
        ACBehavior (fct), SummerTempDay (dbl), SummerTempAway (dbl),
        SummerTempNight (dbl), NWEIGHT (dbl), NWEIGHT1 (dbl), NWEIGHT2 (dbl),
        NWEIGHT3 (dbl), NWEIGHT4 (dbl), NWEIGHT5 (dbl), NWEIGHT6 (dbl), NWEIGHT7
        (dbl), NWEIGHT8 (dbl), NWEIGHT9 (dbl), NWEIGHT10 (dbl), NWEIGHT11 (dbl),
        NWEIGHT12 (dbl), NWEIGHT13 (dbl), NWEIGHT14 (dbl), NWEIGHT15 (dbl),
        NWEIGHT16 (dbl), NWEIGHT17 (dbl), NWEIGHT18 (dbl), NWEIGHT19 (dbl),
        NWEIGHT20 (dbl), NWEIGHT21 (dbl), NWEIGHT22 (dbl), NWEIGHT23 (dbl),
        NWEIGHT24 (dbl), NWEIGHT25 (dbl), NWEIGHT26 (dbl), NWEIGHT27 (dbl),
        NWEIGHT28 (dbl), NWEIGHT29 (dbl), NWEIGHT30 (dbl), NWEIGHT31 (dbl),
        NWEIGHT32 (dbl), NWEIGHT33 (dbl), NWEIGHT34 (dbl), NWEIGHT35 (dbl),
        NWEIGHT36 (dbl), NWEIGHT37 (dbl), NWEIGHT38 (dbl), NWEIGHT39 (dbl),
        NWEIGHT40 (dbl), NWEIGHT41 (dbl), NWEIGHT42 (dbl), NWEIGHT43 (dbl),
        NWEIGHT44 (dbl), NWEIGHT45 (dbl), NWEIGHT46 (dbl), NWEIGHT47 (dbl),
        NWEIGHT48 (dbl), NWEIGHT49 (dbl), NWEIGHT50 (dbl), NWEIGHT51 (dbl),
        NWEIGHT52 (dbl), NWEIGHT53 (dbl), NWEIGHT54 (dbl), NWEIGHT55 (dbl),
        NWEIGHT56 (dbl), NWEIGHT57 (dbl), NWEIGHT58 (dbl), NWEIGHT59 (dbl),
        NWEIGHT60 (dbl), BTUEL (dbl), DOLLAREL (dbl), BTUNG (dbl), DOLLARNG (dbl),
        BTULP (dbl), DOLLARLP (dbl), BTUFO (dbl), DOLLARFO (dbl), BTUWOOD (dbl),
        TOTALBTU (dbl), TOTALDOL (dbl)

# `SRVYR` and `DPLYR` packages

The towny dataset provides population data for municipalities in
Ontario, Canada on census years between 1996 and 2021. Taking a look at
towny with dplyr::glimpse(), we can see the dataset has 25 columns with
a mix of character and numeric data.

``` r
towny <- gt::towny
glimpse(towny)
```

    Rows: 414
    Columns: 25
    $ name                     <chr> "Addington Highlands", "Adelaide Metcalfe", "…
    $ website                  <chr> "https://addingtonhighlands.ca", "https://ade…
    $ status                   <chr> "lower-tier", "lower-tier", "lower-tier", "lo…
    $ csd_type                 <chr> "township", "township", "township", "township…
    $ census_div               <chr> "Lennox and Addington", "Middlesex", "Simcoe"…
    $ latitude                 <dbl> 45.00000, 42.95000, 44.13333, 45.52917, 43.85…
    $ longitude                <dbl> -77.25000, -81.70000, -79.93333, -76.89694, -…
    $ land_area_km2            <dbl> 1293.99, 331.11, 371.53, 519.59, 66.64, 116.6…
    $ population_1996          <int> 2429, 3128, 9359, 2837, 64430, 1027, 8315, 16…
    $ population_2001          <int> 2402, 3149, 10082, 2824, 73753, 956, 8593, 18…
    $ population_2006          <int> 2512, 3135, 10695, 2716, 90167, 958, 8654, 19…
    $ population_2011          <int> 2517, 3028, 10603, 2844, 109600, 864, 9196, 2…
    $ population_2016          <int> 2318, 2990, 10975, 2935, 119677, 969, 9680, 2…
    $ population_2021          <int> 2534, 3011, 10989, 2995, 126666, 954, 9949, 2…
    $ density_1996             <dbl> 1.88, 9.45, 25.19, 5.46, 966.84, 8.81, 21.22,…
    $ density_2001             <dbl> 1.86, 9.51, 27.14, 5.44, 1106.74, 8.20, 21.93…
    $ density_2006             <dbl> 1.94, 9.47, 28.79, 5.23, 1353.05, 8.22, 22.09…
    $ density_2011             <dbl> 1.95, 9.14, 28.54, 5.47, 1644.66, 7.41, 23.47…
    $ density_2016             <dbl> 1.79, 9.03, 29.54, 5.65, 1795.87, 8.31, 24.71…
    $ density_2021             <dbl> 1.96, 9.09, 29.58, 5.76, 1900.75, 8.18, 25.39…
    $ pop_change_1996_2001_pct <dbl> -0.0111, 0.0067, 0.0773, -0.0046, 0.1447, -0.…
    $ pop_change_2001_2006_pct <dbl> 0.0458, -0.0044, 0.0608, -0.0382, 0.2226, 0.0…
    $ pop_change_2006_2011_pct <dbl> 0.0020, -0.0341, -0.0086, 0.0471, 0.2155, -0.…
    $ pop_change_2011_2016_pct <dbl> -0.0791, -0.0125, 0.0351, 0.0320, 0.0919, 0.1…
    $ pop_change_2016_2021_pct <dbl> 0.0932, 0.0070, 0.0013, 0.0204, 0.0584, -0.01…

``` r
class(towny)
```

    [1] "tbl_df"     "tbl"        "data.frame"

``` r
library(survey)
```

The {survey} package contains datasets related to the California
Academic Performance Index, which measures student performance in
schools with at least 100 students in California. We can access these
datasets by loading the {survey} package and running data(api).

Let’s work with the apistrat dataset, which is a stratified random
sample, stratified by school type (stype) with three levels: E for
elementary school, M for middle school, and H for high school. We first
create the survey design object (see Chapter 10 for more information).
The sample is stratified by the stype variable and the sampling weights
are found in the pw variable. We can use this information to construct
the design object, apistrat_des.

``` r
data(api)

apistrat_des <- apistrat %>%
  as_survey_design(
    strata = stype,
    weights = pw
  )
class(apistrat_des)
```

    [1] "tbl_svy"        "survey.design2" "survey.design" 

## Function comparison examples

> Calculate mean and median

``` r
towny %>%
  summarise(
    area_mean = mean(land_area_km2),
    area_median = median(land_area_km2)
  )
```

    # A tibble: 1 × 2
      area_mean area_median
          <dbl>       <dbl>
    1      373.        273.

``` r
apistrat_des %>%
  summarize(
    api00_mean = survey_mean(api00),
    api00_median = survey_median(api00)
  )
```

    # A tibble: 1 × 4
      api00_mean api00_mean_se api00_median api00_median_se
           <dbl>         <dbl>        <dbl>           <dbl>
    1       662.          9.54          668            13.7

> Calculate across select columns

``` r
towny %>% summarise(across(
  starts_with("population"),
  ~ mean(.x, na.rm = T)
))
```

    # A tibble: 1 × 6
      population_1996 population_2001 population_2006 population_2011
                <dbl>           <dbl>           <dbl>           <dbl>
    1          25866.          27538.          29173.          30838.
    # ℹ 2 more variables: population_2016 <dbl>, population_2021 <dbl>

``` r
towny %>% summarise(across(
  starts_with("population"),
  ~ mean(.x, na.rm = T)
))
```

    # A tibble: 1 × 6
      population_1996 population_2001 population_2006 population_2011
                <dbl>           <dbl>           <dbl>           <dbl>
    1          25866.          27538.          29173.          30838.
    # ℹ 2 more variables: population_2016 <dbl>, population_2021 <dbl>

``` r
apistrat_des %>% summarise(across(
  starts_with("api"),
  survey_mean
))
```

    # A tibble: 1 × 6
      api00 api00_se api99 api99_se api.stu api.stu_se
      <dbl>    <dbl> <dbl>    <dbl>   <dbl>      <dbl>
    1  662.     9.54  629.     10.1    498.       16.4

> Mutate and filter

``` r
apistrat_des_mod <- apistrat_des %>%
  mutate(api_diff = api00 - api99) %>%
  filter(stype == "E") %>%
  select(stype, api00, api_diff, api_students = api.stu)

apistrat_des_mod
```

    Stratified Independent Sampling design (with replacement)
    Called via srvyr
    Sampling variables:
      - ids: `1` 
      - strata: stype 
      - weights: pw 
    Data variables: 
      - stype (fct), api00 (int), api_diff (int), api_students (int)

> Grouping

``` r
towny %>%
  group_by(csd_type) %>%
  dplyr::summarise(
    area_mean = mean(land_area_km2),
    area_median = median(land_area_km2)
  )
```

    # A tibble: 5 × 3
      csd_type     area_mean area_median
      <chr>            <dbl>       <dbl>
    1 city             498.        198. 
    2 municipality     607.        488. 
    3 town             183.        129. 
    4 township         363.        301. 
    5 village           23.0         3.3

``` r
apistrat_des %>%
  group_by(stype) %>%
  summarise(
    api00_mean = survey_mean(api00),
    api00_median = survey_median(api00)
  )
```

    # A tibble: 3 × 5
      stype api00_mean api00_mean_se api00_median api00_median_se
      <fct>      <dbl>         <dbl>        <dbl>           <dbl>
    1 E           674.          12.5          671            20.7
    2 H           626.          15.5          635            21.6
    3 M           637.          16.6          648            24.1

Alternative syntax

``` r
towny %>%
  dplyr::summarise(
    area_mean = mean(land_area_km2),
    area_median = median(land_area_km2),
    .by = csd_type
  )
```

    # A tibble: 5 × 3
      csd_type     area_mean area_median
      <chr>            <dbl>       <dbl>
    1 township         363.        301. 
    2 town             183.        129. 
    3 municipality     607.        488. 
    4 city             498.        198. 
    5 village           23.0         3.3

``` r
apistrat_des %>%
  summarise(
    api00_mean = survey_mean(api00),
    api00_median = survey_median(api00),
    .by = stype
  )
```

    # A tibble: 3 × 5
      stype api00_mean api00_mean_se api00_median api00_median_se
      <fct>      <dbl>         <dbl>        <dbl>           <dbl>
    1 E           674.          12.5          671            20.7
    2 H           626.          15.5          635            21.6
    3 M           637.          16.6          648            24.1
