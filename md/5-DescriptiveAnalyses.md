# 4-Descriptive Analyses


# Setup

Reference prior chapter

``` r
library(tidyverse)
library(srvyr)
library(broom)
load("data/anes_2020.rda")
load("data/recs_2020.rda")
```

``` r
target_pop <- 231592693

anes_des <- anes_2020 %>%
  mutate(weight = sum(Weight) * target_pop) %>%
  as_survey_design(
    weights = weight,
    strata = Stratum,
    ids = VarUnit,
    nest = T
  )
```

``` r
recs_des <- recs_2020 %>%
  as_survey_rep(
    weights = NWEIGHT,
    repweights = NWEIGHT1:NWEIGHT60,
    type = "JK1",
    scale = 59 / 60,
    mse = T
  )
```

# Intro

This chapter discusses how to analyze measures of distribution (e.g.,
cross-tabulations), central tendency (e.g., means), relationship (e.g.,
ratios), and dispersion (e.g., standard deviation) using functions from
the {srvyr} package (Freedman Ellis and Schneider 2024).

Measures of distribution describe how often an event or response occurs.
These measures include counts and totals. We cover the following
functions:

- Count of observations (survey_count() and survey_tally())
- Summation of variables (survey_total())

Measures of central tendency find the central (or average) responses.
These measures include means and medians. We cover the following
functions:

- Means and proportions (survey_mean() and survey_prop())
- Quantiles and medians (survey_quantile() and survey_median())

Measures of relationship describe how variables relate to each other.
These measures include correlations and ratios. We cover the following
functions:

- Correlations (survey_corr())
- Ratios (survey_ratio())

Measures of dispersion describe how data spread around the central
tendency for continuous variables. These measures include standard
deviations and variances. We cover the following functions:

- Variances and standard deviations (survey_var() and survey_sd())

# Counts and Cross-tabulations

    survey_count(
      x,
      ...,
      wt = NULL,
      sort = FALSE,
      name = "n",
      .drop = dplyr::group_by_drop_default(x),
      vartype = c("se", "ci", "var", "cv")
      )

- x: a tbl_svy object created by as_survey
- …: variables to group by, passed to group_by
- wt: a variable to weight on in addition to the survey weights,
  defaults to NULL
- sort: how to sort the variables, defaults to FALSE
- name: the name of the count variable, defaults to n
- .drop: whether to drop empty groups
- vartype: type(s) of variation estimate to calculate including any of
  c(“se”, “ci”, “var”, “cv”), defaults to se (standard error)

<!-- -->

    survey_tally(
      x,
      wt,
      sort = FALSE,
      name = "n",
      vartype = c("se", "ci", "var", "cv")
    )

Both functions include the vartype argument with four different values:

- se: standard error
  - The estimated standard deviation of the estimate
  - Output has a column with the variable name specified in the name
    argument with a suffix of “\_se”
- ci: confidence interval
  - The lower and upper limits of a confidence interval
  - Output has two columns with the variable name specified in the name
    argument with a suffix of “\_low” and “\_upp”
  - By default, this is a 95% confidence interval but can be changed by
    using the argument level and specifying a number between 0 and 1.
    For example, level=0.8 would produce an 80% confidence interval.
- var: variance
  - The estimated variance of the estimate
  - Output has a column with the variable name specified in the name
    argument with a suffix of “\_var”
- cv: coefficient of variation
  - A ratio of the standard error and the estimate
  - Output has a column with the variable name specified in the name
    argument with a suffix of “\_cv”

Note that specifying df = Inf is equivalent to using a normal (z-based)
confidence interval – this is the default in {survey}.

## Estimated Population Count

``` r
recs_des %>%
  survey_count()
```

    # A tibble: 1 × 2
               n  n_se
           <dbl> <dbl>
    1 123529025. 0.148

``` r
recs_des %>%
  survey_tally()
```

    # A tibble: 1 × 2
               n  n_se
           <dbl> <dbl>
    1 123529025. 0.148

## Estimated counts by subgroups (cross-tabs)

``` r
recs_des %>%
  survey_count(Region, Division, name = "N") # Changes count variable name
```

    # A tibble: 10 × 4
       Region    Division                   N         N_se
       <fct>     <fct>                  <dbl>        <dbl>
     1 Northeast New England         5876166  0.0000000137
     2 Northeast Middle Atlantic    16043503  0.0000000487
     3 Midwest   East North Central 18546912  0.000000437 
     4 Midwest   West North Central  8495815  0.0000000177
     5 South     South Atlantic     24843261  0.0000000418
     6 South     East South Central  7380717. 0.114       
     7 South     West South Central 14619094  0.000488    
     8 West      Mountain North      4615844  0.119       
     9 West      Mountain South      4602070  0.0000000492
    10 West      Pacific            18505643. 0.00000295  

Or

``` r
recs_des %>%
  group_by(Region, Division) %>%
  survey_tally(name = "N")
```

    # A tibble: 10 × 4
    # Groups:   Region [4]
       Region    Division                   N         N_se
       <fct>     <fct>                  <dbl>        <dbl>
     1 Northeast New England         5876166  0.0000000137
     2 Northeast Middle Atlantic    16043503  0.0000000487
     3 Midwest   East North Central 18546912  0.000000437 
     4 Midwest   West North Central  8495815  0.0000000177
     5 South     South Atlantic     24843261  0.0000000418
     6 South     East South Central  7380717. 0.114       
     7 South     West South Central 14619094  0.000488    
     8 West      Mountain North      4615844  0.119       
     9 West      Mountain South      4602070  0.0000000492
    10 West      Pacific            18505643. 0.00000295  

# Totals and Sums

    survey_total(
      x,
      na.rm = FALSE,
      vartype = c("se", "ci", "var", "cv"),
      level = 0.95,
      deff = FALSE,
      df = NULL
    )

- x: a variable, expression, or empty
- na.rm: an indicator of whether missing values should be dropped,
  defaults to FALSE
- vartype: type(s) of variation estimate to calculate including any of
  c(“se”, “ci”, “var”, “cv”), defaults to se (standard error) (see
  Section 5.2.1 for more information)
- level: a number or a vector indicating the confidence level, defaults
  to 0.95
- deff: a logical value stating whether the design effect should be
  returned, defaults to FALSE (this is described in more detail in
  Section 5.9.3)
- df: (for vartype = ‘ci’), a numeric value indicating degrees of
  freedom for the t-distribution

## Estimated Population Count

``` r
recs_des %>%
  summarize(tot = survey_total())
```

    # A tibble: 1 × 2
             tot tot_se
           <dbl>  <dbl>
    1 123529025.  0.148

## Overall summation of continuous variables

``` r
recs_des %>%
  summarise(elec_bill = survey_total(DOLLAREL))
```

    # A tibble: 1 × 2
          elec_bill elec_bill_se
              <dbl>        <dbl>
    1 170473527909.   664893504.

It is estimated that American residential households spent a total of
\$170,473,527,909 on electricity in 2020, and the estimate has a
standard error of \$664,893,504.

## Summation by groups

``` r
recs_des %>%
  group_by(Region) %>%
  summarise(elec_bill = survey_total(DOLLAREL, vartype = "ci"))
```

    # A tibble: 4 × 4
      Region       elec_bill elec_bill_low elec_bill_upp
      <fct>            <dbl>         <dbl>         <dbl>
    1 Northeast 29430369947.  28788987554.  30071752341.
    2 Midwest   34972544751.  34339576041.  35605513460.
    3 South     72496840204.  71534780902.  73458899506.
    4 West      33573773008.  32909111702.  34238434313.

# Means and Proportions

    survey_mean(
      x,
      na.rm = FALSE,
      vartype = c("se", "ci", "var", "cv"),
      level = 0.95,
      proportion = FALSE,
      prop_method = c("logit", "likelihood", "asin", "beta", "mean"),
      deff = FALSE,
      df = NULL
    )

    survey_prop(
      na.rm = FALSE,
      vartype = c("se", "ci", "var", "cv"),
      level = 0.95,
      proportion = TRUE,
      prop_method = 
        c("logit", "likelihood", "asin", "beta", "mean", "xlogit"),
      deff = FALSE,
      df = NULL
    )

- na.rm: an indicator of whether missing values should be dropped,
  defaults to FALSE
- vartype: type(s) of variation estimate to calculate including any of
  c(“se”, “ci”, “var”, “cv”), defaults to se (standard error) (see
  Section 5.2.1 for more information)
- level: a number or a vector indicating the confidence level, defaults
  to 0.95
- prop_method: Method to calculate the confidence interval for
  confidence intervals
- deff: a logical value stating whether the design effect should be
  returned, defaults to FALSE (this is described in more detail in
  Section 5.9.3)
- df: (for vartype = ‘ci’), a numeric value indicating degrees of
  freedom for the t-distribution

The confidence interval used for most measures, such as means and
counts, is referred to as a Wald-type interval. However, for
proportions, a Wald-type interval with a symmetric t-based confidence
interval may not provide accurate coverage, especially when dealing with
small sample sizes or proportions “near” 0 or 1. We can use other
methods to calculate confidence intervals, which we specify using the
prop_method option in survey_prop(). The options include:

- logit: fits a logistic regression model and computes a Wald-type
  interval on the log-odds scale, which is then transformed to the
  probability scale. This is the default method.
- likelihood: uses the (Rao-Scott) scaled chi-squared distribution for
  the log-likelihood from a binomial distribution.
- asin: uses the variance-stabilizing transformation for the binomial
  distribution, the arcsine square root, and then back-transforms the
  interval to the probability scale.
- beta: uses the incomplete beta function with an effective sample size
  based on the estimated variance of the proportion.
- mean: the Wald-type interval ($\pm t^∗_{df}×SE$).
- xlogit: uses a logit transformation of the proportion, calculates a
  Wald-type interval, and then back-transforms to the probability scale.
  This method is the same as those used by default in SUDAAN and SPSS.

## One variable proportion

Proportion of people in each region.

``` r
recs_des %>%
  group_by(Region) %>%
  summarize(p = survey_prop())
```

    # A tibble: 4 × 3
      Region        p     p_se
      <fct>     <dbl>    <dbl>
    1 Northeast 0.177 2.12e-10
    2 Midwest   0.219 2.62e-10
    3 South     0.379 7.40e-10
    4 West      0.224 8.16e-10

``` r
recs_des %>%
  group_by(Region) %>%
  summarize(p = survey_mean())
```

    # A tibble: 4 × 3
      Region        p     p_se
      <fct>     <dbl>    <dbl>
    1 Northeast 0.177 2.12e-10
    2 Midwest   0.219 2.62e-10
    3 South     0.379 7.40e-10
    4 West      0.224 8.16e-10

The `survey_prop()` function is essentially the same as using
`survey_mean()` with a categorical variable and without specifying a
numeric variable in the x argument.

## Conditional Proportions

``` r
recs_des %>%
  group_by(Region, ACUsed) %>%
  summarize(p = survey_prop())
```

    # A tibble: 8 × 4
    # Groups:   Region [4]
      Region    ACUsed      p    p_se
      <fct>     <lgl>   <dbl>   <dbl>
    1 Northeast FALSE  0.110  0.00590
    2 Northeast TRUE   0.890  0.00590
    3 Midwest   FALSE  0.0666 0.00508
    4 Midwest   TRUE   0.933  0.00508
    5 South     FALSE  0.0581 0.00278
    6 South     TRUE   0.942  0.00278
    7 West      FALSE  0.255  0.00759
    8 West      TRUE   0.745  0.00759

## Joint Proportions

``` r
recs_des %>%
  group_by(interact(Region, ACUsed)) %>%
  summarize(p = survey_prop())
```

    # A tibble: 8 × 4
      Region    ACUsed      p    p_se
      <fct>     <lgl>   <dbl>   <dbl>
    1 Northeast FALSE  0.0196 0.00105
    2 Northeast TRUE   0.158  0.00105
    3 Midwest   FALSE  0.0146 0.00111
    4 Midwest   TRUE   0.204  0.00111
    5 South     FALSE  0.0220 0.00106
    6 South     TRUE   0.357  0.00106
    7 West      FALSE  0.0573 0.00170
    8 West      TRUE   0.167  0.00170

## Overall Mean

``` r
recs_des %>%
  summarise(elec_bill = survey_mean(
    DOLLAREL,
    vartype = c("se", "ci")
  ))
```

    # A tibble: 1 × 4
      elec_bill elec_bill_se elec_bill_low elec_bill_upp
          <dbl>        <dbl>         <dbl>         <dbl>
    1     1380.         5.38         1369.         1391.

## Means by subgroup

``` r
recs_des %>%
  group_by(Region) %>%
  summarize(elec_bill = survey_mean(DOLLAREL))
```

    # A tibble: 4 × 3
      Region    elec_bill elec_bill_se
      <fct>         <dbl>        <dbl>
    1 Northeast     1343.         14.6
    2 Midwest       1293.         11.7
    3 South         1548.         10.3
    4 West          1211.         12.0

# Quantiles and Medians

    survey_quantile(
      x,
      quantiles,
      na.rm = FALSE,
      vartype = c("se", "ci", "var", "cv"),
      level = 0.95,
      interval_type = 
        c("mean", "beta", "xlogit", "asin", "score", "quantile"),
      qrule = c("math", "school", "shahvaish", "hf1", "hf2", "hf3", 
                "hf4", "hf5", "hf6", "hf7", "hf8", "hf9"),
      df = NULL
    )

    survey_median(
      x,
      na.rm = FALSE,
      vartype = c("se", "ci", "var", "cv"),
      level = 0.95,
      interval_type = 
        c("mean", "beta", "xlogit", "asin", "score", "quantile"),
      qrule = c("math", "school", "shahvaish", "hf1", "hf2", "hf3", 
                "hf4", "hf5", "hf6", "hf7", "hf8", "hf9"),
      df = NULL
    )

The arguments available in both functions are:

- x: a variable, expression, or empty
- na.rm: an indicator of whether missing values should be dropped,
  defaults to FALSE
- vartype: type(s) of variation estimate to calculate, defaults to se
  (standard error)
- level: a number or a vector indicating the confidence level, defaults
  to 0.95
- interval_type: method for calculating a confidence interval
- qrule: rule for defining quantiles. The default is the lower end of
  the quantile interval (“math”). The midpoint of the quantile interval
  is the “school” rule. “hf1” to “hf9” are weighted analogs to type=1 to
  9 in quantile(). “shahvaish” corresponds to a rule proposed by Shah
  and Vaish (2006). See vignette(“qrule”, package=“survey”) for more
  information.
- df: (for vartype = ‘ci’), a numeric value indicating degrees of
  freedom for the t-distribution

Quantiles also have two more methods available:

- score: the Francisco and Fuller confidence interval based on inverting
  a score test (only available for design-based survey objects and not
  replicate-based objects)
- quantile: based on the replicates of the quantile. This is not valid
  for jackknife-type replicates but is available for bootstrap and BRR
  replicates.

## Overall Quartiles

``` r
recs_des %>%
  summarise(
    elec_bill = survey_quantile(DOLLAREL,
      quantiles = c(0.25, 0.5, 0.75)
    )
  )
```

    # A tibble: 1 × 6
      elec_bill_q25 elec_bill_q50 elec_bill_q75 elec_bill_q25_se elec_bill_q50_se
              <dbl>         <dbl>         <dbl>            <dbl>            <dbl>
    1          795.         1215.         1770.             5.69             6.33
    # ℹ 1 more variable: elec_bill_q75_se <dbl>

## Quartiles by Subgroup

``` r
recs_des %>%
  group_by(Region) %>%
  summarise(elec_bill = survey_quantile(DOLLAREL,
    quantiles = c(0.25, .5, .75)
  ))
```

    # A tibble: 4 × 7
      Region    elec_bill_q25 elec_bill_q50 elec_bill_q75 elec_bill_q25_se
      <fct>             <dbl>         <dbl>         <dbl>            <dbl>
    1 Northeast          740.         1148.         1712.            13.7 
    2 Midwest            769.         1149.         1632.             8.88
    3 South              968.         1402.         1945.            10.6 
    4 West               623.         1028.         1568.            10.8 
    # ℹ 2 more variables: elec_bill_q50_se <dbl>, elec_bill_q75_se <dbl>

## Minimum and Maximum

``` r
recs_des %>%
  summarize(elec_bill = survey_quantile(DOLLAREL,
    quantiles = c(0, 1)
  ))
```

    # A tibble: 1 × 4
      elec_bill_q00 elec_bill_q100 elec_bill_q00_se elec_bill_q100_se
              <dbl>          <dbl>            <dbl>             <dbl>
    1         -889.         15680.              NaN                 0

Some customers sell energy back to the grid, hence the negative number.

## Overall Median

``` r
recs_des %>%
  summarize(elec_bill = survey_median(DOLLAREL))
```

    # A tibble: 1 × 2
      elec_bill elec_bill_se
          <dbl>        <dbl>
    1     1215.         6.33

## Medians by Subgroup

``` r
recs_des %>%
  group_by(Region) %>%
  summarize(elec_bill = survey_median(DOLLAREL))
```

    # A tibble: 4 × 3
      Region    elec_bill elec_bill_se
      <fct>         <dbl>        <dbl>
    1 Northeast     1148.        16.6 
    2 Midwest       1149.        11.6 
    3 South         1402.         9.17
    4 West          1028.        14.3 

# Ratios

    survey_ratio(
      numerator,
      denominator,
      na.rm = FALSE,
      vartype = c("se", "ci", "var", "cv"),
      level = 0.95,
      deff = FALSE,
      df = NULL
    )

- numerator: The numerator of the ratio
- denominator: The denominator of the ratio
- na.rm: A logical value to indicate whether missing values should be
  dropped
- vartype: type(s) of variation estimate to calculate including any of
  c(“se”, “ci”, “var”, “cv”), defaults to se (standard error) (see
  Section 5.2.1 for more information)
- level: A single number or vector of numbers indicating the confidence
  level
- deff: A logical value to indicate whether the design effect should be
  returned (this is described in more detail in Section 5.9.3)
- df: (For vartype = “ci” only) A numeric value indicating the degrees
  of freedom for t-distribution

## Overall Ratios

Suppose we wanted to find the ratio of dollars spent on liquid propane
per unit (in British thermal unit \[Btu\]) nationally6. To find the
average cost to a household, we can use survey_mean(). However, to find
the national unit rate, we can use survey_ratio(). I

``` r
recs_des %>%
  summarize(
    DOLLARLP_Tot = survey_total(DOLLARLP, vartype = NULL),
    BTULP_Tot = survey_total(BTULP, vartype = NULL),
    DOL_BTU_Rat = survey_ratio(DOLLARLP, BTULP),
    DOL_BTU_Avg = survey_mean(DOLLARLP / BTULP, na.rm = T)
  )
```

    # A tibble: 1 × 6
      DOLLARLP_Tot   BTULP_Tot DOL_BTU_Rat DOL_BTU_Rat_se DOL_BTU_Avg DOL_BTU_Avg_se
             <dbl>       <dbl>       <dbl>          <dbl>       <dbl>          <dbl>
    1  8122911173.     3.91e11      0.0208       0.000240      0.0240       0.000223

## Ratios by Subgroup

``` r
recs_des %>%
  group_by(Region) %>%
  summarize(DOL_BTU_Rat = survey_ratio(DOLLARLP, BTULP)) %>%
  arrange(DOL_BTU_Rat)
```

    # A tibble: 4 × 3
      Region    DOL_BTU_Rat DOL_BTU_Rat_se
      <fct>           <dbl>          <dbl>
    1 Midwest        0.0158       0.000240
    2 South          0.0245       0.000388
    3 West           0.0246       0.000875
    4 Northeast      0.0247       0.000488

# Correlations

    survey_corr(
      x,
      y,
      na.rm = FALSE,
      vartype = c("se", "ci", "var", "cv"),
      level = 0.95,
      df = NULL
    )

- level: (For vartype = “ci” only) A single number or vector of numbers
  indicating the confidence level
- df: (For vartype = “ci” only) A numeric value indicating the degrees
  of freedom for t-distribution

## Overall Correlation

``` r
recs_des %>%
  summarize(SQFT_Elec_Corr = survey_corr(TOTSQFT_EN, BTUEL))
```

    # A tibble: 1 × 2
      SQFT_Elec_Corr SQFT_Elec_Corr_se
               <dbl>             <dbl>
    1          0.417           0.00689

## Correlation by Subgroup

``` r
recs_des %>%
  group_by(ACUsed) %>%
  summarize(SQFT_Elec_Corr = survey_corr(TOTSQFT_EN, DOLLAREL))
```

    # A tibble: 2 × 3
      ACUsed SQFT_Elec_Corr SQFT_Elec_Corr_se
      <lgl>           <dbl>             <dbl>
    1 FALSE           0.290           0.0240 
    2 TRUE            0.401           0.00808

# Standard Deviation and Variance

    survey_var(
      x,
      na.rm = FALSE,
      vartype = c("se", "ci", "var"),
      level = 0.95,
      df = NULL
    )

    survey_sd(
      x, 
      na.rm = FALSE
    )

## Overall Variability

``` r
recs_des %>%
  summarize(
    var_elbill = survey_var(DOLLAREL),
    sd_elbill = survey_sd(DOLLAREL)
  )
```

    # A tibble: 1 × 3
      var_elbill var_elbill_se sd_elbill
           <dbl>         <dbl>     <dbl>
    1    704906.        13926.      840.

## Variability by Subgroup

``` r
recs_des %>%
  group_by(Region) %>%
  summarize(
    var_elbill = survey_var(DOLLAREL),
    sd_elbill = survey_sd(DOLLAREL)
  )
```

    # A tibble: 4 × 4
      Region    var_elbill var_elbill_se sd_elbill
      <fct>          <dbl>         <dbl>     <dbl>
    1 Northeast    775450.        38843.      881.
    2 Midwest      552423.        25252.      743.
    3 South        702521.        30641.      838.
    4 West         717886.        30597.      847.

# Unweighted Analysis

The unweighted() function calculates unweighted summaries from a tbl_svy
object, providing the summary among the respondents without
extrapolating to a population estimate.

``` r
recs_des %>%
  summarize(
    elec_bill = survey_mean(DOLLAREL),
    elec_unweight = unweighted(mean(DOLLAREL))
  )
```

    # A tibble: 1 × 3
      elec_bill elec_bill_se elec_unweight
          <dbl>        <dbl>         <dbl>
    1     1380.         5.38         1425.

# Subpopulation Analysis

Statistics for sub-groups.

``` r
recs_des %>%
  filter(BTUNG > 0) %>%
  summarize(
    NG_mean = survey_mean(DOLLARNG,
      vartype = c("se", "ci")
    )
  )
```

    # A tibble: 1 × 4
      NG_mean NG_mean_se NG_mean_low NG_mean_upp
        <dbl>      <dbl>       <dbl>       <dbl>
    1    631.       4.64        621.        640.

``` r
recs_des %>%
  summarize(
    NG_mean = survey_mean(DOLLARNG,
      vartype = c("se", "ci")
    )
  )
```

    # A tibble: 1 × 4
      NG_mean NG_mean_se NG_mean_low NG_mean_upp
        <dbl>      <dbl>       <dbl>       <dbl>
    1    382.       3.41        375.        389.

# Design Effects

The design effect measures how the precision of an estimate is
influenced by the sampling design. In other words, it measures how much
more or less statistically efficient the survey design is compared to a
simple random sample (SRS). It is computed by taking the ratio of the
estimate’s variance under the design at hand to the estimate’s variance
under a simple random sample without replacement. A design effect less
than 1 indicates that the design is more statistically efficient than an
SRS design, which is rare but possible in a stratified sampling design
where the outcome correlates with the stratification variable(s). A
design effect greater than 1 indicates that the design is less
statistically efficient than an SRS design.

``` r
recs_des %>%
  summarize(across(
    c(BTUEL, BTUNG, BTULP, BTUFO, BTUWOOD),
    ~ survey_mean(.x, deff = TRUE, vartype = NULL)
  )) %>%
  select(ends_with("deff"))
```

    # A tibble: 1 × 5
      BTUEL_deff BTUNG_deff BTULP_deff BTUFO_deff BTUWOOD_deff
           <dbl>      <dbl>      <dbl>      <dbl>        <dbl>
    1      0.597      0.938       1.21      0.720         1.10

# Summary Rows

    cascade(
      .data, 
      ..., 
      .fill = NA, 
      .fill_level_top = FALSE, 
      .groupings = NULL
    )

- .data: A tbl_svy object
- …: Name-value pairs of summary functions (same as the summarize()
  function)
- .fill: Value to fill in for group summaries (defaults to NA)
- .fill_level_top: When filling factor variables, whether to put the
  value ‘.fill’ in the first position (defaults to FALSE, placing it in
  the bottom)

``` r
recs_des %>%
  group_by(Region) %>%
  cascade(
    DOLLAREL_mn = survey_mean(DOLLAREL),
    .fill = "National",
    .fill_level_top = T
  )
```

    # A tibble: 5 × 3
      Region    DOLLAREL_mn DOLLAREL_mn_se
      <fct>           <dbl>          <dbl>
    1 National        1380.           5.38
    2 Northeast       1343.          14.6 
    3 Midwest         1293.          11.7 
    4 South           1548.          10.3 
    5 West            1211.          12.0 

# Estimates for many outcomes

Suppose we want to calculate the total and average consumption, along
with coefficients of variation (CV), for each fuel type. These include
the reported consumption of electricity (BTUEL), natural gas (BTUNG),
liquid propane (BTULP), fuel oil (BTUFO), and wood (BTUWOOD).

``` r
consumption_ests <- recs_des %>%
  summarize(across(
    starts_with("BTU"),
    list(
      Total = ~ survey_total(.x, vartype = "cv"),
      Mean = ~ survey_mean(.x, vartype = "cv")
    ),
    .unpack = "{outer}.{inner}"
  ))
consumption_ests
```

    # A tibble: 1 × 20
      BTUEL_Total.coef BTUEL_Total._cv BTUEL_Mean.coef BTUEL_Mean._cv
                 <dbl>           <dbl>           <dbl>          <dbl>
    1    4453284510065         0.00377          36051.        0.00377
    # ℹ 16 more variables: BTUNG_Total.coef <dbl>, BTUNG_Total._cv <dbl>,
    #   BTUNG_Mean.coef <dbl>, BTUNG_Mean._cv <dbl>, BTULP_Total.coef <dbl>,
    #   BTULP_Total._cv <dbl>, BTULP_Mean.coef <dbl>, BTULP_Mean._cv <dbl>,
    #   BTUFO_Total.coef <dbl>, BTUFO_Total._cv <dbl>, BTUFO_Mean.coef <dbl>,
    #   BTUFO_Mean._cv <dbl>, BTUWOOD_Total.coef <dbl>, BTUWOOD_Total._cv <dbl>,
    #   BTUWOOD_Mean.coef <dbl>, BTUWOOD_Mean._cv <dbl>

The estimated total consumption of electricity (BTUEL) is
4,453,284,510,065 (BTUEL_Total.coef), the estimated average consumption
is 36,051 (BTUEL_Mean.coef), and the CV is 0.0038.

``` r
consumption_ests_long <- consumption_ests %>%
  pivot_longer(
    cols = everything(),
    names_to = c("FuelType", "Stat", "Type"),
    names_pattern = "BTU(.*)_(.*)\\.(.*)"
  )
consumption_ests_long
```

    # A tibble: 20 × 4
       FuelType Stat  Type     value
       <chr>    <chr> <chr>    <dbl>
     1 EL       Total coef  4.45e+12
     2 EL       Total _cv   3.77e- 3
     3 EL       Mean  coef  3.61e+ 4
     4 EL       Mean  _cv   3.77e- 3
     5 NG       Total coef  4.24e+12
     6 NG       Total _cv   9.08e- 3
     7 NG       Mean  coef  3.43e+ 4
     8 NG       Mean  _cv   9.08e- 3
     9 LP       Total coef  3.91e+11
    10 LP       Total _cv   3.80e- 2
    11 LP       Mean  coef  3.17e+ 3
    12 LP       Mean  _cv   3.80e- 2
    13 FO       Total coef  3.96e+11
    14 FO       Total _cv   3.43e- 2
    15 FO       Mean  coef  3.20e+ 3
    16 FO       Mean  _cv   3.43e- 2
    17 WOOD     Total coef  3.45e+11
    18 WOOD     Total _cv   4.54e- 2
    19 WOOD     Mean  coef  2.79e+ 3
    20 WOOD     Mean  _cv   4.54e- 2

``` r
consumption_ests_long %>%
  mutate(Type = case_when(
    Type == "coef" ~ "",
    Type == "_cv" ~ " (CV)"
  )) %>%
  pivot_wider(
    id_cols = FuelType,
    names_from = c(Stat, Type),
    names_glue = "{Stat}{Type}",
    values_from = value
  )
```

    # A tibble: 5 × 5
      FuelType   Total `Total (CV)`   Mean `Mean (CV)`
      <chr>      <dbl>        <dbl>  <dbl>       <dbl>
    1 EL       4.45e12      0.00377 36051.     0.00377
    2 NG       4.24e12      0.00908 34330.     0.00908
    3 LP       3.91e11      0.0380   3169.     0.0380 
    4 FO       3.96e11      0.0343   3203.     0.0343 
    5 WOOD     3.45e11      0.0454   2794.     0.0454 

## Proportions

``` r
cool_heat_tab <- recs_des %>%
  summarize(across(c(ACUsed, SpaceHeatingUsed), ~ survey_mean(.x),
    .unpack = "{outer}.{inner}"
  ))

cool_heat_tab %>%
  pivot_longer(everything(),
    names_to = c("Comfort", ".value"),
    names_pattern = "(.*)\\.(.*)"
  ) %>%
  rename(
    p = coef, se = `_se`
  )
```

    # A tibble: 2 × 3
      Comfort              p      se
      <chr>            <dbl>   <dbl>
    1 ACUsed           0.887 0.00306
    2 SpaceHeatingUsed 0.953 0.00207

`purrr::map()`

Suppose we want to create a table that shows the proportion of people
who express trust in their government (TrustGovernment)10 as well as
those that trust in people (TrustPeople)11 using data from the 2020
ANES.

``` r
calc_ps <- function(var) {
  anes_des %>%
    drop_na(!!sym(var)) %>%
    group_by(!!sym(var)) %>%
    summarize(p = survey_prop() * 100) %>%
    mutate(Variable = var) %>%
    rename(Answer := !!sym(var)) %>%
    select(Variable, everything())
}
```

``` r
c("TrustGovernment", "TrustPeople") %>%
  map(calc_ps) %>%
  list_rbind()
```

    # A tibble: 10 × 4
       Variable        Answer                   p  p_se
       <chr>           <fct>                <dbl> <dbl>
     1 TrustGovernment Always               1.08  0.104
     2 TrustGovernment Most of the time    13.7   0.356
     3 TrustGovernment About half the time 31.2   0.557
     4 TrustGovernment Some of the time    44.6   0.611
     5 TrustGovernment Never                9.46  0.373
     6 TrustPeople     Always               0.645 0.104
     7 TrustPeople     Most of the time    47.2   0.591
     8 TrustPeople     About half the time 27.2   0.559
     9 TrustPeople     Some of the time    21.5   0.503
    10 TrustPeople     Never                3.55  0.179
