# Sample Designs and Replicate Weights


# Setup

Reference prior chapter

``` r
library(tidyverse)
library(survey)
library(srvyr)
data(api)
data(scd)
load("data/recs_2015.rda")
load("data/recs_2020.rda")
```

# Simple Random Sample Without Replacement (SRS)

Different from standard SRS because it includes a finite population
correction (FPC) factor $\left(1 - \frac{n}{N}\right)$ in the standard
error calculation:

$$
se(\bar{y})=\sqrt{\frac{s^2}{n}\left( 1-\frac{n}{N} \right)}
$$

For small sample size, it is negligible.

    srs_des <- dat %>%
      as_survey_design(weights = wtvar,
                       fpc = fpcvar)

`weights` and `fpc` are not required if not available.

One of the example datasets we use is from the Academic Performance
Index Program (APIP). The APIP program administered by the California
Department of Education, and the {survey} package includes a population
file (sample frame) of all schools with at least 100 students and
several different samples pulled from that data using different sampling
methods. For this first example, we use the apisrs dataset, which
contains an SRS of 200 schools.

``` r
apisrs_slim <- apisrs %>%
  as_tibble() %>%
  arrange(dnum, snum) %>%
  select(cds, dnum, snum, dname, sname, fpc, pw)
apisrs_slim
```

    # A tibble: 200 × 7
       cds             dnum  snum dname                   sname            fpc    pw
       <chr>          <int> <dbl> <chr>                   <chr>          <dbl> <dbl>
     1 19642126061220     1  1121 ABC Unified             Haskell (Plin…  6194  31.0
     2 19642126066716     1  1124 ABC Unified             Stowers (Ceci…  6194  31.0
     3 36675876035174     5  3895 Adelanto Elementary     Adelanto Elem…  6194  31.0
     4 33669776031512    19  3347 Alvord Unified          Arlanza Eleme…  6194  31.0
     5 33669776031595    19  3352 Alvord Unified          Wells Interme…  6194  31.0
     6 31667876031033    39  3271 Auburn Union Elementary Cain (E.V.) M…  6194  31.0
     7 19642876011407    42  1169 Baldwin Park Unified    Deanza Elemen…  6194  31.0
     8 19642876011464    42  1175 Baldwin Park Unified    Heath (Margar…  6194  31.0
     9 19642956011589    48  1187 Bassett Unified         Erwin (Thomas…  6194  31.0
    10 41688586043392    49  4948 Bayshore Elementary     Bayshore Elem…  6194  31.0
    # ℹ 190 more rows

``` r
apisrs_des <- apisrs_slim %>%
  as_survey_design(
    weights = pw,
    fpc = fpc
  )
apisrs_des
```

    Independent Sampling design
    Called via srvyr
    Sampling variables:
      - ids: `1` 
      - fpc: fpc 
      - weights: pw 
    Data variables: 
      - cds (chr), dnum (int), snum (dbl), dname (chr), sname (chr), fpc (dbl), pw
        (dbl)

In the printed design object, the design is described as an “Independent
Sampling design,” which is another term for SRS. The ids are specified
as 1, which means there is no clustering.

``` r
summary(apisrs_des)
```

    Independent Sampling design
    Called via srvyr
    Probabilities:
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.03229 0.03229 0.03229 0.03229 0.03229 0.03229 
    Population size (PSUs): 6194 
    Data variables:
    [1] "cds"   "dnum"  "snum"  "dname" "sname" "fpc"   "pw"   

# Simple Random Sample With Replacement (SRSWR)

Syntax is the same as SRS, but there is no FPC correction.

The {survey} package does not include an example of SRSWR. To illustrate
this design, we need to create an example.

``` r
set.seed(409963)

apisrswr <- apipop %>%
  as_tibble() %>%
  slice_sample(n = 200, replace = T) %>%
  select(cds, dnum, snum, dname, sname) %>%
  mutate(weight = nrow(apipop) / 200)
head(apisrswr)
```

    # A tibble: 6 × 6
      cds             dnum  snum dname                    sname               weight
      <chr>          <int> <dbl> <chr>                    <chr>                <dbl>
    1 43696416060065   533  5348 Palo Alto Unified        Jordan (David Star…   31.0
    2 07618046005060   650   509 San Ramon Valley Unified Alamo Elementary      31.0
    3 19648086085674   457  2134 Montebello Unified       La Merced Intermed…   31.0
    4 07617056003719   346   377 Knightsen Elementary     Knightsen Elementa…   31.0
    5 19650606023022   744  2351 Torrance Unified         Carr (Evelyn) Elem…   31.0
    6 01611196090120     6    13 Alameda City Unified     Paden (William G.)…   31.0

Duplicates should be kept.

``` r
apisrswr %>%
  group_by(cds) %>%
  filter(n() > 1) %>%
  arrange(cds)
```

    # A tibble: 4 × 6
    # Groups:   cds [2]
      cds             dnum  snum dname                 sname                  weight
      <chr>          <int> <dbl> <chr>                 <chr>                   <dbl>
    1 15633216008841    41   869 Bakersfield City Elem Chipman Junior High      31.0
    2 15633216008841    41   869 Bakersfield City Elem Chipman Junior High      31.0
    3 39686766042782   716  4880 Stockton City Unified Tyler Skills Elementa…   31.0
    4 39686766042782   716  4880 Stockton City Unified Tyler Skills Elementa…   31.0

``` r
apisrswr_des <- apisrswr %>%
  as_survey_design(weights = weight)
apisrswr_des
```

    Independent Sampling design (with replacement)
    Called via srvyr
    Sampling variables:
      - ids: `1` 
      - weights: weight 
    Data variables: 
      - cds (chr), dnum (int), snum (dbl), dname (chr), sname (chr), weight (dbl)

``` r
summary(apisrswr_des)
```

    Independent Sampling design (with replacement)
    Called via srvyr
    Probabilities:
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.03229 0.03229 0.03229 0.03229 0.03229 0.03229 
    Data variables:
    [1] "cds"    "dnum"   "snum"   "dname"  "sname"  "weight"

# Stratified Sampling

Requires a `strata` argument.

``` r
apistrat_slim <- apistrat %>%
  as_tibble() %>%
  arrange(dnum, snum) %>%
  select(cds, dnum, snum, dname, sname, stype, fpc, pw)
apistrat_slim %>% count(stype, fpc)
```

    # A tibble: 3 × 3
      stype   fpc     n
      <fct> <dbl> <int>
    1 E      4421   100
    2 H       755    50
    3 M      1018    50

``` r
apistrat_des <- apistrat_slim %>%
  as_survey_design(
    strata = stype,
    weights = pw,
    fpc = fpc
  )
apistrat_des
```

    Stratified Independent Sampling design
    Called via srvyr
    Sampling variables:
      - ids: `1` 
      - strata: stype 
      - fpc: fpc 
      - weights: pw 
    Data variables: 
      - cds (chr), dnum (int), snum (dbl), dname (chr), sname (chr), stype (fct),
        fpc (dbl), pw (dbl)

``` r
summary(apistrat_des)
```

    Stratified Independent Sampling design
    Called via srvyr
    Probabilities:
       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.02262 0.02262 0.03587 0.04014 0.05339 0.06623 
    Stratum Sizes: 
                 E  H  M
    obs        100 50 50
    design.PSU 100 50 50
    actual.PSU 100 50 50
    Population stratum sizes (PSUs): 
       E    H    M 
    4421  755 1018 
    Data variables:
    [1] "cds"   "dnum"  "snum"  "dname" "sname" "stype" "fpc"   "pw"   

# Clustered Sampling

Requires an `ids` argument specifying the cluster level variable(s).

    clus2_des <- dat %>%
     as_survey_design(weights = wtvar, 
                      ids = c(PSU, SSU), 
                      fpc = c(A, B))

where `PSU` and `SSU` are the variables indicating the `PSU` and `SSU`
identifiers, and `A` and `B` are the variables indicating the population
sizes for each level. The `fpc` is not needed if sampled with
replacement or the population size is very large. If clusters within
each stratum have the same identifier, must use `nest = TRUE`.

The survey package includes a two-stage cluster sample data, apiclus2,
in which school districts were sampled, and then a random sample of five
schools was selected within each district. For districts with fewer than
five schools, all schools were sampled. School districts are identified
by dnum, and schools are identified by snum. The variable fpc1 indicates
how many districts there are in California (the total number of PSUs or
A), and fpc2 indicates how many schools were in a given district with at
least 100 students (the total number of SSUs or B). The data include a
row for each school. In the data printed below, there are 757 school
districts, as indicated by fpc1, and there are nine schools in District
731, one school in District 742, two schools in District 768, and so on
as indicated by fpc2. For illustration purposes, the object
apiclus2_slim has been created from apiclus2, which subsets the data to
only the necessary columns and sorts the data.

``` r
apiclus2_slim <- apiclus2 %>%
  as_tibble() %>%
  arrange(desc(dnum), snum) %>%
  select(cds, dnum, snum, fpc1, fpc2, pw)
apiclus2_slim
```

    # A tibble: 126 × 6
       cds             dnum  snum  fpc1      fpc2    pw
       <chr>          <int> <dbl> <dbl> <int[1d]> <dbl>
     1 47704826050942   795  5552   757         1  18.9
     2 07618126005169   781   530   757         6  22.7
     3 07618126005177   781   531   757         6  22.7
     4 07618126005185   781   532   757         6  22.7
     5 07618126005193   781   533   757         6  22.7
     6 07618126005243   781   535   757         6  22.7
     7 19650786023337   768  2371   757         2  18.9
     8 19650786023345   768  2372   757         2  18.9
     9 54722076054423   742  5898   757         1  18.9
    10 50712906053086   731  5781   757         9  34.1
    # ℹ 116 more rows

``` r
apiclus2_des <- apiclus2_slim %>%
  as_survey_design(
    ids = c(dnum, snum),
    fpc = c(fpc1, fpc2),
    weights = pw
  )
apiclus2_des
```

    2 - level Cluster Sampling design
    With (40, 126) clusters.
    Called via srvyr
    Sampling variables:
      - ids: `dnum + snum` 
      - fpc: `fpc1 + fpc2` 
      - weights: pw 
    Data variables: 
      - cds (chr), dnum (int), snum (dbl), fpc1 (dbl), fpc2 (int[1d]), pw (dbl)

``` r
summary(apiclus2_des)
```

    2 - level Cluster Sampling design
    With (40, 126) clusters.
    Called via srvyr
    Probabilities:
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    0.003669 0.037743 0.052840 0.042390 0.052840 0.052840 
    Population size (PSUs): 757 
    Data variables:
    [1] "cds"  "dnum" "snum" "fpc1" "fpc2" "pw"  

# Replicate Weights

There are several types of replicate weights, including balanced
repeated replication (BRR), Fay’s BRR, jackknife, and bootstrap methods.
An overview of the process for using replicate weights is as follows:

- Divide the sample into subsample replicates that mirror the design of
  the sample
- Calculate weights for each replicate using the same procedures for the
  full-sample weight (i.e., nonresponse and post-stratification)
- Calculate estimates for each replicate using the same method as the
  full-sample estimate
- Calculate the estimated variance, which is proportional to the
  variance of the replicate estimates

# Balanced Repeated Replication Method (BRR)

The balanced repeated replication (BRR) method requires a stratified
sample design with two PSUs in each stratum. Each replicate is
constructed by deleting one PSU per stratum using a Hadamard matrix.

Replicate weights generally come in groups and are sequentially
numbered, such as PWGTP1, PWGTP2, …, PWGTP80 for the person weights in
the American Community Survey (ACS)

To specify a BRR design, we need to specify the weight variable
(weights), the replicate weight variables (repweights), the type of
replicate weights as BRR (type = BRR), and whether the mean squared
error should be used (mse = TRUE) or not (mse = FALSE).

Equivalent syntax

    brr_des <- dat %>%
      as_survey_rep(weights = WT0,
                    repweights = all_of(str_c("WT", 1:20)), 
                    type = "BRR",
                    mse = TRUE)

    brr_des <- dat %>%
      as_survey_rep(weights = WT0,
                    repweights = num_range("WT", 1:20),
                    type = "BRR",
                    mse = TRUE)

Equivalent syntax

    brr_des <- dat %>%
      as_survey_rep(weights = WT,
                    repweights = all_of(str_c("REPWT", 1:20)),
                    type = "BRR",
                    mse = TRUE)

    brr_des <- dat %>%
      as_survey_rep(weights = WT,
                    repweights = starts_with("REPWT"),
                    type = "BRR",
                    mse = TRUE)
                    
    brr_des <- dat %>%
      as_survey_rep(weights = WT,
                    repweights = REPWT1:REPWT20,
                    type = "BRR",
                    mse = TRUE)

The {survey} package includes a data example from section 12.2 of Levy
and Lemeshow (2013). In this fictional data, two out of five ambulance
stations were sampled from each of three emergency service areas (ESAs);
thus BRR weights are appropriate with two PSUs (stations) sampled in
each stratum (ESA). In the code below, we create BRR weights as was done
by Levy and Lemeshow (2013).

``` r
scdbrr <- scd %>%
  as_tibble() %>%
  mutate(
    wt = 5 / 2,
    rep1 = 2 * c(1, 0, 1, 0, 1, 0),
    rep2 = 2 * c(1, 0, 0, 1, 0, 1),
    rep3 = 2 * c(0, 1, 1, 0, 0, 1),
    rep4 = 2 * c(0, 1, 0, 1, 1, 0)
  )

scdbrr
```

    # A tibble: 6 × 9
        ESA ambulance arrests alive    wt  rep1  rep2  rep3  rep4
      <int>     <int>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    1     1         1     120    25   2.5     2     2     0     0
    2     1         2      78    24   2.5     0     0     2     2
    3     2         1     185    30   2.5     2     0     2     0
    4     2         2     228    49   2.5     0     2     0     2
    5     3         1     670    80   2.5     2     0     0     2
    6     3         2     530    70   2.5     0     2     2     0

``` r
scdbrr_des <- scdbrr %>%
  as_survey_rep(
    type = "BRR",
    repweights = starts_with("rep"),
    combined_weights = F,
    weight = wt
  )
scdbrr_des
```

    Call: Called via srvyr
    Balanced Repeated Replicates with 4 replicates.
    Sampling variables:
      - repweights: `rep1 + rep2 + rep3 + rep4` 
      - weights: wt 
    Data variables: 
      - ESA (int), ambulance (int), arrests (dbl), alive (dbl), wt (dbl), rep1
        (dbl), rep2 (dbl), rep3 (dbl), rep4 (dbl)

``` r
summary(scdbrr_des)
```

    Call: Called via srvyr
    Balanced Repeated Replicates with 4 replicates.
    Sampling variables:
      - repweights: `rep1 + rep2 + rep3 + rep4` 
      - weights: wt 
    Data variables: 
      - ESA (int), ambulance (int), arrests (dbl), alive (dbl), wt (dbl), rep1
        (dbl), rep2 (dbl), rep3 (dbl), rep4 (dbl)
    Variables: 
    [1] "ESA"       "ambulance" "arrests"   "alive"     "wt"        "rep1"     
    [7] "rep2"      "rep3"      "rep4"     

Note that combined_weights was specified as FALSE because these weights
are simply specified as 0 and 2 and do not incorporate the overall
weight.

# Fay’s BRR Method

    fay_des <- dat %>%
      as_survey_rep(weights = WT0,
                    repweights = num_range("WT", 1:20),
                    type = "Fay",
                    mse = TRUE,
                    rho = 0.3)

`rho` is Fay’s multiplier

The 2015 RECS (U.S. Energy Information Administration 2017) uses Fay’s
BRR weights with the final weight as NWEIGHT and replicate weights as
BRRWT1 - BRRWT96, and the documentation specifies a Fay’s multiplier of
0.5. On the file, DOEID is a unique identifier for each respondent,
TOTALDOL is the total energy cost, TOTSQFT_EN is the total square
footage of the residence, and REGOINC is the census region.

``` r
recs_2015_des <- recs_2015 %>%
  as_survey_rep(
    weights = NWEIGHT,
    repweights = BRRWT1:BRRWT96,
    type = "Fay",
    rho = 0.5,
    mse = T,
    variables = c(DOEID, TOTALDOL, TOTSQFT_EN, REGIONC)
  )
recs_2015_des
```

    Call: Called via srvyr
    Fay's variance method (rho= 0.5 ) with 96 replicates and MSE variances.
    Sampling variables:
      - repweights: `BRRWT1 + BRRWT2 + BRRWT3 + BRRWT4 + BRRWT5 + BRRWT6 + BRRWT7 +
        BRRWT8 + BRRWT9 + BRRWT10 + BRRWT11 + BRRWT12 + BRRWT13 + BRRWT14 + BRRWT15
        + BRRWT16 + BRRWT17 + BRRWT18 + BRRWT19 + BRRWT20 + BRRWT21 + BRRWT22 +
        BRRWT23 + BRRWT24 + BRRWT25 + BRRWT26 + BRRWT27 + BRRWT28 + BRRWT29 +
        BRRWT30 + BRRWT31 + BRRWT32 + BRRWT33 + BRRWT34 + BRRWT35 + BRRWT36 +
        BRRWT37 + BRRWT38 + BRRWT39 + BRRWT40 + BRRWT41 + BRRWT42 + BRRWT43 +
        BRRWT44 + BRRWT45 + BRRWT46 + BRRWT47 + BRRWT48 + BRRWT49 + BRRWT50 +
        BRRWT51 + BRRWT52 + BRRWT53 + BRRWT54 + BRRWT55 + BRRWT56 + BRRWT57 +
        BRRWT58 + BRRWT59 + BRRWT60 + BRRWT61 + BRRWT62 + BRRWT63 + BRRWT64 +
        BRRWT65 + BRRWT66 + BRRWT67 + BRRWT68 + BRRWT69 + BRRWT70 + BRRWT71 +
        BRRWT72 + BRRWT73 + BRRWT74 + BRRWT75 + BRRWT76 + BRRWT77 + BRRWT78 +
        BRRWT79 + BRRWT80 + BRRWT81 + BRRWT82 + BRRWT83 + BRRWT84 + BRRWT85 +
        BRRWT86 + BRRWT87 + BRRWT88 + BRRWT89 + BRRWT90 + BRRWT91 + BRRWT92 +
        BRRWT93 + BRRWT94 + BRRWT95 + BRRWT96` 
      - weights: NWEIGHT 
    Data variables: 
      - DOEID (dbl), TOTALDOL (dbl), TOTSQFT_EN (dbl), REGIONC (dbl)

``` r
summary(recs_2015_des)
```

    Call: Called via srvyr
    Fay's variance method (rho= 0.5 ) with 96 replicates and MSE variances.
    Sampling variables:
      - repweights: `BRRWT1 + BRRWT2 + BRRWT3 + BRRWT4 + BRRWT5 + BRRWT6 + BRRWT7 +
        BRRWT8 + BRRWT9 + BRRWT10 + BRRWT11 + BRRWT12 + BRRWT13 + BRRWT14 + BRRWT15
        + BRRWT16 + BRRWT17 + BRRWT18 + BRRWT19 + BRRWT20 + BRRWT21 + BRRWT22 +
        BRRWT23 + BRRWT24 + BRRWT25 + BRRWT26 + BRRWT27 + BRRWT28 + BRRWT29 +
        BRRWT30 + BRRWT31 + BRRWT32 + BRRWT33 + BRRWT34 + BRRWT35 + BRRWT36 +
        BRRWT37 + BRRWT38 + BRRWT39 + BRRWT40 + BRRWT41 + BRRWT42 + BRRWT43 +
        BRRWT44 + BRRWT45 + BRRWT46 + BRRWT47 + BRRWT48 + BRRWT49 + BRRWT50 +
        BRRWT51 + BRRWT52 + BRRWT53 + BRRWT54 + BRRWT55 + BRRWT56 + BRRWT57 +
        BRRWT58 + BRRWT59 + BRRWT60 + BRRWT61 + BRRWT62 + BRRWT63 + BRRWT64 +
        BRRWT65 + BRRWT66 + BRRWT67 + BRRWT68 + BRRWT69 + BRRWT70 + BRRWT71 +
        BRRWT72 + BRRWT73 + BRRWT74 + BRRWT75 + BRRWT76 + BRRWT77 + BRRWT78 +
        BRRWT79 + BRRWT80 + BRRWT81 + BRRWT82 + BRRWT83 + BRRWT84 + BRRWT85 +
        BRRWT86 + BRRWT87 + BRRWT88 + BRRWT89 + BRRWT90 + BRRWT91 + BRRWT92 +
        BRRWT93 + BRRWT94 + BRRWT95 + BRRWT96` 
      - weights: NWEIGHT 
    Data variables: 
      - DOEID (dbl), TOTALDOL (dbl), TOTSQFT_EN (dbl), REGIONC (dbl)
    Variables: 
    [1] "DOEID"      "TOTALDOL"   "TOTSQFT_EN" "REGIONC"   

# Jacknife Method

Removes one PSU at a time.

To specify the jackknife method, we use the survey documentation to
understand the type of jackknife (1, n, or 2) and the multiplier. In the
syntax, we need to specify the weight variable (weights), the replicate
weight variables (repweights), the type of replicate weights as
jackknife 1 (type = “JK1”), n (type = “JKN”), or 2 (type = “JK2”),
whether the mean squared error should be used (mse = TRUE) or not (mse =
FALSE), and the multiplier (scale). For example, if the survey is a
jackknife 1 method with a multiplier of αr=(R−1)/R=19/20=0.95, the
dataset has WT0 for the main weight and 20 replicate weights indicated
as WT1, WT2, …, WT20, we use the following syntax:

    jk1_des <- dat %>%
      as_survey_rep(
        weights = WT0,
        repweights = num_range("WT", 1:20),
        type = "JK1",
        mse = TRUE,
        scale = 0.95
      )

The 2020 RECS (U.S. Energy Information Administration 2023c) uses
jackknife weights with the final weight as NWEIGHT and replicate weights
as NWEIGHT1 - NWEIGHT60 with a scale of (R−1)/R=59/60. On the file,
DOEID is a unique identifier for each respondent, TOTALDOL is the total
cost of energy, TOTSQFT_EN is the total square footage of the residence,
and REGOINC is the census region.

``` r
recs_des <- recs_2020 %>%
  as_survey_rep(
    weights = NWEIGHT,
    repweights = NWEIGHT1:NWEIGHT60,
    type = "JK1",
    scale = 59 / 60,
    mse = T,
    variables = c(DOEID, TOTALDOL, TOTSQFT_EN, REGIONC)
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
      - DOEID (dbl), TOTALDOL (dbl), TOTSQFT_EN (dbl), REGIONC (chr)

``` r
summary(recs_des)
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
      - DOEID (dbl), TOTALDOL (dbl), TOTSQFT_EN (dbl), REGIONC (chr)
    Variables: 
    [1] "DOEID"      "TOTALDOL"   "TOTSQFT_EN" "REGIONC"   

# Bootstrap Method

To specify a bootstrap method, we need to specify the weight variable
(weights), the replicate weight variables (repweights), the type of
replicate weights as bootstrap (type = “bootstrap”), whether the mean
squared error should be used (mse = TRUE) or not (mse = FALSE), and the
multiplier (scale). For example, if a dataset had WT0 for the main
weight, 20 bootstrap weights indicated WT1, WT2, …, WT20, and a
multiplier of α=.02, we use the following syntax:

    bs_des <- dat %>%
      as_survey_rep(
        weights = WT0,
        repweights = num_range("WT", 1:20),
        type = "bootstrap",
        mse = TRUE,
        scale = .02
      )

Returning to the APIP example, we are going to create a dataset with
bootstrap weights to use as an example. In this example, we construct a
one-cluster design with 50 replicate weights

``` r
apiclus1_slim <-
  apiclus1 %>%
  as_tibble() %>%
  arrange(dnum) %>%
  select(cds, dnum, fpc, pw)
```

``` r
set.seed(662152)
apibw <- bootweights(
  psu = apiclus1_slim$dnum,
  strata = rep(1, nrow(apiclus1_slim)),
  fpc = apiclus1_slim$fpc,
  replicates = 50
)

bwmata <- apibw$repweights$weights[apibw$repweights$index, ] * apiclus1_slim$pw

apiclus1_slim <- bwmata %>%
  as.data.frame() %>%
  set_names(str_c("pw", 1:50)) %>%
  cbind(apiclus1_slim) %>%
  as_tibble() %>%
  select(cds, dnum, fpc, pw, everything())

apiclus1_slim
```

    # A tibble: 183 × 54
       cds    dnum   fpc    pw   pw1   pw2   pw3   pw4   pw5   pw6   pw7   pw8   pw9
       <chr> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
     1 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
     2 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
     3 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
     4 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
     5 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
     6 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
     7 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
     8 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
     9 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
    10 4369…    61   757  33.8  33.8     0     0  33.8     0  33.8     0  67.7     0
    # ℹ 173 more rows
    # ℹ 41 more variables: pw10 <dbl>, pw11 <dbl>, pw12 <dbl>, pw13 <dbl>,
    #   pw14 <dbl>, pw15 <dbl>, pw16 <dbl>, pw17 <dbl>, pw18 <dbl>, pw19 <dbl>,
    #   pw20 <dbl>, pw21 <dbl>, pw22 <dbl>, pw23 <dbl>, pw24 <dbl>, pw25 <dbl>,
    #   pw26 <dbl>, pw27 <dbl>, pw28 <dbl>, pw29 <dbl>, pw30 <dbl>, pw31 <dbl>,
    #   pw32 <dbl>, pw33 <dbl>, pw34 <dbl>, pw35 <dbl>, pw36 <dbl>, pw37 <dbl>,
    #   pw38 <dbl>, pw39 <dbl>, pw40 <dbl>, pw41 <dbl>, pw42 <dbl>, pw43 <dbl>, …

``` r
api1_bs_des <- apiclus1_slim %>%
  as_survey_rep(
    weights = pw,
    repweights = pw1:pw50,
    type = "bootstrap",
    scale = 0.02186589,
    mse = T
  )
api1_bs_des
```

    Call: Called via srvyr
    Survey bootstrap with 50 replicates and MSE variances.
    Sampling variables:
      - repweights: `pw1 + pw2 + pw3 + pw4 + pw5 + pw6 + pw7 + pw8 + pw9 + pw10 +
        pw11 + pw12 + pw13 + pw14 + pw15 + pw16 + pw17 + pw18 + pw19 + pw20 + pw21
        + pw22 + pw23 + pw24 + pw25 + pw26 + pw27 + pw28 + pw29 + pw30 + pw31 +
        pw32 + pw33 + pw34 + pw35 + pw36 + pw37 + pw38 + pw39 + pw40 + pw41 + pw42
        + pw43 + pw44 + pw45 + pw46 + pw47 + pw48 + pw49 + pw50` 
      - weights: pw 
    Data variables: 
      - cds (chr), dnum (int), fpc (dbl), pw (dbl), pw1 (dbl), pw2 (dbl), pw3
        (dbl), pw4 (dbl), pw5 (dbl), pw6 (dbl), pw7 (dbl), pw8 (dbl), pw9 (dbl),
        pw10 (dbl), pw11 (dbl), pw12 (dbl), pw13 (dbl), pw14 (dbl), pw15 (dbl),
        pw16 (dbl), pw17 (dbl), pw18 (dbl), pw19 (dbl), pw20 (dbl), pw21 (dbl),
        pw22 (dbl), pw23 (dbl), pw24 (dbl), pw25 (dbl), pw26 (dbl), pw27 (dbl),
        pw28 (dbl), pw29 (dbl), pw30 (dbl), pw31 (dbl), pw32 (dbl), pw33 (dbl),
        pw34 (dbl), pw35 (dbl), pw36 (dbl), pw37 (dbl), pw38 (dbl), pw39 (dbl),
        pw40 (dbl), pw41 (dbl), pw42 (dbl), pw43 (dbl), pw44 (dbl), pw45 (dbl),
        pw46 (dbl), pw47 (dbl), pw48 (dbl), pw49 (dbl), pw50 (dbl)

``` r
summary(api1_bs_des)
```

    Call: Called via srvyr
    Survey bootstrap with 50 replicates and MSE variances.
    Sampling variables:
      - repweights: `pw1 + pw2 + pw3 + pw4 + pw5 + pw6 + pw7 + pw8 + pw9 + pw10 +
        pw11 + pw12 + pw13 + pw14 + pw15 + pw16 + pw17 + pw18 + pw19 + pw20 + pw21
        + pw22 + pw23 + pw24 + pw25 + pw26 + pw27 + pw28 + pw29 + pw30 + pw31 +
        pw32 + pw33 + pw34 + pw35 + pw36 + pw37 + pw38 + pw39 + pw40 + pw41 + pw42
        + pw43 + pw44 + pw45 + pw46 + pw47 + pw48 + pw49 + pw50` 
      - weights: pw 
    Data variables: 
      - cds (chr), dnum (int), fpc (dbl), pw (dbl), pw1 (dbl), pw2 (dbl), pw3
        (dbl), pw4 (dbl), pw5 (dbl), pw6 (dbl), pw7 (dbl), pw8 (dbl), pw9 (dbl),
        pw10 (dbl), pw11 (dbl), pw12 (dbl), pw13 (dbl), pw14 (dbl), pw15 (dbl),
        pw16 (dbl), pw17 (dbl), pw18 (dbl), pw19 (dbl), pw20 (dbl), pw21 (dbl),
        pw22 (dbl), pw23 (dbl), pw24 (dbl), pw25 (dbl), pw26 (dbl), pw27 (dbl),
        pw28 (dbl), pw29 (dbl), pw30 (dbl), pw31 (dbl), pw32 (dbl), pw33 (dbl),
        pw34 (dbl), pw35 (dbl), pw36 (dbl), pw37 (dbl), pw38 (dbl), pw39 (dbl),
        pw40 (dbl), pw41 (dbl), pw42 (dbl), pw43 (dbl), pw44 (dbl), pw45 (dbl),
        pw46 (dbl), pw47 (dbl), pw48 (dbl), pw49 (dbl), pw50 (dbl)
    Variables: 
     [1] "cds"  "dnum" "fpc"  "pw"   "pw1"  "pw2"  "pw3"  "pw4"  "pw5"  "pw6" 
    [11] "pw7"  "pw8"  "pw9"  "pw10" "pw11" "pw12" "pw13" "pw14" "pw15" "pw16"
    [21] "pw17" "pw18" "pw19" "pw20" "pw21" "pw22" "pw23" "pw24" "pw25" "pw26"
    [31] "pw27" "pw28" "pw29" "pw30" "pw31" "pw32" "pw33" "pw34" "pw35" "pw36"
    [41] "pw37" "pw38" "pw39" "pw40" "pw41" "pw42" "pw43" "pw44" "pw45" "pw46"
    [51] "pw47" "pw48" "pw49" "pw50"
