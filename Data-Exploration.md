Data Exploration
================
Manye Dong
2023-11-28

## Predict the risk of death based on features 1-14

``` r
# import data and data cleaning
bc_data = read.csv("./Project_2_data.csv") |>
  janitor::clean_names() |> 
  na.omit() |> 
  mutate(
    race = factor(race, labels = c("1", "2", "3"), levels = c("Black","White","Other")),
    marital_status = factor(marital_status, labels = c("1", "2", "3","4","5"),levels = c("Divorced", "Married", "Separated", "Single ", "Widowed")),
    t_stage = factor(t_stage, labels = c("1", "2", "3","4"),levels = c("T1", "T2", "T3", "T4")),
    n_stage = factor(n_stage, labels = c("1","2","3"),levels = c("N1","N2", "N3")),
    x6th_stage = factor(x6th_stage, labels = c("1", "2", "3","4","5"),levels = c("IIA","IIB","IIIA","IIIB","IIIC")),
    differentiate = factor(differentiate, labels = c("1", "2", "3","4"),levels = c("Moderately differentiated","Poorly differentiated","Undifferentiated","Well differentiated")),
    grade = factor(grade, labels = c("1", "2", "3","4"),levels = c("1","2","3"," anaplastic; Grade IV")),
    a_stage = factor(a_stage, labels = c("1","2"),levels = c("Distant","Regional")),
    estrogen_status = factor(estrogen_status, labels = c("0","1"),levels = c("Negative","Positive")),
    progesterone_status = factor(progesterone_status, labels = c("0","1"),levels = c("Negative","Positive"))
    ) |> 
  select(-status)
```

``` r
# Pairwise interaction and Correlation plot
bc_data |> 
  pairs()
```

![](Data-Exploration_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
cor_matrix <- bc_data |> 
  mutate(across(where(is.factor), as.numeric)) |> 
  cor()

corrplot(cor_matrix, type = "upper", diag = FALSE, tl.cex = 0.5, tl.srt = 45)
```

![](Data-Exploration_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
mult.fit = 
  lm(survival_months ~ ., data = bc_data)

summary(mult.fit)
```

    ## 
    ## Call:
    ## lm(formula = survival_months ~ ., data = bc_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -74.685 -15.591   1.087  18.126  56.245 
    ## 
    ## Coefficients: (4 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            58.98164    4.18486  14.094  < 2e-16 ***
    ## age                    -0.04220    0.04138  -1.020  0.30787    
    ## race2                   3.67154    1.40193   2.619  0.00885 ** 
    ## race3                   5.58194    1.84998   3.017  0.00257 ** 
    ## marital_status2         0.71309    1.11566   0.639  0.52276    
    ## marital_status3        -6.24605    3.52093  -1.774  0.07614 .  
    ## marital_status4        -0.03560    1.37859  -0.026  0.97940    
    ## marital_status5        -0.78933    1.80857  -0.436  0.66254    
    ## t_stage2               -1.61447    1.69125  -0.955  0.33984    
    ## t_stage3                0.73762    2.76338   0.267  0.78954    
    ## t_stage4               -2.25092    4.48353  -0.502  0.61567    
    ## n_stage2               -0.58607    1.98650  -0.295  0.76799    
    ## n_stage3               -3.37649    2.67197  -1.264  0.20642    
    ## x6th_stage2             0.53765    1.82506   0.295  0.76832    
    ## x6th_stage3            -0.65701    2.36107  -0.278  0.78082    
    ## x6th_stage4             3.32794    5.15941   0.645  0.51895    
    ## x6th_stage5                  NA         NA      NA       NA    
    ## differentiate2         -0.98945    0.85155  -1.162  0.24533    
    ## differentiate3         -2.95238    5.21782  -0.566  0.57154    
    ## differentiate4         -0.02110    1.07890  -0.020  0.98440    
    ## grade2                       NA         NA      NA       NA    
    ## grade3                       NA         NA      NA       NA    
    ## grade4                       NA         NA      NA       NA    
    ## a_stage2                4.36505    2.67211   1.634  0.10243    
    ## tumor_size             -0.05649    0.03434  -1.645  0.10002    
    ## estrogen_status1        8.61299    1.68605   5.108  3.4e-07 ***
    ## progesterone_status1    1.60271    1.10116   1.455  0.14562    
    ## regional_node_examined  0.10692    0.04828   2.214  0.02686 *  
    ## reginol_node_positive  -0.31396    0.14259  -2.202  0.02774 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 22.47 on 3999 degrees of freedom
    ## Multiple R-squared:  0.04476,    Adjusted R-squared:  0.03903 
    ## F-statistic: 7.808 on 24 and 3999 DF,  p-value: < 2.2e-16

``` r
# include a descriptive table with summary statistics for all variables

# continuous data
conti_var = c("age", "tumor_size", "regional_node_examined","reginol_node_positive", "survival_months")
bc_data |>
  select(all_of(conti_var)) |>
  summary() |>
  knitr::kable()

# discrete data count number of distinct variables


discre_var <- c("race", "marital_status", "t_stage", "n_stage", "x6th_stage", "differentiate", "grade", "a_stage", "estrogen_status", "progesterone_status", "status")

# Function to create a summary table for each variable
summary_table = function(variable) {
  counts = table(bc_data[[variable]])
  summary_df = data.frame(
    Variable = rep(variable, length(counts)),
    Value = paste(variable, names(counts), sep = "_"),
    Count = as.vector(counts)
  )
  return(summary_df)
}

summary_tables = lapply(discre_var, summary_table)
combined_summary = do.call(rbind, summary_tables) |>
  knitr::kable()
print(combined_summary)
```

``` r
# explore the distribution of the outcome and consider potential transformations if necessary
# Since the purpose is to predict the risk of death based on features 1-14, we are going to fit a model with variables 1-14 as predictors (x) and the survival months as the y value. 
# look at the original distribution of survival months
hist(bc_data$survival_months, main = "Distribution of survival months", xlab = "Survival Month")
```

![](Data-Exploration_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#try different transformation 
log_survival = log(bc_data$survival_months)
hist(log_survival, main = "Distribution of log_transformed survival months", xlab = "log-transformed survival months")
```

![](Data-Exploration_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
sqrt_survival = sqrt(bc_data$survival_months)
hist(sqrt_survival, main = "Distribution of sqrt(survival months)", xlab = "sqrt(survival months)")
```

![](Data-Exploration_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
sq_survival = (bc_data$survival_months^2)
hist(sq_survival, main = "Distribution of square(survival months)", xlab = "square(survival months)")
```

![](Data-Exploration_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
bc_data = bc_data |>
  mutate(log_survival = log(survival_months))

bc_data |>
  ggplot(aes(x = log_survival)) +
  geom_histogram() +
  labs(title = "Distribution of log(survival months)", x = "log(survival months)")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Data-Exploration_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

``` r
# examine the marginal distributions and pariwise relationships between variables (e.g., to check to see whether any nonlinearities are immediately obvious)
# explore several candidate models, and explain why you select your model
model_all = lm(survival_months ~ age+reginol_node_positive+ regional_node_examined+factor(estrogen_status)+factor(progesterone_status)+tumor_size+factor(a_stage) +factor(grade)+factor(differentiate)+factor(x6th_stage)+factor(n_stage)+factor(t_stage)+factor(marital_status)+factor(race), data = bc_data)

summary(model_all)
```

``` r
# see if there are any unusual observations and consider them as potential outliers/influential points

# detect non normality of outliers, using qq plot
plot(model_all, which = 2)
```

``` r
# suggest possible models
```
