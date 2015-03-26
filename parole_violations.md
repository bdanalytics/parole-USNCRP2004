# Parole Violations: violator classification
bdanalytics  

**  **    
**Date: (Thu) Mar 26, 2015**    

# Introduction:  

Data: US National Corrections Reporting Program 2004
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/parole.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 144               # or any integer 

glb_predct_var <- "violator"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c(NULL)                # or NULL

glb_exclude_vars_as_features <- union(glb_id_vars, ".rnorm")     # or NULL                      
# List chrs (convert into factors if it's a valid feature); num/int transformed  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("state", "crime")     # or NULL
                                      )
# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
              c("violator.predict.proba", "violator.predict")     # or NULL
                                      )

glb_mice_complete.seed <- 144               # or any integer
glb_is_regression <- FALSE; glb_is_classification <- TRUE

glb_mdl <- glb_sel_mdl <- glb_dmy_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/parole.csv", 
    comment="glb_entity_df", force_header=TRUE,
    print_diagn=(glb_is_separate_newent_dataset | 
                !glb_split_entity_newent_datasets))
```

```
## [1] "Reading file ./data/parole.csv..."
## [1] "dimensions of data in ./data/parole.csv: 675 rows x 9 cols"
```

```r
print(table(glb_entity_df$violator))
```

```
## 
##   0   1 
## 597  78
```

```r
print(summary(glb_entity_df))
```

```
##       male             race            age            state      
##  Min.   :0.0000   Min.   :1.000   Min.   :18.40   Min.   :1.000  
##  1st Qu.:1.0000   1st Qu.:1.000   1st Qu.:25.35   1st Qu.:2.000  
##  Median :1.0000   Median :1.000   Median :33.70   Median :3.000  
##  Mean   :0.8074   Mean   :1.424   Mean   :34.51   Mean   :2.887  
##  3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:42.55   3rd Qu.:4.000  
##  Max.   :1.0000   Max.   :2.000   Max.   :67.00   Max.   :4.000  
##   time.served     max.sentence   multiple.offenses     crime      
##  Min.   :0.000   Min.   : 1.00   Min.   :0.0000    Min.   :1.000  
##  1st Qu.:3.250   1st Qu.:12.00   1st Qu.:0.0000    1st Qu.:1.000  
##  Median :4.400   Median :12.00   Median :1.0000    Median :2.000  
##  Mean   :4.198   Mean   :13.06   Mean   :0.5363    Mean   :2.059  
##  3rd Qu.:5.200   3rd Qu.:15.00   3rd Qu.:1.0000    3rd Qu.:3.000  
##  Max.   :6.000   Max.   :18.00   Max.   :1.0000    Max.   :4.000  
##     violator     
##  Min.   :0.0000  
##  1st Qu.:0.0000  
##  Median :0.0000  
##  Mean   :0.1156  
##  3rd Qu.:0.0000  
##  Max.   :1.0000
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(
        url="<newdt_url>", 
        comment="glb_newent_df", force_header=TRUE, print_diagn=TRUE)
} else {
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
                                          max(2, nrow(glb_entity_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newent_df <- do.call("subset", 
                list(glb_entity_df, parse(text=glb_split_newdata_condition)))
            glb_entity_df <- do.call("subset", 
                list(glb_entity_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_entity_df[, glb_predct_var], 
                                      SplitRatio=(1-glb_split_newdata_size))
                glb_newent_df <- glb_entity_df[!split, ] 
                glb_entity_df <- glb_entity_df[split ,]
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample')")   

    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_entity_df)
        str(glb_entity_df)        
    }
}         
```

```
## Loading required package: caTools
```

```
##    male race  age state time.served max.sentence multiple.offenses crime
## 2     0    1 39.7     1         5.4           12                 0     3
## 3     1    2 29.5     1         5.6           12                 0     3
## 8     0    1 24.6     1         4.8           12                 0     1
## 10    1    2 29.1     1         4.7           12                 0     2
## 19    1    2 24.5     1         6.0           16                 0     3
## 27    1    1 32.8     1         5.9           16                 0     3
##    violator
## 2         0
## 3         0
## 8         0
## 10        0
## 19        0
## 27        0
##     male race  age state time.served max.sentence multiple.offenses crime
## 126    1    1 40.4     2         4.2           12                 0     1
## 130    1    2 18.7     2         5.2           12                 0     1
## 176    1    1 19.5     2         5.3           12                 0     1
## 254    1    2 21.6     3         4.0            8                 1     3
## 282    0    1 51.8     1         6.0           12                 0     1
## 395    0    2 46.0     4         4.5           15                 1     1
##     violator
## 126        0
## 130        1
## 176        0
## 254        1
## 282        0
## 395        0
##     male race  age state time.served max.sentence multiple.offenses crime
## 661    1    1 22.1     4         3.8           14                 1     1
## 665    1    1 56.5     4         5.8           18                 0     1
## 669    1    1 39.0     4         3.9           14                 1     2
## 672    0    1 47.5     1         5.2           16                 0     3
## 673    1    1 45.4     1         5.7           12                 0     3
## 674    1    1 38.4     1         1.8           18                 0     1
##     violator
## 661        0
## 665        0
## 669        0
## 672        0
## 673        0
## 674        0
## 'data.frame':	202 obs. of  9 variables:
##  $ male             : int  0 1 0 1 1 1 1 1 1 1 ...
##  $ race             : int  1 2 1 2 2 1 1 2 1 1 ...
##  $ age              : num  39.7 29.5 24.6 29.1 24.5 32.8 36.7 36.5 33.5 37.3 ...
##  $ state            : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ time.served      : num  5.4 5.6 4.8 4.7 6 5.9 0.9 3.9 4.2 4.6 ...
##  $ max.sentence     : int  12 12 12 12 16 16 16 12 12 12 ...
##  $ multiple.offenses: int  0 0 0 0 0 0 0 1 1 1 ...
##  $ crime            : int  3 3 1 2 3 3 3 4 1 1 ...
##  $ violator         : int  0 0 0 0 0 0 0 1 1 1 ...
##  - attr(*, "comment")= chr "glb_newent_df"
##   male race  age state time.served max.sentence multiple.offenses crime
## 1    1    1 33.2     1         5.5           18                 0     4
## 4    1    1 22.4     1         5.7           18                 0     1
## 5    1    2 21.6     1         5.4           12                 0     1
## 6    1    2 46.7     1         6.0           18                 0     4
## 7    1    1 31.0     1         6.0           18                 0     3
## 9    0    1 32.6     1         4.5           13                 0     3
##   violator
## 1        0
## 4        0
## 5        0
## 6        0
## 7        0
## 9        0
##     male race  age state time.served max.sentence multiple.offenses crime
## 23     1    1 41.9     1         5.1           16                 0     3
## 165    1    1 32.7     2         5.1           12                 1     1
## 248    1    2 28.4     3         2.2           12                 1     1
## 503    1    2 49.0     4         4.9           16                 0     1
## 565    1    1 39.7     4         5.6           18                 1     1
## 616    1    2 43.7     4         5.6           18                 1     4
##     violator
## 23         0
## 165        0
## 248        1
## 503        0
## 565        1
## 616        0
##     male race  age state time.served max.sentence multiple.offenses crime
## 666    1    2 36.4     4         2.9           12                 1     2
## 667    1    2 25.4     4         3.4           14                 1     1
## 668    1    2 28.2     4         6.0           18                 1     4
## 670    0    2 42.0     4         3.3           13                 1     1
## 671    1    1 47.1     4         4.2           16                 1     1
## 675    1    1 47.8     1         6.0           12                 0     4
##     violator
## 666        0
## 667        0
## 668        0
## 670        0
## 671        0
## 675        0
## 'data.frame':	473 obs. of  9 variables:
##  $ male             : int  1 1 1 1 1 0 0 1 1 1 ...
##  $ race             : int  1 1 2 2 1 1 2 1 1 1 ...
##  $ age              : num  33.2 22.4 21.6 46.7 31 32.6 28.4 20.5 30.1 37.8 ...
##  $ state            : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ time.served      : num  5.5 5.7 5.4 6 6 4.5 4.5 5.9 5.3 5.3 ...
##  $ max.sentence     : int  18 18 12 18 18 13 12 12 16 8 ...
##  $ multiple.offenses: int  0 0 0 0 0 0 1 0 0 0 ...
##  $ crime            : int  4 1 1 4 3 3 1 1 3 3 ...
##  $ violator         : int  0 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "comment")= chr "glb_entity_df"
```

```r
script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# male: 1 if the parolee is male, 0 if female
# race: 1 if the parolee is white, 2 otherwise
# age: the parolee's age (in years) when he or she was released from prison
# state: a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. The three states were selected due to having a high representation in the dataset.
# time.served: the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
# max.sentence: the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
# multiple.offenses: 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
# crime: a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
# violator: 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

        state.fctr=factor(state, 
                    as.factor(union(obs_df$state, obs_twin_df$state))), 
        crime.fctr=factor(crime, 
                    as.factor(union(obs_df$crime, obs_twin_df$crime))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<max_n_val>"), 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>),        
        .rnorm=rnorm(1)
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_newent_df)
```

```
## Loading required package: plyr
```

```
##       male             race            age            state      
##  Min.   :0.0000   Min.   :1.000   Min.   :18.40   Min.   :1.000  
##  1st Qu.:1.0000   1st Qu.:1.000   1st Qu.:25.70   1st Qu.:2.000  
##  Median :1.0000   Median :1.000   Median :34.20   Median :3.000  
##  Mean   :0.8203   Mean   :1.414   Mean   :34.93   Mean   :2.873  
##  3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:43.20   3rd Qu.:4.000  
##  Max.   :1.0000   Max.   :2.000   Max.   :67.00   Max.   :4.000  
##   time.served     max.sentence   multiple.offenses     crime     
##  Min.   :0.000   Min.   : 1.00   Min.   :0.0000    Min.   :1.00  
##  1st Qu.:3.200   1st Qu.:12.00   1st Qu.:0.0000    1st Qu.:1.00  
##  Median :4.400   Median :12.00   Median :1.0000    Median :2.00  
##  Mean   :4.197   Mean   :13.14   Mean   :0.5285    Mean   :2.03  
##  3rd Qu.:5.200   3rd Qu.:15.00   3rd Qu.:1.0000    3rd Qu.:3.00  
##  Max.   :6.000   Max.   :18.00   Max.   :1.0000    Max.   :4.00  
##     violator      state.fctr crime.fctr     .rnorm      
##  Min.   :0.0000   1:105      4: 70      Min.   :0.1975  
##  1st Qu.:0.0000   2: 85      1:228      1st Qu.:0.1975  
##  Median :0.0000   3: 48      3:102      Median :0.1975  
##  Mean   :0.1163   4:235      2: 73      Mean   :0.1975  
##  3rd Qu.:0.0000                         3rd Qu.:0.1975  
##  Max.   :1.0000                         Max.   :0.1975  
##              male              race               age             state 
##                 0                 0                 0                 0 
##       time.served      max.sentence multiple.offenses             crime 
##                 0                 0                 0                 0 
##          violator        state.fctr        crime.fctr            .rnorm 
##                 0                 0                 0                 0
```

```r
glb_newent_df <- add_new_diag_feats(glb_newent_df, glb_entity_df)
```

```
##       male             race            age            state      
##  Min.   :0.0000   Min.   :1.000   Min.   :18.50   Min.   :1.000  
##  1st Qu.:1.0000   1st Qu.:1.000   1st Qu.:24.32   1st Qu.:2.000  
##  Median :1.0000   Median :1.000   Median :32.80   Median :3.000  
##  Mean   :0.7772   Mean   :1.446   Mean   :33.54   Mean   :2.921  
##  3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:41.17   3rd Qu.:4.000  
##  Max.   :1.0000   Max.   :2.000   Max.   :58.50   Max.   :4.000  
##   time.served     max.sentence   multiple.offenses     crime      
##  Min.   :0.500   Min.   : 1.00   Min.   :0.0000    Min.   :1.000  
##  1st Qu.:3.425   1st Qu.:12.00   1st Qu.:0.0000    1st Qu.:1.000  
##  Median :4.400   Median :12.00   Median :1.0000    Median :2.000  
##  Mean   :4.200   Mean   :12.87   Mean   :0.5545    Mean   :2.129  
##  3rd Qu.:5.200   3rd Qu.:15.00   3rd Qu.:1.0000    3rd Qu.:3.000  
##  Max.   :6.000   Max.   :18.00   Max.   :1.0000    Max.   :4.000  
##     violator      state.fctr crime.fctr     .rnorm       
##  Min.   :0.0000   1:38       3:51       Min.   :0.00565  
##  1st Qu.:0.0000   2:35       1:87       1st Qu.:0.00565  
##  Median :0.0000   3:34       2:33       Median :0.00565  
##  Mean   :0.1139   4:95       4:31       Mean   :0.00565  
##  3rd Qu.:0.0000                         3rd Qu.:0.00565  
##  Max.   :1.0000                         Max.   :0.00565  
##              male              race               age             state 
##                 0                 0                 0                 0 
##       time.served      max.sentence multiple.offenses             crime 
##                 0                 0                 0                 0 
##          violator        state.fctr        crime.fctr            .rnorm 
##                 0                 0                 0                 0
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_newent_df
# Check for glb_newent_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(table(sign(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(mycreate_xtab(glb_entity_df, <col1_name>))
# print(mycreate_xtab(glb_entity_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](parole_violations_files/figure-html/inspect_explore_data_1-1.png) 

```r
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>"))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function(entity_df, newent_df) {
    if (!glb_is_separate_newent_dataset) {
        # Combine entity & newent
        union_df <- rbind(mutate(entity_df, .src = "entity"),
                          mutate(newent_df, .src = "newent"))
        union_imputed_df <- union_df[, setdiff(setdiff(names(entity_df), 
                                                       glb_predct_var), 
                                               glb_exclude_vars_as_features)]
        print(summary(union_imputed_df))
    
        require(mice)
        set.seed(glb_mice_complete.seed)
        union_imputed_df <- complete(mice(union_imputed_df))
        print(summary(union_imputed_df))
    
        union_df[, names(union_imputed_df)] <- union_imputed_df[, names(union_imputed_df)]
        print(summary(union_df))
        
        # Partition again
        glb_entity_df <<- subset(union_df, .src == "entity", select=-.src)
        comment(glb_entity_df) <- "entity_df"
        glb_newent_df <<- subset(union_df, .src == "newent", select=-.src)
        comment(glb_newent_df) <- "newent_df"
        
        # Generate summaries
        print(summary(entity_df))
        print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
        print(summary(newent_df))
        print(sapply(names(newent_df), function(col) sum(is.na(newent_df[, col]))))
    
    } else stop("Not implemented yet")
}

if ((sum(sapply(names(glb_entity_df), 
                function(col) sum(is.na(glb_entity_df[, col])))) > 0) | 
    (sum(sapply(names(glb_newent_df), 
                function(col) sum(is.na(glb_newent_df[, col])))) > 0))
    glb_impute_missing_data(glb_entity_df, glb_newent_df)

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_newent_df <- mutate(glb_newent_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_newent_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- 
    myselect_features(glb_entity_df, glb_exclude_vars_as_features, glb_predct_var))
```

```
##                                  id        cor.y   cor.y.abs
## max.sentence           max.sentence -0.107847018 0.107847018
## time.served             time.served -0.087969745 0.087969745
## multiple.offenses multiple.offenses  0.078350351 0.078350351
## race                           race  0.042966934 0.042966934
## male                           male -0.019175165 0.019175165
## age                             age -0.008384572 0.008384572
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, merge(glb_feats_df, 
          mydelete_cor_features(glb_feats_df, glb_entity_df, glb_predct_var, 
                                glb_exclude_vars_as_features), 
          all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##                   max.sentence time.served multiple.offenses         race
## max.sentence        1.00000000  0.12258963        0.02652948  0.036212450
## time.served         0.12258963  1.00000000       -0.13624236 -0.048369724
## multiple.offenses   0.02652948 -0.13624236        1.00000000  0.132451967
## race                0.03621245 -0.04836972        0.13245197  1.000000000
## male                0.10913559 -0.01968401        0.01021472  0.002481396
## age                -0.01230051  0.06922450       -0.08502952 -0.002899988
##                           male          age
## max.sentence       0.109135595 -0.012300514
## time.served       -0.019684013  0.069224495
## multiple.offenses  0.010214717 -0.085029517
## race               0.002481396 -0.002899988
## male               1.000000000 -0.055399873
## age               -0.055399873  1.000000000
##                   max.sentence time.served multiple.offenses        race
## max.sentence        0.00000000  0.12258963        0.02652948 0.036212450
## time.served         0.12258963  0.00000000        0.13624236 0.048369724
## multiple.offenses   0.02652948  0.13624236        0.00000000 0.132451967
## race                0.03621245  0.04836972        0.13245197 0.000000000
## male                0.10913559  0.01968401        0.01021472 0.002481396
## age                 0.01230051  0.06922450        0.08502952 0.002899988
##                          male         age
## max.sentence      0.109135595 0.012300514
## time.served       0.019684013 0.069224495
## multiple.offenses 0.010214717 0.085029517
## race              0.002481396 0.002899988
## male              0.000000000 0.055399873
## age               0.055399873 0.000000000
##                  id        cor.y   cor.y.abs cor.low
## 4 multiple.offenses  0.078350351 0.078350351       1
## 5              race  0.042966934 0.042966934       1
## 1               age -0.008384572 0.008384572       1
## 2              male -0.019175165 0.019175165       1
## 6       time.served -0.087969745 0.087969745       1
## 3      max.sentence -0.107847018 0.107847018       1
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Add dummy model - random variable
#   Potential Enhancements:
#       For classifiers, it shd generate proba/outcomes that mimics the freq
#           distribution of glb_predct_var values; Right now it always generates
#           0 (most frequent ?)
ret_lst <- myrun_mdl_fn(indep_vars_vctr=".rnorm",
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.4972  -0.4972  -0.4972  -0.4972   2.0745  
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -2.0281     0.1434  -14.14   <2e-16 ***
## .rnorm            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 340.04  on 472  degrees of freedom
## Residual deviance: 340.04  on 472  degrees of freedom
## AIC: 342.04
## 
## Number of Fisher Scoring iterations: 4
## 
##    feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit
## 1 .rnorm   473       NA       NA           NA 4603.037      NA 342.0351
##   auc.fit auc.OOB
## 1     0.5     0.5
```

```r
glb_dmy_mdl <- glb_mdl

# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5492  -0.5492  -0.4335  -0.4335   2.1961  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -2.3175     0.2344  -9.888   <2e-16 ***
## multiple.offenses   0.5022     0.2969   1.691   0.0908 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 340.04  on 472  degrees of freedom
## Residual deviance: 337.09  on 471  degrees of freedom
## AIC: 341.09
## 
## Number of Fisher Scoring iterations: 5
## 
##               feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB
## 2 multiple.offenses   473       NA       NA           NA 4807.832      NA
## 1            .rnorm   473       NA       NA           NA 4603.037      NA
##    AIC.fit   auc.fit   auc.OOB
## 2 341.0894 0.5610048 0.6287345
## 1 342.0351 0.5000000 0.5000000
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, 
              subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                        glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df)    

# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, 
                                               cor.low == 1)[, "id"],
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9783  -0.5289  -0.4662  -0.3841   2.4276  
## 
## Coefficients:
##                     Estimate Std. Error z value Pr(>|z|)  
## (Intercept)       -5.654e-01  1.010e+00  -0.560   0.5756  
## multiple.offenses  4.437e-01  3.047e-01   1.456   0.1454  
## race               2.295e-01  2.944e-01   0.780   0.4355  
## age               -8.632e-05  1.415e-02  -0.006   0.9951  
## male              -7.800e-02  3.699e-01  -0.211   0.8330  
## time.served       -1.600e-01  1.167e-01  -1.371   0.1704  
## max.sentence      -1.041e-01  4.765e-02  -2.185   0.0289 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 340.04  on 472  degrees of freedom
## Residual deviance: 329.03  on 466  degrees of freedom
## AIC: 343.03
## 
## Number of Fisher Scoring iterations: 5
## 
##                                                           feats n.fit
## 3 multiple.offenses, race, age, male, time.served, max.sentence   473
## 2                                             multiple.offenses   473
## 1                                                        .rnorm   473
##   R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 3       NA       NA           NA 5331.618      NA 343.0286 0.6109178
## 2       NA       NA           NA 4807.832      NA 341.0894 0.5610048
## 1       NA       NA           NA 4603.037      NA 342.0351 0.5000000
##     auc.OOB
## 3 0.7563760
## 2 0.6287345
## 1 0.5000000
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(glb_predct_var, glb_exclude_vars_as_features)),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7041  -0.4236  -0.2719  -0.1690   2.8375  
## 
## Coefficients:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -4.2529200  1.4191897  -2.997  0.00273 ** 
## male               0.3869904  0.4379613   0.884  0.37690    
## race               0.8867192  0.3950660   2.244  0.02480 *  
## age               -0.0001756  0.0160852  -0.011  0.99129    
## time.served       -0.1238867  0.1204230  -1.029  0.30359    
## max.sentence       0.0802954  0.0553747   1.450  0.14705    
## multiple.offenses  1.6119919  0.3853050   4.184 2.87e-05 ***
## state.fctr2        0.4433007  0.4816619   0.920  0.35739    
## state.fctr3        0.8349797  0.5562704   1.501  0.13335    
## state.fctr4       -3.3967878  0.6115860  -5.554 2.79e-08 ***
## crime.fctr1        0.0117627  0.5713035   0.021  0.98357    
## crime.fctr3       -0.2663428  0.6412857  -0.415  0.67790    
## crime.fctr2        0.6954770  0.6714835   1.036  0.30033    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 340.04  on 472  degrees of freedom
## Residual deviance: 251.48  on 460  degrees of freedom
## AIC: 277.48
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                   feats
## 4 male, race, age, time.served, max.sentence, multiple.offenses, state.fctr, crime.fctr
## 3                         multiple.offenses, race, age, male, time.served, max.sentence
## 2                                                                     multiple.offenses
## 1                                                                                .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit   SSE.fit SSE.OOB  AIC.fit
## 4   473       NA       NA           NA 10415.026      NA 277.4848
## 3   473       NA       NA           NA  5331.618      NA 343.0286
## 2   473       NA       NA           NA  4807.832      NA 341.0894
## 1   473       NA       NA           NA  4603.037      NA 342.0351
##     auc.fit   auc.OOB
## 4 0.8459765 0.8945834
## 3 0.6109178 0.7563760
## 2 0.5610048 0.6287345
## 1 0.5000000 0.5000000
```

```r
glb_sel_mdl <- glb_mdl

# User specified - easier to exclude features
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
#     union(union(glb_predct_var, glb_exclude_vars_as_features), c("<feat1_name>", "<feat2_name>"))),
#                         glb_predct_var, glb_predct_var_name,
#                         fit_df=glb_entity_df, OOB_df=glb_newent_df)

# User specified - easier to include features
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("<feat1_name>", "<feat2_name>"),
#                         glb_predct_var, glb_predct_var_name,
#                         fit_df=glb_entity_df, OOB_df=glb_newent_df)

# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
if (glb_is_regression)
    print(myplot_scatter(plot_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))

if (glb_is_classification) {
    # Lower AIC is better
    plot_models_df[, "inv.AIC.fit"] <- 1.0 / plot_models_df[, "AIC.fit"] 
    print(myplot_scatter(plot_models_df, "inv.AIC.fit", "auc.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}
```

![](parole_violations_files/figure-html/run_models-1.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats(lcl_sel_mdl=glb_sel_mdl, 
                                          lcl_entity_df=glb_entity_df))
```

```
##                                  id         Pr.z
## state.fctr               crime.fctr 2.791039e-08
## multiple.offenses multiple.offenses 2.868314e-05
## race                           race 2.480127e-02
## max.sentence           max.sentence 1.470485e-01
## crime.fctr               state.fctr 3.003272e-01
## time.served             time.served 3.035912e-01
## male                           male 3.769026e-01
## age                             age 9.912890e-01
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl        
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_entity_df, type="response")

    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_entity_df[, glb_predct_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    # 0 & 1 does not generate outcomes for certain categories
    thresholds_df <- data.frame(threshold=seq(0.0, 1.0, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     thresholds_df[row_ix, "threshold"], 
                                     glb_predct_var, glb_predct_var_name))
    print(thresholds_df)
    print(myplot_line(thresholds_df, "threshold", "f.score"))
    
    glb_clf_proba_threshold <- thresholds_df[which.max(thresholds_df$f.score), 
                                             "threshold"]
    # This should change to maximize f.score.OOB ???
    print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.fit",
                  glb_clf_proba_threshold))

    # Sync w/ HW
    glb_clf_proba_threshold <- 0.5
    print(sprintf("Classifier Probability Threshold: %0.4f per HW specs",
                  glb_clf_proba_threshold))

    glb_entity_df[, glb_predct_var_name] <- 
        (glb_entity_df[, paste0(glb_predct_var_name, ".proba")] >= 
             glb_clf_proba_threshold) * 1.0
    print(mycreate_xtab(glb_entity_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     glb_clf_proba_threshold, 
                                     glb_predct_var, glb_predct_var_name)))    
}    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7041  -0.4236  -0.2719  -0.1690   2.8375  
## 
## Coefficients:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -4.2529200  1.4191897  -2.997  0.00273 ** 
## crime.fctr1        0.0117627  0.5713035   0.021  0.98357    
## crime.fctr3       -0.2663428  0.6412857  -0.415  0.67790    
## crime.fctr2        0.6954770  0.6714835   1.036  0.30033    
## multiple.offenses  1.6119919  0.3853050   4.184 2.87e-05 ***
## race               0.8867192  0.3950660   2.244  0.02480 *  
## max.sentence       0.0802954  0.0553747   1.450  0.14705    
## state.fctr2        0.4433007  0.4816619   0.920  0.35739    
## state.fctr3        0.8349797  0.5562704   1.501  0.13335    
## state.fctr4       -3.3967878  0.6115860  -5.554 2.79e-08 ***
## time.served       -0.1238867  0.1204230  -1.029  0.30359    
## male               0.3869904  0.4379613   0.884  0.37690    
## age               -0.0001756  0.0160852  -0.011  0.99129    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 340.04  on 472  degrees of freedom
## Residual deviance: 251.48  on 460  degrees of freedom
## AIC: 277.48
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                   feats
## 4 male, race, age, time.served, max.sentence, multiple.offenses, state.fctr, crime.fctr
## 3                         multiple.offenses, race, age, male, time.served, max.sentence
## 2                                                                     multiple.offenses
## 1                                                                                .rnorm
## 5 crime.fctr, multiple.offenses, race, max.sentence, state.fctr, time.served, male, age
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit   SSE.fit SSE.OOB  AIC.fit
## 4   473       NA       NA           NA 10415.026      NA 277.4848
## 3   473       NA       NA           NA  5331.618      NA 343.0286
## 2   473       NA       NA           NA  4807.832      NA 341.0894
## 1   473       NA       NA           NA  4603.037      NA 342.0351
## 5   473       NA       NA           NA 10415.026      NA 277.4848
##     auc.fit   auc.OOB
## 4 0.8459765 0.8945834
## 3 0.6109178 0.7563760
## 2 0.5610048 0.6287345
## 1 0.5000000 0.5000000
## 5 0.8459765        NA
```

![](parole_violations_files/figure-html/fit_training.all-1.png) 

```
##    threshold    f.score
## 1        0.0 0.20833333
## 2        0.1 0.42857143
## 3        0.2 0.47887324
## 4        0.3 0.45544554
## 5        0.4 0.46153846
## 6        0.5 0.38961039
## 7        0.6 0.26865672
## 8        0.7 0.21875000
## 9        0.8 0.07017544
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](parole_violations_files/figure-html/fit_training.all-2.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
## [1] "Classifier Probability Threshold: 0.5000 per HW specs"
##   violator violator.predict.0 violator.predict.1
## 1        0                411                  7
## 2        1                 40                 15
## [1] "f.score=0.3896"
```

```r
print(glb_feats_df <- mymerge_feats_Pr.z(glb_feats_df, glb_sel_mdl, glb_entity_df))
```

```
##                  id        cor.y   cor.y.abs cor.low         Pr.z
## 2        crime.fctr           NA          NA      NA 2.791039e-08
## 5 multiple.offenses  0.078350351 0.078350351       1 2.868314e-05
## 6              race  0.042966934 0.042966934       1 2.480127e-02
## 4      max.sentence -0.107847018 0.107847018       1 1.470485e-01
## 7        state.fctr           NA          NA      NA 3.003272e-01
## 8       time.served -0.087969745 0.087969745       1 3.035912e-01
## 3              male -0.019175165 0.019175165       1 3.769026e-01
## 1               age -0.008384572 0.008384572       1 9.912890e-01
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name)
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        if (nrow(plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)) == 0)
            warning("No coefficients in selected model are statistically significant")
        else print(myplot_prediction_classification(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                               plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name, glb_id_vars)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_entity_df)
```

![](parole_violations_files/figure-html/fit_training.all-3.png) ![](parole_violations_files/figure-html/fit_training.all-4.png) ![](parole_violations_files/figure-html/fit_training.all-5.png) 

```
##    male race  age state time.served max.sentence multiple.offenses crime
## 26    1    1 29.5     1         3.9           16                 0     1
## 36    1    1 37.2     1         2.0           16                 0     1
## 42    1    1 29.9     1         4.8           12                 1     1
## 43    1    1 27.5     1         3.8           12                 1     3
## 45    1    1 34.2     1         4.4           12                 1     1
## 53    1    1 34.9     1         4.5           12                 1     4
##    violator state.fctr crime.fctr    .rnorm violator.predict.proba
## 26        1          1          1 0.1974656              0.1023795
## 36        1          1          1 0.1974656              0.1259747
## 42        1          1          1 0.1974656              0.2705530
## 43        1          1          3 0.1974656              0.2412908
## 45        1          1          1 0.1974656              0.2802909
## 53        1          1          4 0.1974656              0.2754204
##    violator.predict violator.fctr violator.predict.accurate .label
## 26                0             1                     FALSE    .26
## 36                0             1                     FALSE    .36
## 42                0             1                     FALSE    .42
## 43                0             1                     FALSE    .43
## 45                0             1                     FALSE    .45
## 53                0             1                     FALSE    .53
##     male race  age state time.served max.sentence multiple.offenses crime
## 53     1    1 34.9     1         4.5           12                 1     4
## 82     1    2 20.6     2         4.2           12                 0     3
## 243    1    2 20.3     3         5.2           12                 1     3
## 245    0    2 43.2     3         5.8           12                 1     2
## 565    1    1 39.7     4         5.6           18                 1     1
## 599    1    2 41.1     4         3.0           14                 1     1
##     violator state.fctr crime.fctr    .rnorm violator.predict.proba
## 53         1          1          4 0.1974656             0.27542036
## 82         1          2          3 0.1974656             0.18604287
## 243        0          3          3 0.1974656             0.59962582
## 245        0          3          2 0.1974656             0.71102799
## 565        1          4          1 0.1974656             0.01785134
## 599        1          4          1 0.1974656             0.04227896
##     violator.predict violator.fctr violator.predict.accurate .label
## 53                 0             1                     FALSE    .53
## 82                 0             1                     FALSE    .82
## 243                1             0                     FALSE   .243
## 245                1             0                     FALSE   .245
## 565                0             1                     FALSE   .565
## 599                0             1                     FALSE   .599
##     male race  age state time.served max.sentence multiple.offenses crime
## 404    1    2 28.5     4         4.5           18                 1     4
## 565    1    1 39.7     4         5.6           18                 1     1
## 598    1    2 29.9     4         4.2           16                 1     4
## 599    1    2 41.1     4         3.0           14                 1     1
## 613    1    2 47.0     4         3.0           15                 1     2
## 659    1    1 39.2     4         4.5           17                 1     4
##     violator state.fctr crime.fctr    .rnorm violator.predict.proba
## 404        1          4          4 0.1974656             0.04767653
## 565        1          4          1 0.1974656             0.01785134
## 598        1          4          4 0.1974656             0.04236533
## 599        1          4          1 0.1974656             0.04227896
## 613        1          4          2 0.1974656             0.08648779
## 659        1          4          4 0.1974656             0.01864495
##     violator.predict violator.fctr violator.predict.accurate .label
## 404                0             1                     FALSE   .404
## 565                0             1                     FALSE   .565
## 598                0             1                     FALSE   .598
## 599                0             1                     FALSE   .599
## 613                0             1                     FALSE   .613
## 659                0             1                     FALSE   .659
```

![](parole_violations_files/figure-html/fit_training.all-6.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_newent_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_newent_df, type="response")

if (glb_is_classification) {
    # Compute selected model predictions
    glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_newent_df, type="response")
    print(max(glb_newent_df[, paste0(glb_predct_var_name, ".proba")]))
    glb_newent_df[, glb_predct_var_name] <- 
        (predict(glb_sel_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0

    # Compute dummy model predictions
    glb_newent_df[, paste0(glb_predct_var, ".preddmy.proba")] <- 
        predict(glb_dmy_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, paste0(glb_predct_var, ".preddmy")] <- 
        (predict(glb_dmy_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0
}
```

```
## [1] 0.9072791
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```r
myprint_df(glb_newent_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##    violator violator.predict
## 2         0                0
## 3         0                0
## 8         0                0
## 10        0                0
## 19        0                0
## 27        0                0
##     violator violator.predict
## 195        1                1
## 230        0                1
## 511        0                0
## 590        0                0
## 618        0                0
## 629        0                0
##     violator violator.predict
## 661        0                0
## 665        0                0
## 669        0                0
## 672        0                0
## 673        0                0
## 674        0                0
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2) / nrow(glb_newent_df)) ^ 0.5))                        
    print(myplot_scatter(glb_newent_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
                         
    glb_newent_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_newent_df[, glb_predct_var_name] - glb_newent_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_newent_df)))                                                      

#     glb_newent_df[, "<Output Pred variable>"] <- func(glb_newent_df[, glb_pred_var_name])                         
}                         

if (glb_is_classification) {
    ROCRpred <- prediction(glb_newent_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_newent_df[, glb_predct_var])
    print(sprintf("auc=%0.4f", auc <- as.numeric(performance(ROCRpred, "auc")@y.values)))   
    
    print(sprintf("probability threshold=%0.4f", glb_clf_proba_threshold))
    print(newent_conf_df <- mycreate_xtab(glb_newent_df, 
                                        c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score.sel=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=glb_predct_var_name)))
    print(sprintf("sensitivity=%0.4f", newent_conf_df[2, 3] / 
                      (newent_conf_df[2, 3] + newent_conf_df[2, 2])))
    print(sprintf("specificity=%0.4f", newent_conf_df[1, 2] / 
                      (newent_conf_df[1, 2] + newent_conf_df[1, 3])))
    print(sprintf("accuracy=%0.4f", (newent_conf_df[1, 2] + newent_conf_df[2, 3]) / 
                      (newent_conf_df[1, 2] + newent_conf_df[2, 3] + 
                       newent_conf_df[1, 3] + newent_conf_df[2, 2])))
    
    print(mycreate_xtab(glb_newent_df, c(glb_predct_var, paste0(glb_predct_var, ".preddmy"))))
    print(sprintf("f.score.dmy=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_dmy_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=paste0(glb_predct_var, ".preddmy"))))
}    
```

```
## [1] "auc=0.8946"
## [1] "probability threshold=0.5000"
##   violator violator.predict.0 violator.predict.1
## 1        0                167                 12
## 2        1                 11                 12
## [1] "f.score.sel=0.5106"
## [1] "sensitivity=0.5217"
## [1] "specificity=0.9330"
## [1] "accuracy=0.8861"
##   violator violator.preddmy.0
## 1        0                179
## 2        1                 23
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## [1] "f.score.dmy=0.0000"
```

```r
glb_analytics_diag_plots(glb_newent_df)
```

![](parole_violations_files/figure-html/predict_newdata-1.png) ![](parole_violations_files/figure-html/predict_newdata-2.png) ![](parole_violations_files/figure-html/predict_newdata-3.png) 

```
##    male race  age state time.served max.sentence multiple.offenses crime
## 46    1    2 36.5     1         3.9           12                 1     4
## 47    1    1 33.5     1         4.2           12                 1     1
## 51    1    1 37.3     1         4.6           12                 1     1
## 60    1    2 38.8     1         3.7           12                 1     1
## 66    0    1 39.7     1         4.4           12                 1     3
## 81    1    1 33.7     2         5.2           12                 0     3
##    violator state.fctr crime.fctr      .rnorm violator.predict.proba
## 46        1          1          4 0.005649559             0.49836928
## 47        1          1          1 0.005649559             0.28534139
## 51        1          1          1 0.005649559             0.27521137
## 60        0          1          1 0.005649559             0.50740276
## 66        1          1          3 0.005649559             0.16671630
## 81        1          2          3 0.005649559             0.07664446
##    violator.predict violator.preddmy.proba violator.preddmy violator.fctr
## 46                0              0.1162791                0             1
## 47                0              0.1162791                0             1
## 51                0              0.1162791                0             1
## 60                1              0.1162791                0             0
## 66                0              0.1162791                0             1
## 81                0              0.1162791                0             1
##    violator.predict.accurate .label
## 46                     FALSE    .46
## 47                     FALSE    .47
## 51                     FALSE    .51
## 60                     FALSE    .60
## 66                     FALSE    .66
## 81                     FALSE    .81
##     male race  age state time.served max.sentence multiple.offenses crime
## 51     1    1 37.3     1         4.6           12                 1     1
## 130    1    2 18.7     2         5.2           12                 0     1
## 204    1    2 35.9     3         5.7           10                 1     3
## 205    1    2 28.8     3         5.7            8                 1     1
## 216    1    2 27.3     3         0.8           11                 1     1
## 219    1    1 52.5     3         5.6           11                 0     1
##     violator state.fctr crime.fctr      .rnorm violator.predict.proba
## 51         1          1          1 0.005649559              0.2752114
## 130        1          2          1 0.005649559              0.2105891
## 204        0          3          3 0.005649559              0.5445398
## 205        0          3          1 0.005649559              0.5738054
## 216        0          3          1 0.005649559              0.7587042
## 219        1          3          1 0.005649559              0.1243132
##     violator.predict violator.preddmy.proba violator.preddmy violator.fctr
## 51                 0              0.1162791                0             1
## 130                0              0.1162791                0             1
## 204                1              0.1162791                0             0
## 205                1              0.1162791                0             0
## 216                1              0.1162791                0             0
## 219                0              0.1162791                0             1
##     violator.predict.accurate .label
## 51                      FALSE    .51
## 130                     FALSE   .130
## 204                     FALSE   .204
## 205                     FALSE   .205
## 216                     FALSE   .216
## 219                     FALSE   .219
##     male race  age state time.served max.sentence multiple.offenses crime
## 229    1    2 21.9     3         1.7           11                 1     2
## 230    1    2 21.2     3         1.9           12                 1     1
## 232    0    1 25.3     3         6.0            6                 1     2
## 257    0    2 22.8     3         5.9           12                 0     1
## 269    1    1 45.0     3         5.0            6                 0     1
## 647    1    2 44.1     4         4.0           14                 1     2
##     violator state.fctr crime.fctr      .rnorm violator.predict.proba
## 229        0          3          2 0.005649559             0.84796816
## 230        0          3          1 0.005649559             0.74850970
## 232        1          3          2 0.005649559             0.37995403
## 257        1          3          1 0.005649559             0.19716151
## 269        1          3          1 0.005649559             0.09295875
## 647        1          4          2 0.005649559             0.07169321
##     violator.predict violator.preddmy.proba violator.preddmy violator.fctr
## 229                1              0.1162791                0             0
## 230                1              0.1162791                0             0
## 232                0              0.1162791                0             1
## 257                0              0.1162791                0             1
## 269                0              0.1162791                0             1
## 647                0              0.1162791                0             1
##     violator.predict.accurate .label
## 229                     FALSE   .229
## 230                     FALSE   .230
## 232                     FALSE   .232
## 257                     FALSE   .257
## 269                     FALSE   .269
## 647                     FALSE   .647
```

![](parole_violations_files/figure-html/predict_newdata-4.png) 

```r
print(tst_proba <- predict(glb_sel_mdl, 
              newdata=data.frame(male=1, race=1, age=50, state.fctr=factor(1), 
                                 time.served=3,
                                 max.sentence=12, multiple.offenses=0, 
                                 crime.fctr=factor(2)), 
              type="response"))
```

```
##        1 
## 0.154383
```

```r
print(tst_odds <- tst_proba / (1 - tst_proba))
```

```
##         1 
## 0.1825685
```

```r
print(tst_log_odds <- predict(glb_sel_mdl, 
              newdata=data.frame(male=1, race=1, age=50, state.fctr=factor(1),
                                 time.served=3,
                                 max.sentence=12, multiple.offenses=0,
                                 crime.fctr=factor(2)), 
              type="terms"))
```

```
##   crime.fctr multiple.offenses       race max.sentence state.fctr
## 1  0.6399067        -0.8520042 -0.3674355  -0.09115987   1.523225
##   time.served       male          age
## 1   0.1482974 0.06954373 -0.002646931
## attr(,"constant")
## [1] -2.768356
```

```r
print(tst_log_odds <- attr(tst_log_odds, "constant"))
```

```
## [1] -2.768356
```

```r
print(tst_odds <- exp(tst_log_odds))
```

```
## [1] 0.06276508
```

```r
print(tst_odds / (1 - tst_odds))
```

```
## [1] 0.06696835
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.2 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ROCR_1.0-6      gplots_2.16.0   reshape2_1.4.1  plyr_1.8.1     
## [5] caTools_1.17.1  doBy_4.5-13     survival_2.38-1 ggplot2_1.0.1  
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6       colorspace_1.2-6   digest_0.6.8      
##  [4] evaluate_0.5.5     formatR_1.0        gdata_2.13.3      
##  [7] grid_3.1.3         gtable_0.1.2       gtools_3.4.1      
## [10] htmltools_0.2.6    KernSmooth_2.23-14 knitr_1.9         
## [13] labeling_0.3       lattice_0.20-30    MASS_7.3-39       
## [16] Matrix_1.1-5       munsell_0.4.2      proto_0.3-10      
## [19] Rcpp_0.11.5        rmarkdown_0.5.1    scales_0.2.4      
## [22] splines_3.1.3      stringr_0.6.2      tools_3.1.3       
## [25] yaml_2.1.13
```
