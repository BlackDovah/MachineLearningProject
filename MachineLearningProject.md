---
title: "MachineLearningProject"
author: "Samuel Aboelkhir"
date: "2024-06-25"
output: 
   html_document:
     toc: yes
     toc_float: yes
     keep_md: TRUE
     df_print: paged
---



# Abstract :

- In this analysis, we will be preparing a prediction model, with the purpose of predicting the quality of weight lift training for weight lifters. In the provided data, we have 5 categories describing the quality of training, where "A" is correct training, and "B" through "E" are training with common errors. The data was collected from 6 different participants with sensors attached to various parts of their body.

# Examining and cleanining the data :

- For this analysis we will be needing the caret and e1071 packages.


```r
library(caret)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: lattice
```

```r
library(e1071)

pmlTrain = read.csv("pml-training.csv")
pmlTest = read.csv("pml-testing.csv")
```

- Upon examining the data, we can see multiple variables that won't be relevant in the analysis, such as time stamps, windows, and the participant names, as well as multiple variables with NA or empty ("") variables.


```r
str(pmlTrain)
```

```
## 'data.frame':	19622 obs. of  160 variables:
##  $ X                       : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ user_name               : chr  "carlitos" "carlitos" "carlitos" "carlitos" ...
##  $ raw_timestamp_part_1    : int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
##  $ raw_timestamp_part_2    : int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
##  $ cvtd_timestamp          : chr  "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" ...
##  $ new_window              : chr  "no" "no" "no" "no" ...
##  $ num_window              : int  11 11 11 12 12 12 12 12 12 12 ...
##  $ roll_belt               : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt              : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt                : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt        : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ kurtosis_roll_belt      : chr  "" "" "" "" ...
##  $ kurtosis_picth_belt     : chr  "" "" "" "" ...
##  $ kurtosis_yaw_belt       : chr  "" "" "" "" ...
##  $ skewness_roll_belt      : chr  "" "" "" "" ...
##  $ skewness_roll_belt.1    : chr  "" "" "" "" ...
##  $ skewness_yaw_belt       : chr  "" "" "" "" ...
##  $ max_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_belt            : chr  "" "" "" "" ...
##  $ min_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_belt            : chr  "" "" "" "" ...
##  $ amplitude_roll_belt     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_pitch_belt    : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_yaw_belt      : chr  "" "" "" "" ...
##  $ var_total_accel_belt    : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_roll_belt        : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_pitch_belt       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_yaw_belt         : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ gyros_belt_x            : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y            : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z            : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x            : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y            : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z            : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x           : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y           : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z           : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm                : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm               : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm                 : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm         : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ var_accel_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_roll_arm         : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_pitch_arm        : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_yaw_arm          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ gyros_arm_x             : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y             : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z             : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x             : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y             : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z             : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x            : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y            : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z            : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ kurtosis_roll_arm       : chr  "" "" "" "" ...
##  $ kurtosis_picth_arm      : chr  "" "" "" "" ...
##  $ kurtosis_yaw_arm        : chr  "" "" "" "" ...
##  $ skewness_roll_arm       : chr  "" "" "" "" ...
##  $ skewness_pitch_arm      : chr  "" "" "" "" ...
##  $ skewness_yaw_arm        : chr  "" "" "" "" ...
##  $ max_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_roll_arm      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_pitch_arm     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_yaw_arm       : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ roll_dumbbell           : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell          : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell            : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ kurtosis_roll_dumbbell  : chr  "" "" "" "" ...
##  $ kurtosis_picth_dumbbell : chr  "" "" "" "" ...
##  $ kurtosis_yaw_dumbbell   : chr  "" "" "" "" ...
##  $ skewness_roll_dumbbell  : chr  "" "" "" "" ...
##  $ skewness_pitch_dumbbell : chr  "" "" "" "" ...
##  $ skewness_yaw_dumbbell   : chr  "" "" "" "" ...
##  $ max_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_dumbbell        : chr  "" "" "" "" ...
##  $ min_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_dumbbell        : chr  "" "" "" "" ...
##  $ amplitude_roll_dumbbell : num  NA NA NA NA NA NA NA NA NA NA ...
##   [list output truncated]
```

```r
summary(pmlTrain)
```

```
##        X          user_name         raw_timestamp_part_1 raw_timestamp_part_2
##  Min.   :    1   Length:19622       Min.   :1.322e+09    Min.   :   294      
##  1st Qu.: 4906   Class :character   1st Qu.:1.323e+09    1st Qu.:252912      
##  Median : 9812   Mode  :character   Median :1.323e+09    Median :496380      
##  Mean   : 9812                      Mean   :1.323e+09    Mean   :500656      
##  3rd Qu.:14717                      3rd Qu.:1.323e+09    3rd Qu.:751891      
##  Max.   :19622                      Max.   :1.323e+09    Max.   :998801      
##                                                                              
##  cvtd_timestamp      new_window          num_window      roll_belt     
##  Length:19622       Length:19622       Min.   :  1.0   Min.   :-28.90  
##  Class :character   Class :character   1st Qu.:222.0   1st Qu.:  1.10  
##  Mode  :character   Mode  :character   Median :424.0   Median :113.00  
##                                        Mean   :430.6   Mean   : 64.41  
##                                        3rd Qu.:644.0   3rd Qu.:123.00  
##                                        Max.   :864.0   Max.   :162.00  
##                                                                        
##    pitch_belt          yaw_belt       total_accel_belt kurtosis_roll_belt
##  Min.   :-55.8000   Min.   :-180.00   Min.   : 0.00    Length:19622      
##  1st Qu.:  1.7600   1st Qu.: -88.30   1st Qu.: 3.00    Class :character  
##  Median :  5.2800   Median : -13.00   Median :17.00    Mode  :character  
##  Mean   :  0.3053   Mean   : -11.21   Mean   :11.31                      
##  3rd Qu.: 14.9000   3rd Qu.:  12.90   3rd Qu.:18.00                      
##  Max.   : 60.3000   Max.   : 179.00   Max.   :29.00                      
##                                                                          
##  kurtosis_picth_belt kurtosis_yaw_belt  skewness_roll_belt skewness_roll_belt.1
##  Length:19622        Length:19622       Length:19622       Length:19622        
##  Class :character    Class :character   Class :character   Class :character    
##  Mode  :character    Mode  :character   Mode  :character   Mode  :character    
##                                                                                
##                                                                                
##                                                                                
##                                                                                
##  skewness_yaw_belt  max_roll_belt     max_picth_belt  max_yaw_belt      
##  Length:19622       Min.   :-94.300   Min.   : 3.00   Length:19622      
##  Class :character   1st Qu.:-88.000   1st Qu.: 5.00   Class :character  
##  Mode  :character   Median : -5.100   Median :18.00   Mode  :character  
##                     Mean   : -6.667   Mean   :12.92                     
##                     3rd Qu.: 18.500   3rd Qu.:19.00                     
##                     Max.   :180.000   Max.   :30.00                     
##                     NA's   :19216     NA's   :19216                     
##  min_roll_belt     min_pitch_belt  min_yaw_belt       amplitude_roll_belt
##  Min.   :-180.00   Min.   : 0.00   Length:19622       Min.   :  0.000    
##  1st Qu.: -88.40   1st Qu.: 3.00   Class :character   1st Qu.:  0.300    
##  Median :  -7.85   Median :16.00   Mode  :character   Median :  1.000    
##  Mean   : -10.44   Mean   :10.76                      Mean   :  3.769    
##  3rd Qu.:   9.05   3rd Qu.:17.00                      3rd Qu.:  2.082    
##  Max.   : 173.00   Max.   :23.00                      Max.   :360.000    
##  NA's   :19216     NA's   :19216                      NA's   :19216      
##  amplitude_pitch_belt amplitude_yaw_belt var_total_accel_belt avg_roll_belt   
##  Min.   : 0.000       Length:19622       Min.   : 0.000       Min.   :-27.40  
##  1st Qu.: 1.000       Class :character   1st Qu.: 0.100       1st Qu.:  1.10  
##  Median : 1.000       Mode  :character   Median : 0.200       Median :116.35  
##  Mean   : 2.167                          Mean   : 0.926       Mean   : 68.06  
##  3rd Qu.: 2.000                          3rd Qu.: 0.300       3rd Qu.:123.38  
##  Max.   :12.000                          Max.   :16.500       Max.   :157.40  
##  NA's   :19216                           NA's   :19216        NA's   :19216   
##  stddev_roll_belt var_roll_belt     avg_pitch_belt    stddev_pitch_belt
##  Min.   : 0.000   Min.   :  0.000   Min.   :-51.400   Min.   :0.000    
##  1st Qu.: 0.200   1st Qu.:  0.000   1st Qu.:  2.025   1st Qu.:0.200    
##  Median : 0.400   Median :  0.100   Median :  5.200   Median :0.400    
##  Mean   : 1.337   Mean   :  7.699   Mean   :  0.520   Mean   :0.603    
##  3rd Qu.: 0.700   3rd Qu.:  0.500   3rd Qu.: 15.775   3rd Qu.:0.700    
##  Max.   :14.200   Max.   :200.700   Max.   : 59.700   Max.   :4.000    
##  NA's   :19216    NA's   :19216     NA's   :19216     NA's   :19216    
##  var_pitch_belt    avg_yaw_belt      stddev_yaw_belt    var_yaw_belt      
##  Min.   : 0.000   Min.   :-138.300   Min.   :  0.000   Min.   :    0.000  
##  1st Qu.: 0.000   1st Qu.: -88.175   1st Qu.:  0.100   1st Qu.:    0.010  
##  Median : 0.100   Median :  -6.550   Median :  0.300   Median :    0.090  
##  Mean   : 0.766   Mean   :  -8.831   Mean   :  1.341   Mean   :  107.487  
##  3rd Qu.: 0.500   3rd Qu.:  14.125   3rd Qu.:  0.700   3rd Qu.:    0.475  
##  Max.   :16.200   Max.   : 173.500   Max.   :176.600   Max.   :31183.240  
##  NA's   :19216    NA's   :19216      NA's   :19216     NA's   :19216      
##   gyros_belt_x        gyros_belt_y       gyros_belt_z      accel_belt_x     
##  Min.   :-1.040000   Min.   :-0.64000   Min.   :-1.4600   Min.   :-120.000  
##  1st Qu.:-0.030000   1st Qu.: 0.00000   1st Qu.:-0.2000   1st Qu.: -21.000  
##  Median : 0.030000   Median : 0.02000   Median :-0.1000   Median : -15.000  
##  Mean   :-0.005592   Mean   : 0.03959   Mean   :-0.1305   Mean   :  -5.595  
##  3rd Qu.: 0.110000   3rd Qu.: 0.11000   3rd Qu.:-0.0200   3rd Qu.:  -5.000  
##  Max.   : 2.220000   Max.   : 0.64000   Max.   : 1.6200   Max.   :  85.000  
##                                                                             
##   accel_belt_y     accel_belt_z     magnet_belt_x   magnet_belt_y  
##  Min.   :-69.00   Min.   :-275.00   Min.   :-52.0   Min.   :354.0  
##  1st Qu.:  3.00   1st Qu.:-162.00   1st Qu.:  9.0   1st Qu.:581.0  
##  Median : 35.00   Median :-152.00   Median : 35.0   Median :601.0  
##  Mean   : 30.15   Mean   : -72.59   Mean   : 55.6   Mean   :593.7  
##  3rd Qu.: 61.00   3rd Qu.:  27.00   3rd Qu.: 59.0   3rd Qu.:610.0  
##  Max.   :164.00   Max.   : 105.00   Max.   :485.0   Max.   :673.0  
##                                                                    
##  magnet_belt_z       roll_arm         pitch_arm          yaw_arm         
##  Min.   :-623.0   Min.   :-180.00   Min.   :-88.800   Min.   :-180.0000  
##  1st Qu.:-375.0   1st Qu.: -31.77   1st Qu.:-25.900   1st Qu.: -43.1000  
##  Median :-320.0   Median :   0.00   Median :  0.000   Median :   0.0000  
##  Mean   :-345.5   Mean   :  17.83   Mean   : -4.612   Mean   :  -0.6188  
##  3rd Qu.:-306.0   3rd Qu.:  77.30   3rd Qu.: 11.200   3rd Qu.:  45.8750  
##  Max.   : 293.0   Max.   : 180.00   Max.   : 88.500   Max.   : 180.0000  
##                                                                          
##  total_accel_arm var_accel_arm     avg_roll_arm     stddev_roll_arm  
##  Min.   : 1.00   Min.   :  0.00   Min.   :-166.67   Min.   :  0.000  
##  1st Qu.:17.00   1st Qu.:  9.03   1st Qu.: -38.37   1st Qu.:  1.376  
##  Median :27.00   Median : 40.61   Median :   0.00   Median :  5.702  
##  Mean   :25.51   Mean   : 53.23   Mean   :  12.68   Mean   : 11.201  
##  3rd Qu.:33.00   3rd Qu.: 75.62   3rd Qu.:  76.33   3rd Qu.: 14.921  
##  Max.   :66.00   Max.   :331.70   Max.   : 163.33   Max.   :161.964  
##                  NA's   :19216    NA's   :19216     NA's   :19216    
##   var_roll_arm       avg_pitch_arm     stddev_pitch_arm var_pitch_arm     
##  Min.   :    0.000   Min.   :-81.773   Min.   : 0.000   Min.   :   0.000  
##  1st Qu.:    1.898   1st Qu.:-22.770   1st Qu.: 1.642   1st Qu.:   2.697  
##  Median :   32.517   Median :  0.000   Median : 8.133   Median :  66.146  
##  Mean   :  417.264   Mean   : -4.901   Mean   :10.383   Mean   : 195.864  
##  3rd Qu.:  222.647   3rd Qu.:  8.277   3rd Qu.:16.327   3rd Qu.: 266.576  
##  Max.   :26232.208   Max.   : 75.659   Max.   :43.412   Max.   :1884.565  
##  NA's   :19216       NA's   :19216     NA's   :19216    NA's   :19216     
##   avg_yaw_arm       stddev_yaw_arm     var_yaw_arm         gyros_arm_x      
##  Min.   :-173.440   Min.   :  0.000   Min.   :    0.000   Min.   :-6.37000  
##  1st Qu.: -29.198   1st Qu.:  2.577   1st Qu.:    6.642   1st Qu.:-1.33000  
##  Median :   0.000   Median : 16.682   Median :  278.309   Median : 0.08000  
##  Mean   :   2.359   Mean   : 22.270   Mean   : 1055.933   Mean   : 0.04277  
##  3rd Qu.:  38.185   3rd Qu.: 35.984   3rd Qu.: 1294.850   3rd Qu.: 1.57000  
##  Max.   : 152.000   Max.   :177.044   Max.   :31344.568   Max.   : 4.87000  
##  NA's   :19216      NA's   :19216     NA's   :19216                         
##   gyros_arm_y       gyros_arm_z       accel_arm_x       accel_arm_y    
##  Min.   :-3.4400   Min.   :-2.3300   Min.   :-404.00   Min.   :-318.0  
##  1st Qu.:-0.8000   1st Qu.:-0.0700   1st Qu.:-242.00   1st Qu.: -54.0  
##  Median :-0.2400   Median : 0.2300   Median : -44.00   Median :  14.0  
##  Mean   :-0.2571   Mean   : 0.2695   Mean   : -60.24   Mean   :  32.6  
##  3rd Qu.: 0.1400   3rd Qu.: 0.7200   3rd Qu.:  84.00   3rd Qu.: 139.0  
##  Max.   : 2.8400   Max.   : 3.0200   Max.   : 437.00   Max.   : 308.0  
##                                                                        
##   accel_arm_z       magnet_arm_x     magnet_arm_y     magnet_arm_z   
##  Min.   :-636.00   Min.   :-584.0   Min.   :-392.0   Min.   :-597.0  
##  1st Qu.:-143.00   1st Qu.:-300.0   1st Qu.:  -9.0   1st Qu.: 131.2  
##  Median : -47.00   Median : 289.0   Median : 202.0   Median : 444.0  
##  Mean   : -71.25   Mean   : 191.7   Mean   : 156.6   Mean   : 306.5  
##  3rd Qu.:  23.00   3rd Qu.: 637.0   3rd Qu.: 323.0   3rd Qu.: 545.0  
##  Max.   : 292.00   Max.   : 782.0   Max.   : 583.0   Max.   : 694.0  
##                                                                      
##  kurtosis_roll_arm  kurtosis_picth_arm kurtosis_yaw_arm   skewness_roll_arm 
##  Length:19622       Length:19622       Length:19622       Length:19622      
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##  skewness_pitch_arm skewness_yaw_arm    max_roll_arm     max_picth_arm     
##  Length:19622       Length:19622       Min.   :-73.100   Min.   :-173.000  
##  Class :character   Class :character   1st Qu.: -0.175   1st Qu.:  -1.975  
##  Mode  :character   Mode  :character   Median :  4.950   Median :  23.250  
##                                        Mean   : 11.236   Mean   :  35.751  
##                                        3rd Qu.: 26.775   3rd Qu.:  95.975  
##                                        Max.   : 85.500   Max.   : 180.000  
##                                        NA's   :19216     NA's   :19216     
##   max_yaw_arm     min_roll_arm    min_pitch_arm      min_yaw_arm   
##  Min.   : 4.00   Min.   :-89.10   Min.   :-180.00   Min.   : 1.00  
##  1st Qu.:29.00   1st Qu.:-41.98   1st Qu.: -72.62   1st Qu.: 8.00  
##  Median :34.00   Median :-22.45   Median : -33.85   Median :13.00  
##  Mean   :35.46   Mean   :-21.22   Mean   : -33.92   Mean   :14.66  
##  3rd Qu.:41.00   3rd Qu.:  0.00   3rd Qu.:   0.00   3rd Qu.:19.00  
##  Max.   :65.00   Max.   : 66.40   Max.   : 152.00   Max.   :38.00  
##  NA's   :19216   NA's   :19216    NA's   :19216     NA's   :19216  
##  amplitude_roll_arm amplitude_pitch_arm amplitude_yaw_arm roll_dumbbell    
##  Min.   :  0.000    Min.   :  0.000     Min.   : 0.00     Min.   :-153.71  
##  1st Qu.:  5.425    1st Qu.:  9.925     1st Qu.:13.00     1st Qu.: -18.49  
##  Median : 28.450    Median : 54.900     Median :22.00     Median :  48.17  
##  Mean   : 32.452    Mean   : 69.677     Mean   :20.79     Mean   :  23.84  
##  3rd Qu.: 50.960    3rd Qu.:115.175     3rd Qu.:28.75     3rd Qu.:  67.61  
##  Max.   :119.500    Max.   :360.000     Max.   :52.00     Max.   : 153.55  
##  NA's   :19216      NA's   :19216       NA's   :19216                      
##  pitch_dumbbell     yaw_dumbbell      kurtosis_roll_dumbbell
##  Min.   :-149.59   Min.   :-150.871   Length:19622          
##  1st Qu.: -40.89   1st Qu.: -77.644   Class :character      
##  Median : -20.96   Median :  -3.324   Mode  :character      
##  Mean   : -10.78   Mean   :   1.674                         
##  3rd Qu.:  17.50   3rd Qu.:  79.643                         
##  Max.   : 149.40   Max.   : 154.952                         
##                                                             
##  kurtosis_picth_dumbbell kurtosis_yaw_dumbbell skewness_roll_dumbbell
##  Length:19622            Length:19622          Length:19622          
##  Class :character        Class :character      Class :character      
##  Mode  :character        Mode  :character      Mode  :character      
##                                                                      
##                                                                      
##                                                                      
##                                                                      
##  skewness_pitch_dumbbell skewness_yaw_dumbbell max_roll_dumbbell
##  Length:19622            Length:19622          Min.   :-70.10   
##  Class :character        Class :character      1st Qu.:-27.15   
##  Mode  :character        Mode  :character      Median : 14.85   
##                                                Mean   : 13.76   
##                                                3rd Qu.: 50.58   
##                                                Max.   :137.00   
##                                                NA's   :19216    
##  max_picth_dumbbell max_yaw_dumbbell   min_roll_dumbbell min_pitch_dumbbell
##  Min.   :-112.90    Length:19622       Min.   :-149.60   Min.   :-147.00   
##  1st Qu.: -66.70    Class :character   1st Qu.: -59.67   1st Qu.: -91.80   
##  Median :  40.05    Mode  :character   Median : -43.55   Median : -66.15   
##  Mean   :  32.75                       Mean   : -41.24   Mean   : -33.18   
##  3rd Qu.: 133.22                       3rd Qu.: -25.20   3rd Qu.:  21.20   
##  Max.   : 155.00                       Max.   :  73.20   Max.   : 120.90   
##  NA's   :19216                         NA's   :19216     NA's   :19216     
##  min_yaw_dumbbell   amplitude_roll_dumbbell amplitude_pitch_dumbbell
##  Length:19622       Min.   :  0.00          Min.   :  0.00          
##  Class :character   1st Qu.: 14.97          1st Qu.: 17.06          
##  Mode  :character   Median : 35.05          Median : 41.73          
##                     Mean   : 55.00          Mean   : 65.93          
##                     3rd Qu.: 81.04          3rd Qu.: 99.55          
##                     Max.   :256.48          Max.   :273.59          
##                     NA's   :19216           NA's   :19216           
##  amplitude_yaw_dumbbell total_accel_dumbbell var_accel_dumbbell
##  Length:19622           Min.   : 0.00        Min.   :  0.000   
##  Class :character       1st Qu.: 4.00        1st Qu.:  0.378   
##  Mode  :character       Median :10.00        Median :  1.000   
##                         Mean   :13.72        Mean   :  4.388   
##                         3rd Qu.:19.00        3rd Qu.:  3.434   
##                         Max.   :58.00        Max.   :230.428   
##                                              NA's   :19216     
##  avg_roll_dumbbell stddev_roll_dumbbell var_roll_dumbbell  avg_pitch_dumbbell
##  Min.   :-128.96   Min.   :  0.000      Min.   :    0.00   Min.   :-70.73    
##  1st Qu.: -12.33   1st Qu.:  4.639      1st Qu.:   21.52   1st Qu.:-42.00    
##  Median :  48.23   Median : 12.204      Median :  148.95   Median :-19.91    
##  Mean   :  23.86   Mean   : 20.761      Mean   : 1020.27   Mean   :-12.33    
##  3rd Qu.:  64.37   3rd Qu.: 26.356      3rd Qu.:  694.65   3rd Qu.: 13.21    
##  Max.   : 125.99   Max.   :123.778      Max.   :15321.01   Max.   : 94.28    
##  NA's   :19216     NA's   :19216        NA's   :19216      NA's   :19216     
##  stddev_pitch_dumbbell var_pitch_dumbbell avg_yaw_dumbbell  
##  Min.   : 0.000        Min.   :   0.00    Min.   :-117.950  
##  1st Qu.: 3.482        1st Qu.:  12.12    1st Qu.: -76.696  
##  Median : 8.089        Median :  65.44    Median :  -4.505  
##  Mean   :13.147        Mean   : 350.31    Mean   :   0.202  
##  3rd Qu.:19.238        3rd Qu.: 370.11    3rd Qu.:  71.234  
##  Max.   :82.680        Max.   :6836.02    Max.   : 134.905  
##  NA's   :19216         NA's   :19216      NA's   :19216     
##  stddev_yaw_dumbbell var_yaw_dumbbell   gyros_dumbbell_x    gyros_dumbbell_y  
##  Min.   :  0.000     Min.   :    0.00   Min.   :-204.0000   Min.   :-2.10000  
##  1st Qu.:  3.885     1st Qu.:   15.09   1st Qu.:  -0.0300   1st Qu.:-0.14000  
##  Median : 10.264     Median :  105.35   Median :   0.1300   Median : 0.03000  
##  Mean   : 16.647     Mean   :  589.84   Mean   :   0.1611   Mean   : 0.04606  
##  3rd Qu.: 24.674     3rd Qu.:  608.79   3rd Qu.:   0.3500   3rd Qu.: 0.21000  
##  Max.   :107.088     Max.   :11467.91   Max.   :   2.2200   Max.   :52.00000  
##  NA's   :19216       NA's   :19216                                            
##  gyros_dumbbell_z  accel_dumbbell_x  accel_dumbbell_y  accel_dumbbell_z 
##  Min.   : -2.380   Min.   :-419.00   Min.   :-189.00   Min.   :-334.00  
##  1st Qu.: -0.310   1st Qu.: -50.00   1st Qu.:  -8.00   1st Qu.:-142.00  
##  Median : -0.130   Median :  -8.00   Median :  41.50   Median :  -1.00  
##  Mean   : -0.129   Mean   : -28.62   Mean   :  52.63   Mean   : -38.32  
##  3rd Qu.:  0.030   3rd Qu.:  11.00   3rd Qu.: 111.00   3rd Qu.:  38.00  
##  Max.   :317.000   Max.   : 235.00   Max.   : 315.00   Max.   : 318.00  
##                                                                         
##  magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z  roll_forearm      
##  Min.   :-643.0    Min.   :-3600     Min.   :-262.00   Min.   :-180.0000  
##  1st Qu.:-535.0    1st Qu.:  231     1st Qu.: -45.00   1st Qu.:  -0.7375  
##  Median :-479.0    Median :  311     Median :  13.00   Median :  21.7000  
##  Mean   :-328.5    Mean   :  221     Mean   :  46.05   Mean   :  33.8265  
##  3rd Qu.:-304.0    3rd Qu.:  390     3rd Qu.:  95.00   3rd Qu.: 140.0000  
##  Max.   : 592.0    Max.   :  633     Max.   : 452.00   Max.   : 180.0000  
##                                                                           
##  pitch_forearm     yaw_forearm      kurtosis_roll_forearm
##  Min.   :-72.50   Min.   :-180.00   Length:19622         
##  1st Qu.:  0.00   1st Qu.: -68.60   Class :character     
##  Median :  9.24   Median :   0.00   Mode  :character     
##  Mean   : 10.71   Mean   :  19.21                        
##  3rd Qu.: 28.40   3rd Qu.: 110.00                        
##  Max.   : 89.80   Max.   : 180.00                        
##                                                          
##  kurtosis_picth_forearm kurtosis_yaw_forearm skewness_roll_forearm
##  Length:19622           Length:19622         Length:19622         
##  Class :character       Class :character     Class :character     
##  Mode  :character       Mode  :character     Mode  :character     
##                                                                   
##                                                                   
##                                                                   
##                                                                   
##  skewness_pitch_forearm skewness_yaw_forearm max_roll_forearm max_picth_forearm
##  Length:19622           Length:19622         Min.   :-66.60   Min.   :-151.00  
##  Class :character       Class :character     1st Qu.:  0.00   1st Qu.:   0.00  
##  Mode  :character       Mode  :character     Median : 26.80   Median : 113.00  
##                                              Mean   : 24.49   Mean   :  81.49  
##                                              3rd Qu.: 45.95   3rd Qu.: 174.75  
##                                              Max.   : 89.80   Max.   : 180.00  
##                                              NA's   :19216    NA's   :19216    
##  max_yaw_forearm    min_roll_forearm  min_pitch_forearm min_yaw_forearm   
##  Length:19622       Min.   :-72.500   Min.   :-180.00   Length:19622      
##  Class :character   1st Qu.: -6.075   1st Qu.:-175.00   Class :character  
##  Mode  :character   Median :  0.000   Median : -61.00   Mode  :character  
##                     Mean   : -0.167   Mean   : -57.57                     
##                     3rd Qu.: 12.075   3rd Qu.:   0.00                     
##                     Max.   : 62.100   Max.   : 167.00                     
##                     NA's   :19216     NA's   :19216                       
##  amplitude_roll_forearm amplitude_pitch_forearm amplitude_yaw_forearm
##  Min.   :  0.000        Min.   :  0.0           Length:19622         
##  1st Qu.:  1.125        1st Qu.:  2.0           Class :character     
##  Median : 17.770        Median : 83.7           Mode  :character     
##  Mean   : 24.653        Mean   :139.1                                
##  3rd Qu.: 39.875        3rd Qu.:350.0                                
##  Max.   :126.000        Max.   :360.0                                
##  NA's   :19216          NA's   :19216                                
##  total_accel_forearm var_accel_forearm avg_roll_forearm   stddev_roll_forearm
##  Min.   :  0.00      Min.   :  0.000   Min.   :-177.234   Min.   :  0.000    
##  1st Qu.: 29.00      1st Qu.:  6.759   1st Qu.:  -0.909   1st Qu.:  0.428    
##  Median : 36.00      Median : 21.165   Median :  11.172   Median :  8.030    
##  Mean   : 34.72      Mean   : 33.502   Mean   :  33.165   Mean   : 41.986    
##  3rd Qu.: 41.00      3rd Qu.: 51.240   3rd Qu.: 107.132   3rd Qu.: 85.373    
##  Max.   :108.00      Max.   :172.606   Max.   : 177.256   Max.   :179.171    
##                      NA's   :19216     NA's   :19216      NA's   :19216      
##  var_roll_forearm   avg_pitch_forearm stddev_pitch_forearm var_pitch_forearm 
##  Min.   :    0.00   Min.   :-68.17    Min.   : 0.000       Min.   :   0.000  
##  1st Qu.:    0.18   1st Qu.:  0.00    1st Qu.: 0.336       1st Qu.:   0.113  
##  Median :   64.48   Median : 12.02    Median : 5.516       Median :  30.425  
##  Mean   : 5274.10   Mean   : 11.79    Mean   : 7.977       Mean   : 139.593  
##  3rd Qu.: 7289.08   3rd Qu.: 28.48    3rd Qu.:12.866       3rd Qu.: 165.532  
##  Max.   :32102.24   Max.   : 72.09    Max.   :47.745       Max.   :2279.617  
##  NA's   :19216      NA's   :19216     NA's   :19216        NA's   :19216     
##  avg_yaw_forearm   stddev_yaw_forearm var_yaw_forearm    gyros_forearm_x  
##  Min.   :-155.06   Min.   :  0.000    Min.   :    0.00   Min.   :-22.000  
##  1st Qu.: -26.26   1st Qu.:  0.524    1st Qu.:    0.27   1st Qu.: -0.220  
##  Median :   0.00   Median : 24.743    Median :  612.21   Median :  0.050  
##  Mean   :  18.00   Mean   : 44.854    Mean   : 4639.85   Mean   :  0.158  
##  3rd Qu.:  85.79   3rd Qu.: 85.817    3rd Qu.: 7368.41   3rd Qu.:  0.560  
##  Max.   : 169.24   Max.   :197.508    Max.   :39009.33   Max.   :  3.970  
##  NA's   :19216     NA's   :19216      NA's   :19216                       
##  gyros_forearm_y     gyros_forearm_z    accel_forearm_x   accel_forearm_y 
##  Min.   : -7.02000   Min.   : -8.0900   Min.   :-498.00   Min.   :-632.0  
##  1st Qu.: -1.46000   1st Qu.: -0.1800   1st Qu.:-178.00   1st Qu.:  57.0  
##  Median :  0.03000   Median :  0.0800   Median : -57.00   Median : 201.0  
##  Mean   :  0.07517   Mean   :  0.1512   Mean   : -61.65   Mean   : 163.7  
##  3rd Qu.:  1.62000   3rd Qu.:  0.4900   3rd Qu.:  76.00   3rd Qu.: 312.0  
##  Max.   :311.00000   Max.   :231.0000   Max.   : 477.00   Max.   : 923.0  
##                                                                           
##  accel_forearm_z   magnet_forearm_x  magnet_forearm_y magnet_forearm_z
##  Min.   :-446.00   Min.   :-1280.0   Min.   :-896.0   Min.   :-973.0  
##  1st Qu.:-182.00   1st Qu.: -616.0   1st Qu.:   2.0   1st Qu.: 191.0  
##  Median : -39.00   Median : -378.0   Median : 591.0   Median : 511.0  
##  Mean   : -55.29   Mean   : -312.6   Mean   : 380.1   Mean   : 393.6  
##  3rd Qu.:  26.00   3rd Qu.:  -73.0   3rd Qu.: 737.0   3rd Qu.: 653.0  
##  Max.   : 291.00   Max.   :  672.0   Max.   :1480.0   Max.   :1090.0  
##                                                                       
##     classe         
##  Length:19622      
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

- By examining the variables with NA values, we can see that they are mostly NA, and an imputation approach won't be possible. In the test set, the same variables were 100% NA.

- As such, using the following code, we can modify both sets to remove all the variables that have NA values, as well as subset all the variables that don't seem relevant to the analysis out of the data sets.

- We will also be converting the "classe" variable into a factor variable.


```r
pmlTrain[pmlTrain == ""] = NA
pmlTrain = pmlTrain[,!sapply(pmlTrain, anyNA)]
pmlTrainSub = subset(pmlTrain, select = -c(user_name,num_window, new_window, cvtd_timestamp, raw_timestamp_part_2, raw_timestamp_part_1, X))
pmlTrainSub$classe = factor(pmlTrainSub$classe)

pmlTest[pmlTest == ""] = NA
pmlTest = pmlTest[,!sapply(pmlTest, anyNA)]
pmlTestSub =subset(pmlTest, select = -c(num_window, new_window, cvtd_timestamp, raw_timestamp_part_2, raw_timestamp_part_1))
```

# Choosing predictors, and deciding on a model :

- Next up, we need to figure out which predictors and model we will be using.

- In order to do that, we can combine those steps, by using a random forest model with all the predictors selected and the "importance" parameter being set to true.

- We can also add a 10-k fold cross validation training control to the model to ensure we have done sufficient resampling, while maintaining a good balance of bias and variance.


```r
ctrl  <- trainControl(method  = "cv",number  = 10)

rfFit = train(classe~., method = "rf", data = pmlTrainSub, trControl = ctrl, importance = T)

varImp(rfFit, scale = F)
```

```
## rf variable importance
## 
##   variables are sorted by maximum importance across the classes
##   only 20 most important variables shown (out of 52)
## 
##                       A     B     C     D     E
## yaw_belt          37.93 40.21 31.63 39.45 25.97
## roll_belt         29.73 35.54 36.99 35.83 29.17
## pitch_belt        30.12 36.39 31.95 30.17 31.09
## magnet_dumbbell_z 32.42 31.68 35.24 29.02 29.55
## roll_arm          24.82 34.79 31.10 29.80 27.52
## magnet_dumbbell_y 26.75 29.03 34.28 28.73 26.37
## accel_belt_z      24.02 32.72 29.91 28.27 26.69
## accel_dumbbell_y  25.94 30.44 28.89 29.54 27.79
## pitch_forearm     25.04 30.40 29.28 29.40 28.56
## yaw_arm           22.61 29.91 28.12 29.02 25.36
## accel_dumbbell_z  25.64 28.17 27.97 27.05 29.58
## magnet_dumbbell_x 22.49 24.23 29.48 23.35 21.39
## gyros_forearm_y   20.69 29.38 27.14 28.10 23.49
## magnet_belt_z     27.18 29.33 28.27 29.03 26.68
## magnet_forearm_z  22.92 28.65 27.03 29.14 25.88
## gyros_belt_z      23.05 29.11 25.25 26.39 27.71
## gyros_dumbbell_z  23.25 29.09 26.62 26.96 24.96
## magnet_belt_y     23.52 28.73 28.23 27.40 25.36
## gyros_dumbbell_y  24.23 26.63 28.72 26.67 24.42
## accel_arm_y       24.41 28.43 24.94 24.09 24.93
```

```r
predict(rfFit,pmlTestSub)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

# Evaluating the results :

- The model proved to be suitable for the problem at hand, with an accuracy score and kappa of 0.99, and from the cross-validation, we can be confident that it's not biased or overfitted.

- The varImp function also showed that all the predictors were significant for the model.

- The predictions from the test set were then cross-referenced with accompanying quiz's results, and the results showed a 100% prediction accuracy.

# Session info :


```r
sessionInfo()
```

```
## R version 4.4.1 (2024-06-14)
## Platform: x86_64-pc-linux-gnu
## Running under: Ubuntu 22.04.4 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0 
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## time zone: Africa/Cairo
## tzcode source: system (glibc)
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] e1071_1.7-14   caret_6.0-94   lattice_0.22-6 ggplot2_3.5.1 
## 
## loaded via a namespace (and not attached):
##  [1] gtable_0.3.5         xfun_0.43            bslib_0.7.0         
##  [4] recipes_1.0.10       vctrs_0.6.5          tools_4.4.1         
##  [7] generics_0.1.3       stats4_4.4.1         parallel_4.4.1      
## [10] proxy_0.4-27         tibble_3.2.1         fansi_1.0.6         
## [13] pkgconfig_2.0.3      ModelMetrics_1.2.2.2 Matrix_1.7-0        
## [16] data.table_1.15.4    lifecycle_1.0.4      compiler_4.4.1      
## [19] stringr_1.5.1        munsell_0.5.1        codetools_0.2-20    
## [22] htmltools_0.5.8.1    class_7.3-22         sass_0.4.9          
## [25] yaml_2.3.8           prodlim_2023.08.28   pillar_1.9.0        
## [28] jquerylib_0.1.4      MASS_7.3-61          cachem_1.0.8        
## [31] gower_1.0.1          iterators_1.0.14     rpart_4.1.23        
## [34] foreach_1.5.2        nlme_3.1-165         parallelly_1.37.1   
## [37] lava_1.8.0           tidyselect_1.2.1     digest_0.6.35       
## [40] stringi_1.8.3        future_1.33.2        dplyr_1.1.4         
## [43] reshape2_1.4.4       purrr_1.0.2          listenv_0.9.1       
## [46] splines_4.4.1        fastmap_1.1.1        grid_4.4.1          
## [49] colorspace_2.1-0     cli_3.6.2            magrittr_2.0.3      
## [52] survival_3.6-4       utf8_1.2.4           future.apply_1.11.2 
## [55] withr_3.0.0          scales_1.3.0         lubridate_1.9.3     
## [58] timechange_0.3.0     rmarkdown_2.26       globals_0.16.3      
## [61] nnet_7.3-19          timeDate_4032.109    evaluate_0.23       
## [64] knitr_1.46           hardhat_1.4.0        rlang_1.1.4         
## [67] Rcpp_1.0.12          glue_1.7.0           pROC_1.18.5         
## [70] ipred_0.9-14         rstudioapi_0.16.0    jsonlite_1.8.8      
## [73] R6_2.5.1             plyr_1.8.9
```
