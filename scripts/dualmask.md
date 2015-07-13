# Dual Mask




```r
df <- read.csv("./dualmask/dualmask-final.csv", stringsAsFactors = FALSE)
```

# Mean response time and error rate for each subject

<img src="figure/subj-means1.png" title="plot of chunk subj-means" alt="plot of chunk subj-means" style="display: block; margin: auto;" /><img src="figure/subj-means2.png" title="plot of chunk subj-means" alt="plot of chunk subj-means" style="display: block; margin: auto;" /><p class="caption" align="left"><strong>Figure</strong>: Detect outlier subjects using by subject mean response times and error rates.</p>


```r
df <- filter(df, subj_id != "MWP205a")  # slow response times and low accuracy
```

# Overall response time and error rate by condition


```
## Source: local data frame [6 x 4]
## Groups: cue_type
## 
##   cue_type mask_type    rt     err
## 1  invalid    nomask 537.2 0.03980
## 2    noise    nomask 500.4 0.03395
## 3    valid    nomask 481.3 0.02641
## 4  invalid      mask 513.3 0.03586
## 5    noise      mask 503.7 0.03188
## 6    valid      mask 483.1 0.04176
```

# Cueing effect without mask


```r
df_nomask <- filter(df, mask_type == "nomask")
```

```
##   cue_type cue_l   cue_q mask_type
## 1  invalid   0.5 -0.3333    nomask
## 2    noise   0.0  0.6667    nomask
## 3    valid  -0.5 -0.3333    nomask
```

```r
mod_nomask_rt <- lmerTest::lmer(rt ~ cue_l + cue_q + (1 | subj_id), data = df_nomask)
```

```
##         beta    se ci_lwr  ci_upr   df t_value p_value
## cue_l 56.118 6.366  43.64 68.5951 3681   8.815 0.00000
## cue_q -8.927 4.211 -17.18 -0.6744 3681  -2.120 0.03406
```

# Effect of mask on cueing effect


```
##   cue_type cue_l   cue_q mask_type mask_c
## 1  invalid   0.5 -0.3333    nomask   -0.5
## 2    noise   0.0  0.6667    nomask   -0.5
## 3    valid  -0.5 -0.3333    nomask   -0.5
## 4  invalid   0.5 -0.3333      mask    0.5
## 5    noise   0.0  0.6667      mask    0.5
## 6    valid  -0.5 -0.3333      mask    0.5
```

```r
mod_cueing_rt <- lmerTest::lmer(rt ~ (cue_l + cue_q) * mask_c + (1 | subj_id), data = df)
```

```
##                 beta    se  ci_lwr   ci_upr   df t_value  p_value
## cue_l         43.322 4.429  34.642 52.00138 7366  9.7825 0.000000
## cue_q         -1.777 2.928  -7.515  3.96068 7366 -0.6071 0.543829
## mask_c        -6.284 3.217 -12.588  0.02038 7366 -1.9536 0.050783
## cue_l:mask_c -25.534 8.857 -42.893 -8.17474 7366 -2.8829 0.003951
## cue_q:mask_c  14.294 5.855   2.818 25.76926 7366  2.4414 0.014656
```

# Effect of mask on baseline performance


```r
df_noise <- filter(df, cue_type == "noise")
```

```
##   cue_type mask_type mask_c
## 1    noise      mask    0.5
## 2    noise    nomask   -0.5
```

```r
mod_noise_rt <- lmerTest::lmer(rt ~ mask_c + (1 | subj_id), data = df_noise)
```

```
##         beta   se ci_lwr ci_upr   df t_value p_value
## mask_c 3.219 3.88 -4.386  10.82 3673  0.8296  0.4068
```

```r
mod_noise_err <- glmer(is_error ~ mask_c + (1 | subj_id), data = df_noise, family = binomial)
```

```
##         logodds     se ci_lwr ci_upr z_value p_value
## mask_c -0.06594 0.1796 -0.418 0.2862 -0.3671  0.7136
```

# Plot

<img src="figure/dualmask-plot1.png" title="plot of chunk dualmask-plot" alt="plot of chunk dualmask-plot" style="display: block; margin: auto;" /><img src="figure/dualmask-plot2.png" title="plot of chunk dualmask-plot" alt="plot of chunk dualmask-plot" style="display: block; margin: auto;" /><p class="caption" align="left"><strong>Figure</strong>: Results of Experiment 1A.</p>
