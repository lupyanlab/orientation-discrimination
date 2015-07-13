# Dual Mask - Temporal




```r
df <- read.csv("./temporal/temporal-final.csv", stringsAsFactors = FALSE)
df <- filter(df, subj_id != "MWP502")  # incomplete data
```

# Descriptive statistics

<img src="figure/subj-means1.png" title="plot of chunk subj-means" alt="plot of chunk subj-means" style="display: block; margin: auto;" /><img src="figure/subj-means2.png" title="plot of chunk subj-means" alt="plot of chunk subj-means" style="display: block; margin: auto;" /><p class="caption" align="left"><strong>Figure</strong>: Detect outlier subjects using by subject mean response times and error rates.</p>


```r
df <- filter(df, subj_id %nin% c("MWP524", "MWP518"))  # remove subjs with high error rates
```

# Overall response time and error rate by condition


```
## Source: local data frame [9 x 4]
## Groups: cue_type
## 
##   cue_type mask_type    rt     err
## 1  invalid    nomask 520.6 0.03008
## 2    noise    nomask 481.5 0.03840
## 3    valid    nomask 466.2 0.03438
## 4  invalid    during 504.0 0.04635
## 5    noise    during 475.5 0.02802
## 6    valid    during 465.4 0.02571
## 7  invalid     after 511.0 0.03143
## 8    noise     after 491.9 0.04152
## 9    valid     after 475.1 0.02575
```

# Effect of mask **during** auditory cue on cueing effect


```r
df_during <- filter(df, mask_type %in% c("nomask", "during"))
```

```
##   cue_type cue_l   cue_q mask_type mask_c
## 1  invalid   0.5 -0.3333    nomask   -0.5
## 2    noise   0.0  0.6667    nomask   -0.5
## 3    valid  -0.5 -0.3333    nomask   -0.5
## 4  invalid   0.5 -0.3333    during    0.5
## 5    noise   0.0  0.6667    during    0.5
## 6    valid  -0.5 -0.3333    during    0.5
```

```r
mod_during_rt <- lmerTest::lmer(rt ~ (cue_l + cue_q) * mask_c + (1 | subj_id), data = df_during)
```

```
##                 beta    se  ci_lwr ci_upr   df t_value   p_value
## cue_l         46.564 4.242  38.250 54.878 8054 10.9774 0.0000000
## cue_q        -10.552 2.802 -16.044 -5.060 8054 -3.7658 0.0001672
## mask_c        -7.747 3.080 -13.785 -1.710 8054 -2.5152 0.0119153
## cue_l:mask_c -15.617 8.484 -32.245  1.011 8054 -1.8407 0.0656968
## cue_q:mask_c   2.610 5.604  -8.374 13.594 8054  0.4658 0.6413955
```

# Effect of mask **after** auditory cue on cueing effect


```r
df_after <- filter(df, mask_type %in% c("nomask", "after"))
```

```
##   cue_type cue_l   cue_q mask_type mask_c
## 1  invalid   0.5 -0.3333    nomask   -0.5
## 2    noise   0.0  0.6667    nomask   -0.5
## 3    valid  -0.5 -0.3333    nomask   -0.5
## 4  invalid   0.5 -0.3333     after    0.5
## 5    noise   0.0  0.6667     after    0.5
## 6    valid  -0.5 -0.3333     after    0.5
```

```r
mod_after_rt <- lmerTest::lmer(rt ~ (cue_l + cue_q) * mask_c + (1 | subj_id), data = df_after)
```

```
##                 beta    se  ci_lwr  ci_upr   df t_value p_value
## cue_l         44.996 4.251  36.665 53.3272 8045  10.586 0.00000
## cue_q         -6.473 2.819 -11.997 -0.9489 8045  -2.297 0.02167
## mask_c         3.142 3.091  -2.917  9.1997 8045   1.016 0.30948
## cue_l:mask_c -18.737 8.501 -35.399 -2.0754 8045  -2.204 0.02755
## cue_q:mask_c  10.744 5.637  -0.304 21.7930 8045   1.906 0.05668
```

# Plot

<img src="figure/temporal-plot1.png" title="plot of chunk temporal-plot" alt="plot of chunk temporal-plot" style="display: block; margin: auto;" /><img src="figure/temporal-plot2.png" title="plot of chunk temporal-plot" alt="plot of chunk temporal-plot" style="display: block; margin: auto;" /><p class="caption" align="left"><strong>Figure</strong>: Results of Exp. 1B</p>
