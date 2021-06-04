### Table of Contents

---
# Project Overview

## Summary

### Code Used

Package | version
--- | ---
tidymodels | 0.1.2
stringr | 1.4.0
forcats | 0.5.0
RColorBrewer | 1.1-2
ggmosaic | 0.3.4
ggpubr | 0.4.0
FactoMiner | 2.4
factoextra | 1.0.7
probably | 0.0.6
workflowsets | 0.0.1
doParallel | 1.0.16

### Business Requirement
1. Develop a Model to predict if a player will be drafted
2. For the players that got drafted develop a Model to predict if they will be drafted in the 1st Round.

### Data Collection
**(raw)**

Feature | Datatype | Description
--- | --- | ---
player | chr | name of the player
position | chr | position player played in college
school | chr | where player played in college
year | dbl | year player entered the draft
height | dbl | height of player _(cm)_
weight | dbl | weight of player _(lbs)_
forty | dbl | how fast player runs 40yards _(seconds)_
vertical | dbl | how high player jumps from a still position _(inches)_
bench | dbl | how many times player can lift 225lbs
broad_jump | dbl | how far can a player jump. From a balanced stance, the player explodes forward as far as he can and must land without moving _(inches)_
three_cone | dbl | primarily run to evaluate the agility, quickness and fluidity of movement _(seconds)_
shuttle | dbl | The shuttle (20yards), much like the 3-cone drill, test speed and acceleration. The only difference is that players are running laterally instead of forming a right angle.  _(seconds)_
drafted | chr | What NFL team drafted player / Round / Pick / Year

---

# Exploratory Data Analysis

## Summary
<img src="Images/EDA_drafted.PNG" width="700">

**Note (Target)**: Of all the players in this data **_65%_** of players in this dataset was Drafted

<img src="Images/EDA_draft_side.PNG" width="700">

<img src="Images/EDA_draft_position.PNG" width="700">

**Note (Position)**: There are _1459_ Defensive and _1426_ Offensive players in this dataset
- Of the _1459_ Defensive players **70%** was Drafted. Of the _1426_ Offensive players **61%** was Drafted.
- Top 3 Offensive Positions Drafted: **_OT | WR | RB_**
- Top 3 Defensive Positions Drafted: **_CB | DE | OLB_**

<img src="Images/EDA_draft_conference.PNG" width="700" />

**Note (Conference)**: The majority of players Drafted came from the **_[ SEC | ACC | Big 10 ]_** Conferences

## Correlation
<img src="Images/EDA_corr.PNG" width="700">

<img src="Images/EDA_corr_sig.PNG" width="700">

**Note (Correlation)**: Alot of the Combine metrics are **_Highly_** Corelated

## Principal Component Analysis
<p float="left">
  <img src="Images/PCA_biplot.PNG" width="500">
  <img src="Images/PCA_biplot_drafted.PNG" width="500">
</p>

<img src="Images/PCA_eig.PNG" width="700">
<img src="Images/PCA_pc.PNG" width="700">
**Note (PCA)**:
- **PC1**: Represents Agility/Explosiveness
  + Agility: forty | three_cone | shuttle
    + generally if your fast in one your fast in the others
  + Explosiveness: vertical | broad_jump
    + generally if you can jump well vertically you can jump well horizontally
- **PC2**: Represents overall strength


## Combine Summary

### Weight
<img src="Images/COMBINE_weight.PNG" width="700">
<img src="Images/COMBINE_weight_side.PNG" width="  700">
<img src="Images/COMBINE_weight_conference.PNG" width="  700">

**Note (Weight)**: Average weight for players in this dataset is **_252lbs_**
- Offense: **_262lbs_**
  + Average weight for **OT** is **_315lbs_** for both drafted and non drafted players
  + Average weight for **WR** is **_204lbs_** for both drafted and non drafted players
  + Average weight for **RB** is **_215lbs_** for both drafted and non drafted players
- Defense: **_242lbs_**
  + Average weight for **CB** is **_193lbs_** for both drafted and non drafted players
  + Average weight for **DE** is **_268lbs_** for both drafted and non drafted players
  + Average weight for **OLB** is around **_240lbs_** for both drafted and non drafted players
- Conference: Averages
  + Elite = **_253lbs_** | Division I-A = **_246lbs_** | Division I-AA = **_252_** | Division II & II = **_266lbs_**

### Forty
<img src="Images/COMBINE_40.PNG" width="  700">
<img src="Images/COMBINE_40_side.PNG" width="  700">
<img src="Images/COMBINE_40_conference.PNG" width="  700">

**Note (Forty):** Average forty for players in this dataset is **_4.81secs_**
- Offense: **_4.72secs_**
  + Average forty for **OT**: Drafted = **_5.21_** | Not Drafted = **_5.34_**
  + Average forty for **WR**: Drafted = **_4.47_** | Not Drafted = **_4.55_**
  + Average forty for **RB**: Drafted = **_4.52_** | Not Drafted = **_4.61_**
- Defense: **_4.90secs_**
  + Average forty for **CB**: Drafted = **_4.47_** | Not Drafted = **_4.54_**
  + Average forty for **DE**: Drafted = **_4.80_** | Not Drafted = **_4.89_**
  + Average forty for **OLB**: Drafted = **_4.65_** | Not Drafted = **_4.74_**
- Conference: Averages
  + Elite = **_4.81secs_** | Division I-A = **_4.78secs_** | Division I-AA = **_4.82secs_** | Division II & II = **_4.89secs_**

### Bench
<img src="Images/COMBINE_bench.PNG" width="  700">
<img src="Images/COMBINE_bench_side.PNG" width="  700">
<img src="Images/COMBINE_Bench_conference.PNG" width="  700">

**Note (Bench):** Average bench for players in this dataset is **_21reps_**
- Offense: **_21reps_**
  + Average bench for **OT**: Drafted = **_25reps_** | Not Drafted = **_23reps_**
  + Average bench for **WR** was **_14reps_** for both drafted and non drafted players
  + Average bench for **RB**: Drafted = **_20reps_** | Not Drafted = **_19reps_**
- Defense: **_21reps_**
  + Average bench for **CB**: Drafted = **_15reps_** | Not Drafted = **_13reps_**
  + Average bench for **DE**: Drafted = **_24reps_** | Not Drafted = **_23reps_**
  + Average bench for **OLB**: Drafted = **_23reps_** | Not Drafted = **_21reps_**
- Conference: Averages
  + Elite = **_21reps_** | Division I-A = **_21reps_** | Division I-AA = **_21reps_** | Division II & II = **_23reps_**

### Broad Jump
<img src="Images/COMBINE_jump.PNG" width="  700">
<img src="Images/COMBINE_jump_side.PNG" width="  700">
<img src="Images/COMBINE_jump_conference.PNG" width="  700">

**Note (Jump):** Average broad jump for players in this dataset is **_113inches**
- Offense: **_110inches_**
  + Average broad jump for **OT**: Drafted = **_103inches_** | Not Drafted = **_100inches_**
  + Average broad jump for **WR**: Drafted = **_121inches_** | Not Drafted = **_119inches_**
  + Average broad jump for **RB**: Drafted = **_119inches_** | Not Drafted = **_117inches_**
- Defense: **_116inches_**
  + Average broad jump for **CB**: Drafted = **_122inches_** | Not Drafted = **_120inches_**
  + Average broad jump for **DE**: Drafted = **_115inches_** | Not Drafted = **_113inches_**
  + Average broad jump for **OLB**: Drafted = **_118inches_** | Not Drafted = **_115inches_**
- Conference: Averages
  + Elite = **_113inches_** | Division I-A = **_114inches_** | Division I-AA = **_113inches_** | Division II & II = **_112inches_**

---

# Modeling

## Preprocess
<img src="Images/NFL_Split.PNG" width=" 500">

**Split**

- **Train**: 80% stratified by drafted feature
- **Test**: 20% stratified by drafted feature
- **Validation**: 10 fold cross-validation using the training set

## Preprocess - Logistic Regression

#### NORMAL
- **Normalize**: All numeric variables
- **Dummy**: All categorical variables

**Model Data**

<img src="Images/PREPROCESS_LOG_normal.PNG" width=" 700">

#### PCA

**Components**

<img src="Images/PREPROCESS_LOG_pca_pc.PNG" width=" 700">

**Model Data**

<img src="Images/PREPROCESS_LOG_pca.PNG" width=" 700">

### Simple

**Model Data**  

<img src="Images/PREPROCESS_LOG_simple.PNG" width=" 700">


## Preprocess - Random Forrest

#### NORMAL

**Model Data**

<img src="Images/PREPROCESS_RF_none.PNG" width=" 700">

#### PCA

**Model Data**

<img src="Images/PREPROCESS_RF_pca.PNG" width=" 700">

#### Simple

**Model Data**

<img src="Images/PREPROCESS_RF_simple.PNG" width=" 700">

### Control
- **Parallel Processing**

### Metrics
 - **AUC:** Measure of performance across all possible class
 - **Accuracy:** What percentage did the model correctly predicted who got drafted and who didn't
 - **Sensitivity:** Out of all the players that got actually got drafted what percentage did the model predict correctly
 - **Specificity:** Out of all the players that got did not get drafted what percentage did the model predict correctly
 - **Precision:** Out of all the players the Model predicted got Drafted what percentage actually got Drafted ?
 - **F1:** Balance between Precision and Recall "Sensitivity"

## Fit

### Logistic Regression

<img src="Images/GRID_log.PNG" width="  600">
<img src="Images/FIT_LOG_acc.PNG" width="  600">
<img src="Images/FIT_LOG_f.PNG" width="  600">

### **Best Metrics**

Preprocess | Metric | Penalty | Mixture | Stat
--- | --- | --- | --- | ---
Normal | Accuracy | 0.006 | 0.5 | 71.5%
PCA  |  Accuracy | 0.001 | 0.5 | 69.1%
Simple |  Accuracy | 0.001 | 0.5 | 71.1%
Normal | F Score | 0.006 | 1 | 71.5%
PCA  |  F Score | 0.036 | 0.5 | 80%
Simple |  F Score | 0.006 | 1 | 80.5%


### Random Forest

**Grid**

<img src="Images/TUNE_RF_grid.PNG" width="  600">
<img src="Images/FIT_RF_acc.PNG" width="  600">
<img src="Images/FIT_RF_f.PNG" width="  600">

#### **Best Metrics**

Preprocess | Metric | mtry | min_n | Stat
--- | --- | --- | --- | ---
None | Accuracy | 5 | 7 | 70%
PCA  | Accuracy | 1 | 5 | 67%
Simple | Accuracy | 2 | 9 | 70.4%
None | F Score | 1 | 5 | 79.7%
PCA  | F Score | 1 | 5 | 78.5%
Simple | F Score | 1 | 9 | 79.6%

---

# Validation Diagnostic

## Logistic Regression

### ROC Curve
<img src="Images/ROC_Log.PNG" width="  700">

### Confusion Matrix
<img src="Images/CM_Log.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | Recall | F1
--- | --- | --- | --- | --- | --- | --- | ---
Accuracy (P = 0.001, M = 0.5) | 0.695 | 0.707 | 0.88 | 0.377 | 0.728 | 0.88 | 0.797
F Score (P = 0.006, M = 1) | 0.697 | 0.707 | 0.914 | 0.314 | 0.716 | 0.914 | 0.803

## Random Forrest

### ROC Curve
<img src="Images/ROC_RF.PNG" width="  700">

### Confusion Matrix
<img src="Images/CM_RF.PNG" width="  700">

### Metrics

Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | Recall | F1
--- | --- | --- | --- | --- | --- | --- | ---
Accuracy (mtry = 2, min = 9) | 0.705 | 0.696 | 0.834 | 0.434 | 0.736 | 0.834 | 0.782
F Score (mtry = 1, min = 9) | 0.715 | 0.698 | 0.884 | 0.346 | 0.719 | 0.884 | 0.793

---
# Refit: Probability Threshold

## Logistic Regression
<img src="Images/ThRES_LOG.PNG" width="  700">
<img src="Images/CM_Log_thres.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | Recall| F1
--- | --- | --- | --- | --- | --- | --- | ---
Acc-60% (P = 0.001, M = 0.5) | 0.695 | 0.667 | 0.738 | 0.535 | 0.75 | 0.738 | 0.744

## Random Forrest

<img src="Images/ThRES_RF.PNG" width="  700">
<img src="Images/CM_RF_thres.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | Recall | F1
--- | --- | --- | --- | --- | --- | --- | ---
Acc-60%  (mtry = 2, min = 9) | 0.705 | 0.659 | 0.684 | 0.61 | 0.769 | 0.684 | 0.724

---
# Test Results

## ROC Curve
<img src="Images/ROC_FINAL.PNG" width="  700">

## Confusion Matrix
**Logistic Regression**

<img src="Images/CM_Log_final.PNG" width="  700">


**Random Forrest**

<img src="Images/CM_RF_final.PNG" width="  700">


## Metrics  

Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | Recall | F1
--- | --- | --- | --- | --- | --- | --- | ---
LOG Acc-50% (p=0.001, m=0.5) | 0.729 | 0.698 | 0.878 | 0.357 | 0.721 | 0.878 | 0.792
LOG Acc-70% (p=0.001, m=0.5) | 0.729 | 0.682 | 0.764 | 0.528 | 0.754 | 0.764 | 0.759  
LOG F (p=0.006, m=1) | 0.727 | 0.675 |**_0.899_** | 0.251 | 0.695 | 0.899 | 0.784
RF Acc-50% (mtry=2, min=9) | **_0.746_** | **_0.72_** | 0.867 | 0.442 | 0.747 | 0.867 | **_0.802_**
RF Acc-60% (mtry=2, min=9) | 0.746 | 0.715 | 0.769 | 0.613 | **_0.79_** | 0.769 | 0.78
RF F (mtry=1, min=9) | 0.742 | 0.707 | 0.886 | 0.367 | 0.726 | 0.886 | 0.798

**Top (AUC):** Random Forrest _(mtry = 2, min = 9)_ - **74.6%**

**Top (Accuracy):** Random Forrest _(mtry = 2, min = 9)_ - **72%**

**Top (Sensitivity):** Logistic Regression _(penalty = 0.006, mix = 1)_ - **89.9%**

**Top (Precision):** Random Forrest(60%) _(mtry = 2, min = 9)_ - **79%**

**Top (F Score):** Random Forrest _(mtry = 2, min = 9)_ - **80.2%**
