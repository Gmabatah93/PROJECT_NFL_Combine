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
<img src="Images/draft.PNG" width="700">

**Note (Target)**: **_65%_** of players in this dataset was Drafted

<img src="Images/draft_Side.PNG" width="700">

<img src="Images/draft_Position.PNG" width="700">

**Note (Position)**: There are _1459_ Defensive and _1426_ Offensive players in this dataset
- Of the _1459_ Defensive players **70%** was Drafted. Of the _1426_ Offensive players **61%** was Drafted.
- Top 3 Offensive Positions Drafted: **_OT | WR | RB_**
- Top 3 Defensive Positions Drafted: **_CB | DE | OLB_**

<img src="Images/draft_Conference.PNG" width="700" />

**Note (Conference)**: The majority of players Drafted came from the **_[ SEC | ACC | Big 10 ]_** Conferences

## Correlation
<img src="Images/Combine_Corr.PNG" width="700">

<img src="Images/Combine_sig.PNG" width="700">

**Note (Correlation)**: Alot of the Combine metrics are **_Highly_** Corelated

## Principal Component Analysis
<p float="left">
  <img src="Images/Combine_PCA.PNG" width="500">
  <img src="Images/Combine_PCA_draft.PNG" width="500">
</p>

<img src="Images/Combine_PCA_Eig.PNG" width="600">

**Note (PCA)**:
- **PC1**: Represents Agility/Explosiveness
  + Agility: forty | three_cone | shuttle
    + generally if your fast in one your fast in the others
  + Explosiveness: vertical | broad_jump
    + generally if you can jump well vertically you can jump well horizontally
- **PC2**: Represents overall strength


## Combine Summary

### Weight
<img src="Images/Combine_Weight.PNG" width="700">
<img src="Images/Combine_Weight_Position_draft.PNG" width="  700">
<img src="Images/Combine_Weight_Conference_draft.PNG" width="  700">

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
<img src="Images/Combine_40.PNG" width="  700">
<img src="Images/Combine_40_Position.PNG" width="  700">
<img src="Images/Combine_40_Conference.PNG" width="  700">

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
<img src="Images/Combine_Bench.PNG" width="  700">
<img src="Images/Combine_Bench_Position.PNG" width="  700">
<img src="Images/Combine_Bench_Conference.PNG" width="  700">

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
<img src="Images/Combine_Jump.PNG" width="  700">
<img src="Images/Combine_Jump_Position.PNG" width="  700">
<img src="Images/Combine_Jump_Conference.PNG" width="  700">

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

### Split
- **Train**: 80% stratified by drafted feature
- **Test**: 20% stratified by drafted feature
- **Validation**: 10 fold cross-validation using the training set

### Logistic Regression
- **Bin**: Infrequent position into "Other" category   
- **Normalize**: All numeric variables
- **Dummy**: All categorical variables

#### Model Data
<img src="Images/Model_Log.PNG" width="  700">

### Random Forrest
_no preprocess_

**Model Data**
<img src="Images/Model_RF.PNG" width="  700">

#### PCA
**Eigen**
<img src="Images/Model_RF_PCA_Eig.PNG" width="  700">

**Components**
<img src="Images/Model_RF_PCA_components.PNG" width="  700">

**Model Data**
<img src="Images/Model_RF_PCA_2.PNG" width="  700">
<img src="Images/Model_RF_PCA_4.PNG" width="  700">
### Control
- **Parallel Processing**

### Metrics
 - [AUC | Sensitivity | Specificity | Precision | F1]

## Fit

### Logistic Regression

#### Grid
<img src="Images/tune_log_grid.PNG" width="  600">

**Accuracy**

<img src="Images/tune_log_fit_Acc.PNG" width="  600">


Grid | Metric | Penalty | Mixture | Stat
--- | --- | --- | --- | ---
Random | Accuracy | -4.1e4 | 0.851 | 0.71
Latin  | Accuracy | -1.4e8 | 0.516 | 0.71
Custom | Accuracy | 0.006 | 1 | 0.7

#### Sensitivity

<img src="Images/tune_log_fit_Sens.PNG" width="  600">

**Best**

Grid | Metric | Penalty | Mixture | Stat
--- | --- | --- | --- | ---
Random | Sensitivity | 0.136 | 0.955 | 1
Latin  | Sensitivity | 0.712 | 0.854 | 1
Custom | Sensitivity | 0.096 | 0.5 | 1

#### Precision

<img src="Images/tune_log_fit_Prec.PNG" width="  600">

**Best**

Grid | Metric | Penalty | Mixture | Stat
--- | --- | --- | --- | ---
Random | Precision | -4.28e9 | 0.98 | 0.73
Latin  | Precision | -6.54e7 | 0.572 | 0.73
Custom | Precision | 0.001 | 1 | 0.73

#### F Score

<img src="Images/tune_log_fit_F.PNG" width="  600">

**Best**

Grid | Metric | Penalty | Mixture | Stat
--- | --- | --- | --- | ---
Random | F Score | -3.92e4 | 0.961 | 0.8
Latin  | F Score | .00957 | 0.984 | 0.8
Custom | F Score | 0.016 | 1 | 0.8

### Random Forest

#### Grid
<img src="Images/tune_rf_grid.PNG" width="  600">

#### Fit - Latin

<img src="Images/tune_rf_latin_Acc.PNG" width="  600">
<img src="Images/tune_rf_latin_F.PNG" width="  600">

**Best**

Preprocess | Metric | mtry | min_n | Stat
--- | --- | --- | --- | ---
None | Accuracy | 3 | 28 | 70.7%
PC 2  | Accuracy | 2 | 35 | 69.0%
PC 4 | Accuracy | 1 | 32 | 68.9%
None | F Score | 1 | 33 | 80.4%
PC 2  | F Score | 1 | 25 | 79.7%
PC 4 | F Score | 1 | 32 | 79.9%

#### Fit - Custom

<img src="Images/tune_rf_custom_Acc.PNG" width="  600">
<img src="Images/tune_rf_custom_F.PNG" width="  600">

Preprocess | Metric | mtry | min_n | Stat
--- | --- | --- | --- | ---
None | Accuracy | 5 | 9 | 70.6%
PC 2  | Accuracy | 1 | 7 | 69.0%
PC 4 | Accuracy | 2 | 9 | 69.0%
None | F Score | 1 | 9 | 80.2%
PC 2  | F Score | 1 | 7 | 79.8%
PC 4 | F Score | 1 | 7 | 79.7%

---

# Diagnostic

## Logistic Regression

### Confusion Matrix
<img src="Images/CM_Log.PNG" width="  700">

### ROC Curve
<img src="Images/ROC_Log.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | F1 | PPV | NPV
--- | --- | --- | --- | --- | --- | --- | --- | ---
Accuracy | 0.698 | 0.714 | 0.865 | 0.427 | 0.741 | 0.798 | 0.741 | 0.625
Sensitivity | 0.639 | 0.655 | 1 | 0 | 0.655 | 0.791 | 0.655 | NaN
Precision | 0.698 | 0.714 | 0.865 | 0.427 | 0.741 | 0.798 | 0.741 | 0.625
F Score | 0.698 | 0.696 | 0.912 | 0.288 | 0.708 | 0.797 | 0.708 | 0.633

## Random Forrest

### Confusion Matrix
<img src="Images/CM_RF.PNG" width="  700">

### ROC Curve
<img src="Images/ROC_RF.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | F1 | PPV | NPV
--- | --- | --- | --- | --- | --- | --- | --- | ---
Accuracy | 0.673 | 0.681 | 0.836 | 0.387 | 0.721 | 0.774 | 0.721 | 0.554
Sensitivity | 0.674 | 0.684 | 0.857 | 0.357 | 0.716 | 0.78 | 0.716 | 0.568
Precision | 0.675 | 0.672 | 0.814 | 0.402 | 0.721 | 0.765 | 0.721 | 0.533
F Score | 0.672 | 0.686 | 0.859 | 0.357 | 0.717 | 0.782 | 0.717 | 0.573
