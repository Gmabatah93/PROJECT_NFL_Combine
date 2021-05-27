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
<img src="Images/NFL_Split.PNG" width=" 500">

**Split**

- **Train**: 80% stratified by drafted feature
- **Test**: 20% stratified by drafted feature
- **Validation**: 10 fold cross-validation using the training set

## Preprocess - Logistic Regression

#### NORMAL
- **Bin**: Infrequent position into "Other" category   
- **Normalize**: All numeric variables
- **Dummy**: All categorical variables

**Model Data**

<img src="Images/Model_Log_Normal.PNG" width=" 700">

#### PCA
**Eigen**

<img src="Images/Model_Log_PCA_Eig.PNG" width=" 700">

**Components**

<img src="Images/Model_Log_PCA_Components.PNG" width=" 700">

**Model Data**

<img src="Images/Model_Log_PCA.PNG" width=" 700">

### Simple
**Model Data**  
<img src="Images/Model_Log_Simple.PNG" width=" 700">

## Preprocess - Random Forrest

#### NORMAL

**Model Data**

<img src="Images/Model_RF_Normal.PNG" width=" 700">

#### PCA
**Model Data**

<img src="Images/Model_RF_PCA.PNG" width=" 700">

#### Simple
**Model Data**

<img src="Images/Model_RF_Simple.PNG" width=" 700">

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

**Grid**

<img src="Images/tune_log_grid.PNG" width="  600">

#### Latin Grid

**Accuracy**

<img src="Images/fit_Log_Latin_Acc.PNG" width="  600">

**F Score**

<img src="Images/fit_Log_Latin_F.PNG" width="  600">


Preprocess | Metric | Penalty | Mixture | Stat
--- | --- | --- | --- | ---
Normal | Accuracy | 0.00148 | 0.539 | 71%
PCA  | Accuracy | 7.22e-6 | 0.331 | 69.3%
Simple | Accuracy | 3.73e-5 | 0.735 | 69.3%
Normal | F Score | 0.00957 | 0.984 | 80.5%
PCA  | F Score | 0.0216 | 0.916 | 80.1%
Simple | F Score | 0.0216 | 0.916 | 79.8%

#### Custom Grid

**Accuracy**

<img src="Images/fit_Log_Custom_Acc.PNG" width="  600">

**F Score**

<img src="Images/fit_Log_Custom_F.PNG" width="  600">

Preprocess | Metric | Penalty | Mixture | Stat
--- | --- | --- | --- | ---
Normal | Accuracy | 0.001 | 1 | 71%
PCA  | Accuracy | 0.016 | 1 | 69%
Simple | Accuracy | 0.001 | 0 | 69.3%
Normal | F Score | 0.011 | 1 | 80.5%
PCA  | F Score | 0.041 | 0.5 | 80.2%
Simple | F Score | 0.016 | 1 | 80%


### Random Forest

**Grid**

<img src="Images/tune_rf_grid.PNG" width="  600">

#### Latin Grid

**Accuracy**

<img src="Images/fit_RF_Latin_Acc.PNG" width="  600">

**F Score**

<img src="Images/fit_RF_Latin_F.PNG" width="  600">

**Best**

Preprocess | Metric | mtry | min_n | Stat
--- | --- | --- | --- | ---
None | Accuracy | 7 | 37 | 70.5%
PCA  | Accuracy | 3 | 28 | 69.5%
Simple | Accuracy | 7 | 39 | 70.5%
None | F Score | 1 | 25 | 80.3%
PCA  | F Score | 1 | 19 | 80.1%
Simple | F Score | 1 | 32 | 80.3%

#### Custom Grid

**Accuracy**

<img src="Images/fit_RF_Custom_Acc.PNG" width="  600">

**F Score**

<img src="Images/fit_RF_Custom_F.PNG" width="  600">

Preprocess | Metric | mtry | min_n | Stat
--- | --- | --- | --- | ---
None | Accuracy | 8 | 7 | 70.3%
PCA  | Accuracy | 1 | 1 | 69.3%
Simple | Accuracy | 5 | 5 | 70.5%
None | F Score | 1 | 7 | 80.1%
PCA  | F Score | 1 | 1 | 80.1%
Simple | F Score | 1 | 9 | 80.1%

---

# Validation Diagnostic

## Logistic Regression

### Confusion Matrix
<img src="Images/CM_Log.PNG" width="  700">

### ROC Curve
<img src="Images/ROC_Log.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | F1 | PPV | NPV
--- | --- | --- | --- | --- | --- | --- | --- | ---
Accuracy | 0.732 | 0.735 | 0.917 | 0.39 | 0.74 | 0.819 | 0.74 | 0.713
F Score | 0.724 | 0.68 | 0.953 | 0.164 | 0.683 | 0.796 | 0.65 | 0.65

## Random Forrest

### Confusion Matrix
<img src="Images/CM_RF.PNG" width="  700">

### ROC Curve
<img src="Images/ROC_RF.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | F1 | PPV | NPV
--- | --- | --- | --- | --- | --- | --- | --- | ---
Accuracy | 0.722 | 0.704 | 0.87 | 0.39 | 0.73 | 0.794 | 0.73 | 0.614
F Score | 0.732 | 0.683 | 0.953 | 0.17 | 0.685 | 0.797 | 0.685 | 0.659

---
# Refit: Probability Threshold

## Logistic Regression
<img src="Images/Thres_Log.PNG" width="  700">
<img src="Images/CM_Log_Thres.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | F1 | PPV | NPV
--- | --- | --- | --- | --- | --- | --- | --- | ---
Accuracy (Thres 60%) | 0.732 | 0.722 | 0.777 | 0.616 | 0.793 | 0.785 | 0.793 | 0.594

## Random Forrest

<img src="Images/Thres_RF.PNG" width="  700">
<img src="Images/CM_RF_Thres.PNG" width="  700">

### Metrics
Optimization | AUC | Accuracy | Sensitivity | Specificity | Precision | F1 | PPV | NPV
--- | --- | --- | --- | --- | --- | --- | --- | ---
Accuracy (Thres 60%) | 0.722 | 0.696 | 0.734 | 0.623 | 0.786 | 0.759 | 0.786 | 0.553

---
# Final Diagnostic

## Logistic Regression

**Confusion Matrix**

<img src="Images/CM_Log_FINAL.PNG" width="  700">

**Metrics**

## Random Forrest

**Confusion Matrix**

<img src="Images/CM_RF_FINAL.PNG" width="  700">

**Metrics**
