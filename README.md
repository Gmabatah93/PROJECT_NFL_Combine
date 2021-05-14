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
broad_jump | dbl | how far can a player jump. From a balanced stance, the player explodes forward as far as he can and must land without moving _(cm)_
three_cone | dbl | primarily run to evaluate the agility, quickness and fluidity of movement _(seconds)_
shuttle | dbl | The shuttle (20yards), much like the 3-cone drill, test speed and acceleration. The only difference is that players are running laterally instead of forming a right angle.  _(seconds)_
drafted | chr | What NFL team drafted player / Round / Pick / Year

# Exploratory Data Analysis

## Summary
<p float="left">
  <img src="Images/EDA_Drafted.PNG" width="400" />
  <img src="Images/EDA_round.PNG" width="400" />
</p>

**Note**:

<p float="left">
  <img src="Images/EDA_Side.PNG" width="400" />
  <img src="Images/EDA_round_Side.PNG" width="400" />
</p>

**Note**:

<p float="left">
  <img src="Images/EDA_Position.PNG" width="400" />
  <img src="Images/EDA_round_Position.PNG" width="400" />
</p>

**Note**:

<p float="left">
  <img src="Images/EDA_Conference.PNG" width="400" />
  <img src="Images/EDA_round_Conference.PNG" width="400" />
</p>

**Note**:

## Correlation
<img src="Images/Corr_Combine.PNG" width="400">

**Note**:

<p float="left">
  <img src="Images/Corr_Sig.PNG" width="700" />
  <img src="Images/Corr_round_Sig.PNG" width="700" />
</p>

**Note**:

## Principal Component Analysis
<img src="Images/PCA_Biplot.PNG" width="700">

**Note**:

<img src="Images/PCA_Biplot_drafted_round.PNG" width="700">
<img src="Images/PCA_Biplot_Side.PNG" width="700">

**Note**:

<p float="left">
  <img src="Images/PCA_PC1.PNG" width="400" />
  <img src="Images/PCA_PC2.PNG" width="400" />
</p>

## Combine Summary

### Weight
<img src="Images/Combine_Weight.PNG" width="700">

**Note:**

<img src="Images/Combine_Weight_draft_round.PNG" width="  1000">

**Note:**

<img src="Images/Combine_Weight_conference.PNG" width="  1000">

**Note:**

### Forty
<img src="Images/Combine_40.PNG" width="  1000">

**Note:**

<img src="Images/Combine_40_Position.PNG" width="  1000">

**Note:**

<img src="Images/Combine_40_Conference.PNG" width="  1000">

**Note:**

### Bench
<img src="Images/Combine_Bench.PNG" width="  700">

**Note:**

<img src="Images/Combine_Bench_Position.PNG" width="  1000">

**Note:**

<img src="Images/Combine_Bench_Conference.PNG" width="  1000">

**Note:**

## Offense Summary
<img src="Images/Offense_Conference.PNG" width="  1000">

**Note:**

<img src="Images/Offense_Position.PNG" width="  1000">

**Note:**

## Defense Summary
<img src="Images/Defense_Conference.PNG" width="  1000">

**Note:**

<img src="Images/Defense_Position.PNG" width="  1000">

**Note:**
