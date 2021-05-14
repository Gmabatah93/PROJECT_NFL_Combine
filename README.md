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
<img src="Images/draft.PNG" width="400">

**Note (Target)**: **_65%_** of players in this dataset was **_Drafted_**

<img src="Images/draft_Side.PNG" width="400">

<img src="Images/draft_Position.PNG" width="400">

**Note (Position)**:

<img src="Images/draft_Conference.PNG" width="400" />

**Note (Conference)**:

## Correlation
<img src="Images/Combine_Corr.PNG" width="500">

<img src="Images/Combine_sig.PNG" width="500">

**Note (Correlation)**:

## Principal Component Analysis
<img src="Images/Combine_PCA.PNG" width="700">
<img src="Images/Combine_PCA_draft.PNG" width="700">
<img src="Images/Combine_PCA_Eig.PNG" width="600">

**Note (PCA)**:

## Combine Summary

### Weight
<img src="Images/Combine_Weight.PNG" width="700">
<img src="Images/Combine_Weight_Position_draft.PNG" width="  700">
<img src="Images/Combine_Weight_Conference_draft.PNG" width="  700">

**Note (Weight)**

### Forty
<img src="Images/Combine_40.PNG" width="  700">
<img src="Images/Combine_40_Position.PNG" width="  700">
<img src="Images/Combine_40_Conference.PNG" width="  700">

**Note (Forty):**

### Bench
<img src="Images/Combine_Bench.PNG" width="  700">
<img src="Images/Combine_Bench_Position.PNG" width="  700">
<img src="Images/Combine_Bench_Conference.PNG" width="  700">
**Note (Bench):**

## Offense Summary
<img src="Images/Offense_Position_draft.PNG" width="  700">

**Note (Offense-Position):**

<img src="Images/Offense_Conference_draft.PNG" width="  700">

**Note (Offense-Conference):**

## Defense Summary
<img src="Images/Defense_Position_draft.PNG" width="  700">

**Note (Defense-Position):**

<img src="Images/Defense_Conference_draft.PNG" width="  700">

**Note (Defense-Conference):**
