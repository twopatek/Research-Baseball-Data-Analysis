# NCAA Pitching App

## Overview

This project consists of a comprehensive **Shiny dashboard** built to analyze, explore, and visualize NCAA Division I baseball **pitching data**. 

The app is designed for baseball analysts, researchers, and fans interested in evaluating historical and team-level pitching performance across multiple dimensions.

---

## Live Dashboard Prototype

➡️ **[Click here to explore the NCAA Pitching Analysis Dashboard](https://matthew-apps.shinyapps.io/baseball_data_analysis/)**  
_The app is hosted via [shinyapps.io](https://www.shinyapps.io)._

## License
MIT License

## Dashboard Features

###  Leaderboard Tab
Quickly explore top pitching performers with built-in filters:
- **Innings Pitched Slider**: Adjust the minimum and maximum IP threshold
- **Top 5 SO (Team-Year & Player-Year)**
- **Top 5 ERA (Team-Year & Player-Year)**
- **Top 5 WHIP (Team-Year & Player-Year)**  
Displayed using interactive DataTables for easy comparison.

---

### Player Ratings Tab
Comprehensive system for evaluating pitchers across multiple dimensions:
- **Customizable prior shrinkage factors** (Bayesian smoothing by IP)
- **User-defined composite rating weights** for ERA, FIP, SO/9, BB/9, etc.
- League-year averages and rescaling to **0–100 rating scale**
- **Dynamic recalculation of ratings** with adjustable inputs
- Expanded Info tab with full explanation of rating logic and design

---

## Data  

- Pitching data was **manually compiled** from publicly accessible season pages on [Baseball Reference](https://www.baseball-reference.com/).  
- All files were **cleaned and standardized locally** before being used in the dashboard.  
- This project is **open source**, and users can reproduce the dataset themselves by following the instructions in the repository.  
- ⚠️ **Note:** Current coverage is partial; not all Division I teams are included. Expanding to full coverage remains a long-term goal.  


---

