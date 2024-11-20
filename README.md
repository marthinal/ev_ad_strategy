# Electric Vehicle Charging Data Analysis Project

## Overview

This repository contains the code and analysis for an academic project aimed at understanding electric vehicle (EV) charging patterns. The primary goal is to optimize the placement and timing of advertisements at EV charging stations, with a specific focus on long-distance travelers. The project leverages exploratory data analysis (EDA) techniques to uncover insights from the dataset and guide decision-making.

## Objectives

1. **Understand Charging Behavior**:
   - Identify patterns in charging times, locations, and user types.
   - Analyze the relationship between charging station usage and day of the week, time of day, and charging types.

2. **Targeted Advertising**:
   - Optimize ad placement for hotels by identifying key patterns in long-distance travelers' charging behavior.
   - Tailor recommendations for specific user types and charging station locations.

## Repository Structure

The repository is organized as follows:

- **Scripts**:
  - `scripts/bivariate_analysis_*.R`: Scripts for performing bivariate analysis between variables such as:
    - Charging type and day of the week
    - User type and charging location
    - End time and day of the week
  - `scripts/multivariate_analysis.R`: Script for multivariate analysis combining multiple variables, such as user type, charging type, location, and time.
  - `scripts/univariate_analysis.R`: Script for analyzing individual variables like charging type, user type, and charging locations.
  - `scripts/load_and_inspect_dataset.R`: Script for loading and inspecting the dataset.
  - `scripts/import_EV_data_into_mysql.R`: Script for importing the dataset into a MySQL database for structured querying.

- **Generated Outputs**:
  - `html_report/`: Directory containing interactive HTML reports generated for each analysis.
  - `images/`: Directory containing static images of the visualizations.

## Key Variables

The following variables were selected for the analysis based on their relevance to the project's goals:

- **UserType**: Identifies the type of user (e.g., Casual Driver, Commuter, Long-Distance Traveler).
- **DayOfWeek**: Helps identify patterns based on the day of the week.
- **Charging.Station.Location**: Segments behavior by charging station location.
- **EndTime**: Determines the end time of charging sessions to understand user behavior.
- **ChargerType**: Differentiates between Level 1, Level 2, and DC Fast Chargers.

## Why This Project Matters

By understanding the behavior of long-distance EV travelers, this project provides actionable insights for optimizing ad placement at EV charging stations. The insights generated can help maximize the visibility and impact of targeted advertisements for hotels, restaurants, and other services catering to EV travelers.