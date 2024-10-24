
# Niayes 2040: Model Calibration and Scenario Exploration

This repository contains the scripts and data for calibrating models and visualizing data to explore the future scenarios of the **Niayes region in Senegal**. The project focuses on simulating land use and hydrological dynamics to assess plausible futures under different environmental, demographic, and economic pressures.

### Project Overview

The **Niayes 2040** project aims to model and simulate the land and water dynamics in the Niayes region to help decision-makers plan for sustainable development. This repository includes:
- **Model Calibration**: Hydrological models and land-use models are calibrated to simulate the effects of urbanization, agricultural development, and other environmental changes.
- **Scenario Exploration**: Multiple scenarios, including urbanization, groundwater salinization, and demographic growth, are simulated to assess their impact on the region's future.
- **Sensitivity Analysis**: The model parameters are optimized through sensitivity testing to improve accuracy and robustness.
  
### Key Features

1. **Hydrological Model Calibration**: Calibrating hydrological models to simulate the impacts of climate change and groundwater depletion.
2. **Land Use Model Calibration**: Simulation of land-use changes, focusing on the balance between agricultural and urban expansion.
3. **Statistical Analysis**: Descriptive statistics and demographic projections provide critical inputs to the simulation models.
4. **Farm Typology Segmentation**: Using machine learning techniques (K-means, Random Forest) to classify different types of agricultural practices.
5. **Data Visualization**: Automated generation of graphics to visualize model outputs and scenario results.

### Tools and Libraries
- **R**: For model calibration, statistical analysis, and data visualization.
- **Python**: For some statistical modeling and machine learning tasks.
- **Gephi**: (optional) For network analysis and visualizing complex relationships in the data.

### How to Run

1. Clone the repository.
2. Install the necessary R packages listed in `used_packages.R`.
3. Run the scripts in each directory according to the instructions provided.

### File Structure

- `sensitivity_analysis/`: Scripts and notebooks for conducting sensitivity analysis on model parameters.
- `hydro_model_calibration/`: R scripts for calibrating hydrological models for the Niayes region.
- `land_use_model_calibration/`: Scripts for calibrating land-use models, focusing on irrigation and urban growth.
- `descriptive_stats/`: Contains demographic and agricultural statistical analysis.
- `farm_typology/`: Machine learning scripts for generating farm typologies.
- `archives/`: Old versions of scripts and documentation (optional for exploration).
- `used_packages.R`: A list of required R packages for the project.

### Next Steps
- Refine the hydrological and land-use models by incorporating more granular climate and soil data.
- Explore additional demographic scenarios to evaluate their impact on land and water resources.
- Use results from the sensitivity analysis to improve model precision.
