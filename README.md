Data Description for Min (2026) Personal Thermal Sensation RF Model

This dataset contains the complete records (n=1,658) used for the Random Forest modeling in the study.

[Variables]

1. Dependent Variable
- pts: Personal Thermal Sensation score reported by citizens.
       (1 = Comfortable, 2 = Normal, 3 = Hot, 4 = Very Hot)

2. Meteorological Variables
- temp: Air Temperature (°C)
- humi: Relative Humidity (%)
- ws: Wind Speed (m/s)

3. Urban Morphological Variables
- road: Road-area ratio (RA) (%)
- green: Green-area ratio (GA) (%)
- building: Average building-area ratio (ABA) (%)
- meanh: Average building height (ABH) (m)
- svf: Sky View Factor (SVF) (%)

[Note]
- To ensure participant privacy and prevent spatiotemporal tracking, all identifiable information (e.g., demographics, precise coordinates, and exact timestamps) has been excluded, retaining only the processed variables essential for model training.
- Please refer to the accompanying R script (RF_Model_2026_Min.R) to reproduce the model performance summary, Permutation Feature Importance, and Confusion Matrix results.
