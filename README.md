# Microplastics Concentration Calculation

This document explains the methodology used to calculate microplastic concentrations by integrating spectroscopy and microscopy data.

## Data Extraction and Grouping
1. **Spectroscopy Data:**  
   - **Grouping:** Data are grouped by the following variables:  
     `bmp`, `year`, `event`, `location`, `matrix`, `replicate`, `size_fraction`.
   - **Count Calculation:**  
     Count the total number of particles for each group, which we denote as `count_spectro`.
   - **Microplastic Proportion:**  
     For each group, calculate the fraction of microplastic particles as:

$$  \text{percentage}_{mp} =  \frac{\text{Number of MP particles}}{\text{Total number of spectroscopy particles}} $$

2. **Microscopy Data:**  
   - **Grouping:** Data are grouped by the same parameters:  
     `bmp`, `year`, `event`, `location`, `matrix`, `replicate`, `size_fraction`.
   - **Count Calculation:**  
     Count the total number of particles for each group, denoted as `count_micro`.

3. **Merging Data:**  
   - Merge the summaries from the spectroscopy and microscopy datasets so that each group has the values:  
     `count_spectro`, `count_micro`, and `percentage_mp`.
   - Additionally, merge these results with the constants dataset, which provides:  
     `sample_volume`, `sub_sample`, `pct_sample_processed`, `pct_filter_counted`, and `unit_passing`.
   - **No

## Calculations

For each group, perform the following computations:

1. **Back-Calculated Particle Count:**  
   The particle count is adjusted using the processing percentages. Depending on the sample type:
   
   - **If the sample is a subsample**:
     
   $$  \text{percentage}_{mp} =  \frac{\text{Number of MP particles}}{\text{Total number of spectroscopy particles}} $$

   - **If the sample is not a subsample** :
  
   $$  \text{percentage}_{mp} =  \frac{\text{Number of MP particles}}{\text{Total number of spectroscopy particles}} $$
   
3. **Microplastic Particle Count:**  
   The microplastic-specific count is calculated by applying the microplastic fraction:
   
   $$  \text{percentage}_{mp} =  \frac{\text{Number of MP particles}}{\text{Total number of spectroscopy particles}} $$

5. **Concentration Calculation:**  
   Finally, the concentrations are computed by normalizing the counts by the passing unit:
   
   - **Overall Particle Concentration:**

  $$  \text{percentage}_{mp} =  \frac{\text{Number of MP particles}}{\text{Total number of spectroscopy particles}} $$

5. **Concentration Calculation:**  
   Finally, the concentrations are computed by normalizing the counts by the passing unit:
   
   - **Overall Particle Concentration:**

  $$  \text{percentage}_{mp} =  \frac{\text{Number of MP particles}}{\text{Total number of spectroscopy particles}} $$

5. **Concentration Calculation:**  
   Finally, the concentrations are computed by normalizing the counts by the passing unit:
   
   - **Overall Particle Concentration:**

  $$  \text{percentage}_{mp} =  \frac{\text{Number of MP particles}}{\text{Total number of spectroscopy particles}} $$
  

  




