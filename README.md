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

$$  \text{Percentage of Microplastics Particle} =  \frac{\text{Number of MP particles}}{\text{Total Number of Spectroscopy Particles}} $$

2. **Microscopy Data:**  
   - **Grouping:** Data are grouped by the same parameters:  
     `bmp`, `year`, `event`, `location`, `matrix`, `replicate`, `size_fraction`.
   - **Count Calculation:**  
     Count the total number of particles for each group, denoted as `count_micro`.

## Calculations

For each group, perform the following computations:

**Back-Calculated Particle Count:**  
   The particle count is adjusted using the processing percentages. Depending on the sample type:
   
**If the sample is a subsample**:

$$  \text{Back Calculated Particle Count} =  \frac{\text{Number of Microscopy Particles}}{\text{Percent Filter Counted} \ \times \ \text{Sample Processed}} $$

**If the sample is not a subsample**:

$$  \text{Back Calculated Particle Count} =  \frac{\text{Number of Specstrocopy Particles}}{\text{Percent Filter Counted} \ \times \ \text{Sample Processed}} $$

**Back-Calculated Microplastics Particle Count:**   
   The microplastic-specific count is calculated by applying the microplastic fraction:

$$  \text{Back Calculated MP Particle Count} = \text{Back Calculated Particle Count} \ \times \ \text{Percentage of Microplastics Particle}  $$

**Concentration Calculation:**  
   Finally, the concentrations are computed by normalizing the counts by the passing unit:

**All Particle Concentration**:

$$  \text{Concentration} = \frac{\text{Back Calculated Particle Count}}{\text{Unit Passing}} $$

**Microplastics Particle Concentration**:

$$  \text{Concentration} = \frac{\text{Back Calculated MP Particle Count}}{\text{Unit Passing}} $$





