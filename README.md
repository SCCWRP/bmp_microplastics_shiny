# Microplastics Concentration Calculation

This document explains the methodology used to calculate microplastic concentrations by integrating spectroscopy and microscopy data.

## Data Extraction and Grouping

1. **Spectroscopy Data (`rawftir`):**  
   - **Grouping:** Data are grouped by the following variables:  
     `bmp`, `year`, `event`, `location`, `matrix`, `replicate`, `size_fraction`.
   - **Count Calculation:**  
     Count the total number of particles for each group, which we denote as `count_spectro`.
   - **Microplastic Proportion:**  
     For each group, calculate the fraction of microplastic particles as:  
     $$\text{percentage\_mp} = \frac{\text{Number of MP particles}}{\text{Total number of spectroscopy particles}}$$

2. **Microscopy Data (`rawall`):**  
   - **Grouping:** Data are grouped by the same parameters:  
     `bmp`, `year`, `event`, `location`, `matrix`, `replicate`, `size_fraction`.
   - **Count Calculation:**  
     Count the total number of particles for each group, denoted as `count_micro`.

3. **Merging Data:**  
   - Merge the summaries from the spectroscopy and microscopy datasets so that each group has the values:  
     `count_spectro`, `count_micro`, and `percentage_mp`.
   - Additionally, merge these results with the constants dataset, which provides:  
     `sample_volume`, `sub_sample`, `pct_sample_processed`, `pct_filter_counted`, and `unit_passing`.
   - **Note:** Ensure that the column `is_subsample` (from `rawftir`) is also included in the merged dataset.

## Calculations

For each group, perform the following computations:

1. **Back-Calculated Particle Count:**  
   Determine the particle count adjusted by the processing percentages. Use either the microscopy or spectroscopy count depending on the `is_subsample` flag:
   
   - **If the sample is a subsample** (`is_subsample = 'y'`):
     $$
     \text{back\_calculated\_particle\_count} = \frac{\text{count\_micro}}{\text{pct\_filter\_counted} \times \text{pct\_sample\_processed}}
     $$
   - **If the sample is not a subsample** (`is_subsample = 'n'`):
     $$
     \text{back\_calculated\_particle\_count} = \frac{\text{count\_spectro}}{\text{pct\_filter\_counted} \times \text{pct\_sample\_processed}}
     $$

2. **Microplastic Particle Count:**  
   Adjust the particle count to obtain the microplastic-specific count:
   $$
   \text{back\_calculated\_mp\_particle\_count} = \text{back\_calculated\_particle\_count} \times \text{percentage\_mp}
   $$

3. **Concentration Calculation:**  
   Normalize the calculated counts by the sample's unit passing to obtain the concentration.
   
   - **Overall Particle Concentration:**
     $$
     \text{concentration\_all} = \frac{\text{back\_calculated\_particle\_count}}{\text{unit\_passing}}
     $$
   - **Microplastic Particle Concentration:**
     $$
     \text{concentration\_mp} = \frac{\text{back\_calculated\_mp\_particle\_count}}{\text{unit\_passing}}
     $$

## Final Output

The final output is a dataframe containing, for each group defined by:
- **Grouping Variables:** `bmp`, `year`, `event`, `location`, `matrix`, `replicate`, `size_fraction`
- **Counts:** `count_spectro` and `count_micro`
- **Computed Metrics:** `percentage_mp`, `back_calculated_particle_count`, and `back_calculated_mp_particle_count`
- **Concentration Values:** `concentration_all` and `concentration_mp`
- **Additional Constants:** `sample_volume`, `sub_sample`, `pct_sample_processed`, `pct_filter_counted`, `unit_passing`

This complete set of computations forms the basis for further analysis and visualization of microplastic contamination.
