# Integrated Data QAQC and Visualization System

This system streamlines environmental data workflows by integrating data validation, centralized storage, and interactive visualization. It is composed of three core components:

1. **Data QAQC Checker**  
   Users begin by submitting their data to the QAQC (Quality Assurance/Quality Control) checker. This step ensures the integrity and standardization of incoming datasets.

2. **Centralized Database**  
   Once validated, the data is automatically loaded into a centralized database, serving as the authoritative data source for all downstream analyses.

3. **Shiny Application**  
   The web-based Shiny application, built with the modular and scalable Golem, retrieves raw data from the database, computes key statistics, and presents results through interactive visualizations.

## Key Features

- **Analysis Visualization**: Interactive charts and tables for exploring trends and summaries.
- **MDA Analysis**: Minimum Detectable Abundance (MDA) calculations to assess data sensitivity.
- **Data Download**: Export options for processed datasets and summary outputs.

## Getting Started

- **Data Submission Template**: Use the official submission template available at [https://nexus.sccwrp.org/opcswampchecker/](https://nexus.sccwrp.org/opcswampchecker/) to prepare your data.
- **Methodologies**: Detailed explanations of QAQC rules, statistical calculations, and visual outputs are accessible directly within the Shiny application.

## Technologies Used

- **R / Shiny** for front-end interactivity
- **Golem Framework** for modular app structure
- **Relational Database (e.g., PostgreSQL)** for data storage
- **Custom R packages** for QAQC and statistical routines

## License

This project is maintained by SCCWRP and subject to relevant institutional data use and sharing policies.
