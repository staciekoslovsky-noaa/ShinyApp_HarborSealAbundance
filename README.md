# Coastal Harbor Seal Abundance: Shiny App

This repository stores the code and required data for running the Shiny app that was developed for representing harbor seal abundance estimates visually and interactively. This work was done collaboratively with a UW CICOES intern (Allison James).

The code and files within this repository are detailed as follows:
* **app/Data folder** - contains shareable app inputs committed with the repository
* **LegacyCode folder** - contains older code that was used to develop an earlier iteration of the application
* **R/harbor_seal_shared.R** - shared helper code used by both data prep and the Shiny app runtime
* **DataPrep/HarborSealAbundance_PrepData4App.R** - code for preparing derived artifacts for use in the Shiny app
* **app/** - Shiny app runtime code
* **ShinyApp_HarborSealAbundance_4EDMW.R** version was presented at the NOAA Environmental Data Management Workshop in 2022

The app now expects generated/private artifacts (for example the data cube, trend tables, and derived survey polygons) to live outside version control by default under `local_data/app_artifacts/`. You can override those locations with environment variables such as `HARBOR_SEAL_APP_ARTIFACTS_DIR`, `HARBOR_SEAL_APP_DATA_DIR`, and `HARBOR_SEAL_DATA_CUBE_PATH` when running the prep script or the app.

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.