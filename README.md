# ASCETIC-Vision

ASCETIC-Vision is a dashboard built using the Shiny package in R. It is designed to facilitate the analysis of cancer evolutionary signatures and patient stratification performed by ASCETIC framework.

## Dashboard Sections

ASCETIC-Vision is structured into several sections, each corresponding to a specific phase of the ASCETIC framework:

### Home Page
- **Load Project**: Load a previously saved analysis.
- **New Analysis**: Start a new analysis from scratch.

### Inference of Evolutionary Signatures
- **Input Data - Genomic**: Upload necessary files based on the sequenced data type.
- **Evolution Model Inference**: Set parameters to activate the function that identifies evolutionary signatures.
- **Confidence Estimation**: Set parameters for executing the confidence estimation process.

### Survival Analysis
- **Input Data - Survival**: Choose to continue with the same patients from the inference phase or load new ones. Additionally, upload a file containing survival data.
- **Evolutionary Signatures**: Activate the function that divides patients into different risk groups, calculates a hazard ratio for each evolutionary step, and determines the prevalence of each step in each patient cluster.

### Saving Phase
- Finalize and save the project for future reference or further analysis.

## Sample Data

To test the functionality of ASCETIC-Vision, sample data is available in the `data_input` folder:

### Input Data - Genomic
You can load one of three different files depending on the type of sequenced data:
- `dataset_bulk_single_region`
- `dataset_bulk_multi_region`
- `dataset_single_cell`

For inference with resampling using `dataset_bulk_single_region`, also upload `resampling_database_bulk_single_region`.

For `dataset_bulk_multi_region` and `dataset_single_cell`, upload the corresponding `trees_bulk` or `trees_single_cell` folder.

### Input Data - Survival
Upload the survival data using `survival_bulk_single_region`.

## Getting Started

1. Clone the repository.
2. Navigate to the project directory.
3. Launch the Shiny application by running the script in R.
