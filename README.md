README
================

# RestorationES

## Overview

**RestorationES** is an R package designed to estimate the potential
gains in ecosystem services (ES) from restoration interventions. This
package integrates modeled griddle ecosystem service estimations, with
restoration priority pixels to provide actionable insights for landscape
restoration planning. The primary objective is to estimate the benefits
of restoration actions in a replicable, transparent, and scientifically
robust manner.

## Features

- **Multi-Service Integration**: Integrates different modelded grided
  spatial ecosystem services,: preliminary, and nitrogen sediment
  retention, pollination, and coastal protection.
- **Spatial Data Processing**: Tools for aligning, normalizing, and
  aggregating raster data.
- **Intervention Analysis**: Evaluates potential ecosystem service gains
  under various restoration scenarios.
- **Reproducible Workflows**: Facilitates replicable workflows for
  multiple regions, such as Brazil, Mexico, Peru, Madagascar, and
  Vietnam.

## Data Sources

The package relies on publicly available datasets and state-of-the-art
models: - **Ecosystem Service Data**: Chaplin-Kramer et al. (2022),
*Mapping the Planet’s Critical Natural Assets*. - **InVEST** Integrated
Valuation of Ecosystem Services and Tradeoffs: Natural Capital Project
(2024) - **Restoration Potential**: Adjusted Griscom restoration data,
focusing on priority intervention areas.

## Installation

You can install the package from GitHub as follows:

``` r
# Install devtools if not already installed
install.packages("devtools")

# Install RestorationES from GitHub
devtools::install_github("git@github.com:springinnovate/nbs_op.git")
```

# Usage

Below is an example of a typical workflow using the package:

``` r
# Load the package
library(RestorationES)

# Prepare data for analysis
aligned_rasters <- align_rasters(raster_list, template)

# Normalize raster data
normalized_raster <- normalize_raster(aligned_rasters)

# Calculate aggregated ecosystem services
combined_raster <- process_intervention_area(normalized_raster)

# Visualize results
visualize_es(combined_raster, title = "Aggregated Ecosystem Services")
```

# Methods

## Alignment and Normalization

Raster data is aligned to a consistent resolution and extent, normalized
to a \[0, 1\] scale, and masked to intervention areas.

## Aggregation

Aggregates multiple ecosystem services to estimate the combined benefits
of restoration efforts. Weighted or unweighted aggregation is supported,
depending on user inputs.

## Visualization

Outputs include raster-based visualizations of ecosystem service
potential, priority areas, and regional comparisons.

# Contributing

We welcome contributions! If you would like to contribute, please:

1.  Fork the repository.
2.  Create a feature branch (git checkout -b feature-name).
3.  Submit a pull request.

For bug reports or feature requests, please open an issue in the GitHub
repository.

# License

This project is licensed under the MIT License. See the LICENSE file for
details.

Acknowledgments

Special thanks to the team for guidance and support, as well as
contributors to foundational data sources such as Chaplin-Kramer et
al. (2022) and Griscom restoration models.
