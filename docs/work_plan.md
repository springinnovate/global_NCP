# Project Work Plan: Interpretation & Synthesis Phase

This document outlines the work plan for the final phase of the Global NCP Hotspot Analysis project, focusing on interpretation, synthesis, and manuscript preparation.

## Phase 1: Narrative Development & Results Interpretation

The immediate goal is to translate the quantitative output tables into clear, narrative answers to the project's core research questions.

*   **Action:** Use the `analysis/results_interpretation.qmd` notebook to explore the summary tables (`hotspot_area_stats.csv`, `hotspot_pop_exposure.csv`, etc.).
*   **Objective:** Build the story around "Where are the hotspots?", "Who is affected?", and "What are the drivers?".

## Phase 2: Manuscript & Documentation

With the core methodology stabilized and documented in `docs/methodology.md`, the next step is to draft the formal methods section of the paper.

**Proposed Methods Section Structure:**

1.  **Overview of Analytical Framework:** Briefly introduce the "two-path" analysis and the overall goal.
2.  **Ecosystem Service & Socioeconomic Data:** Detail the source, resolution, and years of all input data.
3.  **Change Calculation & Hotspot Identification:**
    *   Explain the "Path B" (difference of aggregates) approach.
    *   Define the Symmetric Percentage Change (SPC) metric and its rationale.
    *   State the hotspot definition (top/bottom 5%).
4.  **Geospatial Aggregation & Synthesis:**
    *   Describe the regional groupings (WB regions, biomes).
    *   Define the key synthesis metrics: Hotspot Intensity, Global Share, and Relative Intensity.
5.  **Driver Analysis:**
    *   **Land Cover Change:** Explain the `diffeR` methodology and the specific driver models (Forest Loss, Expansion).
    *   **Socioeconomic Profiling:** Describe the Kolmogorov-Smirnov (KS) test and the "median background" sampling strategy.

## Phase 3: Synthesis & High-Level Takeaways

This is the final step of connecting the specific results to broader implications for policy and conservation.

*   **Action:** Use the "Synthesis & Key Takeaways" section in `analysis/results_interpretation.qmd` to draft these points.
*   **Objective:** Formulate high-level arguments supported by the quantitative findings.
    *   *Example Takeaway 1: Geographic Mismatch.*
    *   *Example Takeaway 2: Equity and Vulnerability.*
    *   *Example Takeaway 3: Drivers of Change.*

This structured approach will guide the project from the final data products to a completed manuscript.