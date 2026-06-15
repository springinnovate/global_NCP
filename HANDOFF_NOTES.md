# Project Consolidation & Presentation: Handoff Notes

**Project:** Global NCP - Ecosystem Service Hotspots Analysis  
**Date:** May 2026  
**Status:** Phase 1-3 Complete | Ready for Co-Author Review & Interpretation Phase  
**Presenters:** Jerónimo Rodríguez Escobar, Richard P. Sharp  

---

## Executive Summary

We have consolidated the Global NCP repository structure, updated core documentation, and developed a comprehensive Quarto-based presentation (reveal.js) structured around three narrative pillars: **WHY** (motivation), **HOW** (methodology), and **WHAT** (findings).

The presentation is ready for:
1. Co-author review and feedback
2. Adaptation for specific audiences (scientific, policy, general public)
3. Integration with manuscript development
4. Use in stakeholder engagement and communication

---

## What Was Completed

### ✅ Phase 1: Repository Audit & Consolidation
- **Verified active workflow:** 7 core Quarto notebooks in `/analysis/` follow the intended sequence
- **Archived legacy code:** Legacy notebooks properly organized in `/archive/notebooks/`
- **Documentation audit:** `/docs/` contains methodology, runbook, and data dictionaries
- **Dependencies mapped:** Python (Docker) → R/Quarto pipeline is clear and well-documented

### ✅ Phase 2: Documentation Consolidation
- **Updated README.md** with:
  - New "Quick Start" section with Docker and Quarto commands
  - Detailed "Active R Analysis Workflow" section explaining each .qmd file
  - Clear Python → R data handoff explanation
  - Full execution order and interdependencies
  
- **Key sections added:**
  - Runbook cross-reference for full technical details
  - Data flow diagram showing pipeline stages
  - Prerequisites and command examples

### ✅ Phase 3: Presentation Development
- **Created `presentation.qmd`** — A standalone reveal.js presentation deck
- **Structure:** 
  - **Part 1 (WHY):** 4 slides on problem statement, questions, and project goals
  - **Part 2 (HOW):** 5 slides on data sources, methodology, metrics, KS tests, and pipeline overview
  - **Part 3 (WHAT):** 5 slides on key findings (geographic clustering, socioeconomic profiles, attribution gap, population exposure, synthesis table)
  - **Appendix:** References, data sources, repository structure

- **Features:**
  - Reveal.js slide format with color-coded sections (Dark green for WHY, olive for HOW, orange for WHAT)
  - Mermaid diagram of analysis pipeline
  - Embedded findings and interpretations from `results_interpretation.qmd`
  - Speaker notes and context for each slide

---

## Key Presentation Content

### WHY (Motivation & Context)
- **Problem:** Global ecosystem service decline is uneven and poorly understood
- **Questions:** Where? Who? Why?
- **Goals:** Global scale, transparency, decision support
- **Decision relevance:** Guides conservation, sustainable development, and climate action

### HOW (Methodology & Data)
- **Data:** InVEST models, ESA land cover, IUCN 10km grid, socioeconomic layers (1992 & 2020)
- **Two-path analysis:** Path A (pixel-level) vs. Path B (grid-level, canonical for hotspots)
- **Key metrics:** Absolute change, SPC (Symmetric Percentage Change), KS tests
- **Pipeline:** Python zonal summaries → R data processing → hotspot detection → socioeconomic profiling

### WHAT (Key Findings)
1. **Geographic Clustering** – Hotspots concentrate in Global South (Latin America, East Asia, Sub-Saharan Africa)
2. **Socioeconomic Profiling** – Urbanized areas affected by Nature Access/Coastal Risk loss; agricultural areas affected by Pollination loss
3. **Attribution Gap** – Many ES hotspots don't overlap with land cover conversion hotspots (degradation ≥ conversion)
4. **Population Exposure** – Majority of affected people in middle-income countries (absolute scale matters)

---

## Recommendations for Co-Authors

### For Using the Presentation

1. **Customization:**
   - The presentation is modular—slide can be reordered or expanded for different audiences
   - Add specific regional maps/charts from `/outputs/plots/` to strengthen findings
   - Embed data tables from `hotspot_synthesis.qmd` outputs for technical audiences

2. **Next Steps:**
   - Review "Part 3: WHAT" findings for accuracy and completeness
   - Flag any slides that need revision or additional context
   - Suggest additions to speaker notes or figure captions

3. **Dissemination:**
   - Present to WWF colleagues, policy partners, and external stakeholders
   - Use as basis for manuscript figures and narrative structure
   - Adapt for conference presentations or webinars

### For the Interpretation Phase

The presentation can serve as the **bridge between analysis and narrative synthesis**:

1. **Validate findings** – Do results align with existing literature and expert knowledge?
2. **Deepen narratives** – Expand findings with regional case studies, socioeconomic depth, driver mechanisms
3. **Develop implications** – What do hotspots mean for conservation, policy, and climate action?
4. **Refine messaging** – Clarify trade-offs, limitations, and future work

---

## Repository Changes Made

### Files Added
- **`presentation.qmd`** – Main presentation deck (reveal.js format)

### Files Updated
- **`README.md`** – Added Quick Start section and Active R Analysis Workflow details

### Files Preserved (No Changes)
- **`_quarto.yml`** – Book structure remains intact; presentation is separate
- **All analysis files** (`/analysis/`) – Unchanged; ready for re-running
- **Documentation** (`/docs/`) – All existing docs remain; README now cross-references them

---

## How to Render/Use the Presentation

### Render as HTML Slides
```bash
# From repository root:
quarto render presentation.qmd --to revealjs

# Opens in browser: presentation.html
# Navigation: Arrow keys or click navigation
```

### Render as PDF (for printing or sharing)
```bash
quarto render presentation.qmd --to pdf
```

### View Slides in VS Code
- Open `presentation.qmd` in Quarto preview
- Press "Render" to generate HTML
- Open `presentation.html` in browser

---

## Testing & Validation

### ✅ Completed Validation
- Presentation structure verified (YAML frontmatter valid)
- All sections present and complete
- Links to documentation accurate
- Mermaid diagram syntax correct
- Speaker notes comprehensive

### 📋 Recommended by Co-Authors
- [ ] Review slide content for accuracy
- [ ] Suggest regional examples or specific findings to highlight
- [ ] Verify data citations align with manuscript
- [ ] Test rendering on local machine
- [ ] Provide feedback on narrative flow and pacing

---

## Handoff Checklist

- ✅ Repository structure consolidated and audited
- ✅ README.md updated with active workflow and quick-start guide
- ✅ Presentation deck created (WHY/HOW/WHAT structure)
- ✅ Documentation organized and cross-referenced
- ✅ Ready for co-author review
- ⏭️ **Next:** Co-author feedback → Interpret findings → Manuscript development

---

## Contact & Questions

**Jerónimo Rodríguez Escobar**  
Global Science, WWF  
jeronimo.rodriguez@wwfus.org

**Richard P. Sharp**  
Natural Capital Project  

---

## Appendix: Repository Quick Reference

| Item | Location | Purpose |
|------|----------|---------|
| **Active Analysis** | `/analysis/` | 7 .qmd files (prepare → hotspots → interpret) |
| **Core Functions** | `/R/` | Reusable R functions for analysis |
| **Documentation** | `/docs/` | Methodology, runbook, data dictionary |
| **Utilities** | `/scripts/` | Standalone scripts (mapping, monitoring) |
| **Outputs** | `/outputs/` | Generated plots, maps (gitignore) |
| **Python Pipeline** | `summary_pipeline_landgrid.py` | Zonal statistics (Docker) |
| **Presentation** | `presentation.qmd` | This deck (reveal.js) |
| **Book** | Rendered by `quarto render` | Full analysis documentation |

