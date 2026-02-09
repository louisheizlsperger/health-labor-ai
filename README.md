# Exploring how AI tools may affect clinician retention and local health-labor markets (LEHD QWI + BLS QCEW)

Hospitals and health systems are rapidly adopting AI tools—most visibly **ambient documentation (“AI scribes”)**, but also message drafting, coding/prior-auth automation, and operational decision support. A central hypothesis is that these tools reduce administrative burden and improve working conditions, which could translate into **higher retention (lower separations)** and **changes in hiring**. This repository builds a public-data foundation to test whether any such patterns show up at scale in **aggregate labor-market outcomes**.

## What we aim to learn (high-level goal)

1) **Do we see detectable shifts in separations, hires, employment, or wages in hospital-heavy labor markets** during the period when AI tools diffuse?  
2) **Are changes concentrated where adoption is plausibly higher** (e.g., markets with large early-adopting systems)?  
3) **What magnitudes are plausible in public aggregates**, and what data would we need to credibly isolate mechanisms (nurses vs physicians, specific tools, specific hospitals)?

## What we do now (public-first baseline)

We start with two official, publicly available data products:

- **LEHD QWI (Quarterly Workforce Indicators)** — quarterly *hires* and *separations* (and related workforce measures) by geography × industry.  
  Source: https://lehd.ces.census.gov/data/#qwi

- **BLS QCEW (Quarterly Census of Employment and Wages)** — quarterly *employment* and *wages* by geography × industry, based on UI records.  
  Source: https://www.bls.gov/cew/downloadable-data-files.htm

In the first issue, we will:
- assemble clean county×quarter panels for **NAICS 622 (Hospitals)** (and related healthcare industries as needed),
- build simple descriptive plots (time series, maps),
- run lightweight “event-study style” checks around candidate adoption timing/exposure measures (initially coarse),
- document where the public aggregates are informative vs where they clearly aren’t.

## Future extensions (once the public baseline is in place)

If the public data show suggestive patterns (or even if they show none), we can extend in several directions:

- **Hospital-level adoption measurement**: integrate structured survey measures (e.g., AHA IT Supplement / other licensed hospital IT datasets) or systematically-coded public adoption announcements.
- **Employer-level workforce dynamics**: add alternative workforce datasets (e.g., Dewey/People Data Labs company insights) to better approximate churn/tenure by employer and role.
- **Occupation specificity (nurses vs physicians)**: explore sources that more directly separate occupations (state licensure renewal modules where accessible; resume/profile-based role tags; internal HR if available).
- **Mechanisms & patient-facing outcomes**: link adoption to operational metrics (wait times, throughput) or clinical proxies (where data access allows).

---

## Repository structure

```
ai-retention/
├─ README.md
├─ .gitignore
├─ run.R                        # central orchestrator; defines repo-relative paths
├─ config/
│  └─ config.R                  # package loads + shared helpers used by build/analysis
├─ build/
│  ├─ code/                     # all build code (download + clean + assemble panels)
│  ├─ run_build.R               # ordered build steps (R)
│  ├─ output/
│  │  └─ logs/
│  └─ output_external_storage/  # symbolic link to large-file location (processed data)
├─ analysis/
│  ├─ code/                     # all analysis code (descriptives + regressions + figures)
│  ├─ run_analysis.R            # ordered analysis steps (R)
│  └─ output/
│     └─ logs/
└─ data_raw/                    # symbolic link to raw data outside repo
```

---

## Quickstart

### 1) Symbolic Links Setup
**CRITICAL**: Create symbolic links to external storage before building or analyzing data.

**Required cloud drive structure (example):**
```
GoogleDrive/health-labor-ai/
├── data_raw/                    # Raw downloads (LEHD/QCEW files as obtained)
├── data_processed/              # Build output (clean panels, intermediate parquet/csv)
└── analysis_data/               # Analysis caches (derived datasets, final extracts)
```

**macOS/Linux:**
```bash
cd /path/to/repo/ai-retention

# Create required directories first (edit paths to your setup)
mkdir -p "/Users/<you>/Library/CloudStorage/GoogleDrive-<account>/My Drive/health-labor-ai/data_raw"
mkdir -p "/Users/<you>/Library/CloudStorage/GoogleDrive-<account>/My Drive/health-labor-ai/data_processed"
mkdir -p "/Users/<you>/Library/CloudStorage/GoogleDrive-<account>/My Drive/health-labor-ai/analysis_data"

# Create symbolic links
ln -s "/Users/<you>/Library/CloudStorage/GoogleDrive-<account>/My Drive/health-labor-ai/data_raw" data_raw
ln -s "/Users/<you>/Library/CloudStorage/GoogleDrive-<account>/My Drive/health-labor-ai/data_processed" build/output_external_storage
ln -s "/Users/<you>/Library/CloudStorage/GoogleDrive-<account>/My Drive/health-labor-ai/analysis_data" analysis/output_external_storage
```

**Windows (Command Prompt):**
```bat
cd /d C:\path\to\repo\ai-retention

REM Create required directories first (edit paths to your setup)
mkdir "C:\Users\<USER>\GoogleDrive\health-labor-ai\data_raw"
mkdir "C:\Users\<USER>\GoogleDrive\health-labor-ai\data_processed"
mkdir "C:\Users\<USER>\GoogleDrive\health-labor-ai\analysis_data"

REM Create directory junctions
mklink /J "data_raw" "C:\Users\<USER>\GoogleDrive\health-labor-ai\data_raw"
mklink /J "build\output_external_storage" "C:\Users\<USER>\GoogleDrive\health-labor-ai\data_processed"
mklink /J "analysis\output_external_storage" "C:\Users\<USER>\GoogleDrive\health-labor-ai\analysis_data"
```

### 2) Credentials (optional but recommended)
Some LEHD access patterns work better with a Census API key. Store it as an environment variable:

- `CENSUS_API_KEY`

Example `.Renviron` entry:
```r
CENSUS_API_KEY="YOUR_KEY_HERE"
```

(Do **not** commit keys to Git.)

---

## Data sources and what we use them for

### LEHD QWI (Quarterly Workforce Indicators)
**Use:** quarterly hires/separations + employment counts by geography × industry.

- Unit: typically county (or other geography) × quarter × NAICS.
- What we’ll focus on: **NAICS 622 (Hospitals)** (and possibly sub-industries, where available).
- Outcomes of interest:
  - separations (proxy for turnover),
  - hires (replacement / growth),
  - employment level (stock).

Notes:
- QWI is a public statistical product (not employer HR microdata). Some cells can be suppressed/noised.

### BLS QCEW (Quarterly Census of Employment and Wages)
**Use:** quarterly employment and wages by geography × industry.

- Unit: county × quarter × NAICS.
- Outcomes of interest:
  - employment level,
  - average weekly wage,
  - establishment counts (where useful).

Important:
- QCEW is excellent for levels/trends, but **does not provide separations/turnover**. Use QWI for hires/separations.

---

## GitHub workflow

- Use Issues for any proposed changes; attach interim tables/figures and short interpretations.
- Work on feature branches:
  - Name branches like `build:add_qwi_download` or `analysis:county_event_study`.
  - Create from an up-to-date `main`: `git checkout main && git pull && git checkout -b <slug>`.
  - Keep scope tight: open a draft pull request linked to the Issue.
  - Sync with `main` regularly.
  - After review, **squash-merge** and delete the branch.

---

## References (official sources)

- LEHD QWI: https://lehd.ces.census.gov/data/#qwi
- QWI technical docs: https://lehd.ces.census.gov/doc/QWI_101.pdf
- BLS QCEW data: https://www.bls.gov/cew/downloadable-data-files.htm
