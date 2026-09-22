# Restigouche smolt routes

Reproducibility companion to **Atlantic salmon smolt migration through an island network: implications for capture-based monitoring**, by Guillaume J. R. Dauphin, Jason M. Daniels and Carole-Anne Gillis (2026 manuscript).

The workflow reproduces historical conditional RST recapture analyses, discharge comparisons, acoustic-telemetry summaries, channel-use posterior outputs and the CMR convergence comparison. Historical conditional recapture proportions are not channel-use estimates. The telemetry-informed CMR split model did not converge adequately; its posterior estimates are not presented as reliable estimates.

## Reproduce the results

Tested with R **4.5.2**, JAGS **4.3.1**, coda **0.19-4.1**, ggplot2 **4.0.0** and rjags **4-17** on Windows. `renv.lock` pins 20 packages including dependencies and renv. JAGS is a separate system installation required for optional refits, not for default archived-draw output generation. A platform without binary packages may also need an R compilation toolchain to restore dependencies.

Start a fresh R session with this folder as its working directory, without restoring a saved workspace. The included `.Rprofile` activates the project environment. Allow the initial renv bootstrap if requested, then run:

```r
renv::restore()
source("run_all.R")
```

If starting R with startup files disabled, activate the environment first with `source("renv/activate.R")`. Dependency installation may require internet access; the analysis itself performs no downloads and reads no source-project files. All analytical paths are relative to this folder. No raw datasets or saved interactive workspace are needed.

The default switches at the top of `run_all.R` are:

```r
refit_state_space <- FALSE
refit_cmr_models <- FALSE
run_bootstraps <- TRUE
write_outputs <- TRUE
```

All 10,000 dispersion, 5,000 likelihood-ratio and thirteen 2,000-replicate leave-one-year-out bootstrap sequences run by default. They preserve seed **20260906**, sequential RNG order and plus-one p-values. Disabling bootstraps produces a REVIEW verification row and is not complete reproduction.

## Optional full refits

In a separate working copy, change either or both refit switches to `TRUE` in `run_all.R`, then run `source("run_all.R")`. Setting variables before sourcing does not override the switches defined inside that file.

| Model | Chains | Total MCMC iterations | Burn-in | Thinning | Chain seeds |
| --- | ---: | ---: | ---: | ---: | --- |
| Channel use | 3 | 350,000 | 9,800 | 100 | 20230122, 20230123, 20230124 |
| Original CMR | 2 | 10,500,000 | 500,000 | 2,000 | 202107, 202108 |
| Telemetry-split CMR | 2 | 10,500,000 | 1,000,000 | 2,000 | 202107, 202108 |

Each model has a separate 1,000-iteration initialization/adaptation phase before these MCMC iterations. All chains specify `base::Mersenne-Twister`. Refits use rjags to honor explicit RNG and burn-in settings. The model equations and priors are unchanged. Initial values and the complete monitoring sets are supplied as documented RDS lists.

The manuscript rounds burn-in to approximately 10,000. The published state-space archive retains its **10,200 joint draws** (3,400 iterations x three chains); nominal retention from the specified refit settings is 3,402 per chain. Exact draws may vary with JAGS/package versions and the historical RNG state is not archived. Published figures use the unchanged archive, not a new fit.

Refit samples are saved under `output/`, never over the published archive. Other generated figures/tables in that working copy use the new fit and may differ. Aggregate route R-hat must be <1.01 with ESS >=2,900 before route outputs are accepted; a failed gate stops execution. Small published-summary differences are reported by verification, not hidden. A long refit is not warranted merely to improve isolated latent-state diagnostics.

## Data and diagnostic provenance

`data/` contains curated analytical units: 16 eligible historical years (including three zero-recapture years), 14 annual discharge summaries, 45 tagged fish, 675 pooled fish-location event rows, receiver/network geometry, narrow tide/light inputs, travel distances and model-ready CMR inputs. Raw detections, hydrometric records, trap histories and manuscript Word files are deliberately excluded. The 28 CMR input fields are identical for the two models; principal annual components are also provided as CSV.

`derived-data/` contains a compact chain-preserving posterior archive, complete compact state-space diagnostics and CMR diagnostic summaries. All four aggregate route quantities pass the author-requested gate. Fish 35 (61899) retains an uncertain approximately 51% Moses / 49% neither assignment, with less than one percentage point between-chain spread. Elevated R-hat for rare intermediate states is documented. The strong convergence statement applies only to the **19 detection/transition probability parameters**, not all monitored latent states.

See `audit/curated_data_dictionary.csv` for all 250 input/archive field definitions and `audit/discrepancies.md` for the author decisions and reporting corrections. Source paths in the crosswalk document lineage only; they are not dependencies. The main manuscript is being edited separately. Verification uses the audited baseline plus approved reporting corrections, not whatever happens to be in a concurrently edited Word file.

## Outputs

Tables are CSV in `output/tables/`: Tables 1-3 and B1; unrounded historical/biological/travel/posterior summaries; bootstrap results; discharge correlations/regressions; network probabilities; aggregate and fish-level chain diagnostics; and the CMR convergence comparison. Table 2 conditional proportions and CIs are displayed as percentages; unrounded analysis files retain proportions. `NA` means undefined or not applicable, never an inferred zero.

Figures are 300-dpi PNG and vector PDF in `output/figures/`:

| Manuscript | Stable output stem |
| --- | --- |
| 1C | `channel_network` |
| 2 | `island_passage_tide_light` |
| 3 | `historical_moses_proportions` |
| 4 | `moses_proportion_discharge` |
| 5 | `riverine_migration_speeds` |
| 6 | `receiver_detection_probabilities` |
| 7 | `transition_probabilities` |
| 8 | `individual_route_probabilities` |
| B1 | `detection_posterior_distributions` |
| B2 | `transition_posterior_distributions` |

Figure 1A and Figure 1B are **not generated**. Figure 5 stops at HoT; its fan ribbons summarize means +/- SD, not posterior credible intervals. Figure 1C labels marginal link means +/- SD, propagated within joint draws. Figures 6/7 show posterior medians and central 50%/95% CrI. Figure 4 contrasts historical exact 95% CIs with the 2022 draw-wise conditional Moses posterior mean and 95% CrI. Figure 3 empirical-Bayes intervals condition on fitted beta hyperparameters. Figure 2 uses the displayed yellow/orange/red scheme and eight-hour cutoff; label offsets improve legibility without changing track coordinates. Exact fonts are not part of numerical reproduction.

`audit/verification_results.csv` is regenerated from unrounded analysis objects and independent reporting references. `output/` also records stage times, total elapsed time, peak R-managed heap, session information, warnings and file checksums. Output files are ignored by Git; only directory placeholders belong in committed source.

## Measured performance and verification

Two isolated default runs completed in **12.24 and 12.23 seconds**, with **217.3 and 201.2 MB peak R-managed heap**, respectively, including every bootstrap. This memory statistic is not whole-process resident memory. Both runs had zero warnings/errors and restored the 20 locked packages from already-installed matching versions; this tested version restoration, not a new download on another operating system. The sandboxed startup stalled; the normal-permission runs completed successfully without a workflow change.

All **47 output files** matched between runs: 27 CSV tables and 10 PNG figures were byte-identical; the 10 PDFs were byte-identical after normalizing only CreationDate/ModDate values. `audit/clean_run_metrics.csv` and `audit/clean_run_checksums.csv` preserve the evidence. Syntax parsing and analytical-file-access tracing passed. Combined verification reports **1,531 PASS, zero FAIL and two REVIEW** items: the author-controlled licensing/redistribution decision and an additional D8 channel-label inconsistency. `audit/repository_validation.csv` holds dated construction/visual-review records; those are carried into the combined report and are distinct from the numerical checks recalculated on every run.

The audited Table 3 calls D8 zone 3, channel 2; the executed D8 likelihood uses `Z3_ab_no_dodge`, represented as state (3,3) in Figure 1C. Table 3 references retain the audited value pending author reconciliation, while model equations and network nodes retain the executed mapping. This reporting-label issue does not change the likelihood, detection histories or posterior results. Do not use the Table 3 index to infer model connectivity; use the explicit model and node-state fields.

All three models compiled and initialized from repository inputs alone. Full long-chain refit wall time and peak memory have **not been measured**; no extrapolated runtime is claimed. The archived posterior and diagnostic files are the exact-reproduction sources; initialization tests do not establish convergence of a new fit.

## Data use and citation

Use the verified author information in `CITATION.cff` and cite the companion manuscript. No DOI or repository URL is assigned here. Licensing and permission to distribute the supplied data require author confirmation before public release; see `LICENSE_PENDING.md`. Citation alone does not confer redistribution rights. No repository has been published by this workflow.
