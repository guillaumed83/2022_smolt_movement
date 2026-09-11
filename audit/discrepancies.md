# Source audit and reporting decisions

## Current status: 11 September 2026

**Repository construction and clean-session verification are complete.** The author resolved the original timing, data-provenance, diagnostic-scope and reporting questions. All four aggregate diagnostic gates passed before construction of the compact public posterior archive. The private source audit had 70 PASS, zero FAIL and zero REVIEW before the additional label review below. Combined public verification has **1,531 PASS, zero FAIL and two REVIEW** items: a newly identified D8 channel-label inconsistency and the author-controlled licensing/redistribution decision. No manuscript Word file has been edited.

The final workflow passes **1,460 recalculated numerical checks**, including 1,413 table cells and an independent check of the CMR constant derivation. All three JAGS models compiled and initialized their full chain sets from public inputs alone. All ten scoped figures were visually compared with the embedded reporting targets. Minor typography/geometry differs; Figure 2 labels were separated with leader lines without moving observations, and Figure 8 retains the original stack order and colors. No long model refit has been performed.

Two fresh isolated runs with initially empty output folders executed the README commands `renv::restore()` and `source("run_all.R")`: **12.24 and 12.23 seconds**, peak R-managed heap **217.3 and 201.2 MB**, zero warnings/errors. Package restoration checked the 20 preinstalled locked versions; a new download on another operating system was not tested. Sandboxed renv startup stalled; normal-permission runs succeeded. All 47 generated output files match: 27 CSVs and 10 PNGs byte-for-byte, 10 PDFs after normalizing only CreationDate/ModDate. All seven public analysis scripts parse. Traced analytical file accesses remain project-relative; no private source data or interactive workspace is used. See `clean_run_metrics.csv`, `clean_run_checksums.csv` and `repository_validation.csv` for dated records.

### Authoritative timestamp and data conventions

The apparent **ADT** tide label and the release workbook's `release time (NB)` label were reviewed with the coauthor supplying the data. The author confirms that tide, release and light clock labels follow **Quebec local time**. This is authoritative, not New Brunswick Atlantic time.

The public workflow uses `America/Toronto`, with **no one-hour shift**. All 308,160 minute-spaced clock labels from 1 April through 31 October 2022 produce identical epoch seconds and UTC-04 offsets under `America/Toronto` and legacy `America/Curacao`. This envelope includes every retained release and location event (24 May 16:24 through 21 July 04:03:25) and every displayed tide/light date. UTC acoustic observation instants are preserved.

Receiver files are **coauthor-supplied data used as provided**. No additional false-detection screening or exclusion procedure is known. `_edited.csv` is not evidence of false-detection cleaning. No new filter was applied or reconstructed. Export follows the executed 45-tag selection, ASF-priority merge of overlapping receiver feeds and pooling of receivers into locations/arrays. It matches all 675 source count cells, 540 fitted binary history cells, 270 travel-time cells and 21 network transit times. Nondetections retain zero counts and undefined first/last times. Raw records are excluded.

### Chain-preserving diagnostic gate: passed

Quantities were derived within every retained iteration and chain of `BUGSoutput$sims.array`, before pooling. Moses is `Z2_a`, Butters is `Z4_b`, and neither is `Z3_ab_dodge + Z4_ab`; they partition every fish in every draw. Conditional Moses is the draw-wise ratio Moses/(Moses+Butters).

| Quantity | R-hat | ESS | MCSE | Chain 1 mean | Chain 2 mean | Chain 3 mean |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Moses | 1.000071 | 8675.14 | 0.001510 | 0.266699 | 0.271660 | 0.270092 |
| Butters | 1.000001 | 10200.00 | 0.000324 | 0.321706 | 0.321425 | 0.321752 |
| Neither | 1.000059 | 8434.02 | 0.001561 | 0.411595 | 0.406915 | 0.408157 |
| Conditional Moses | 1.000074 | 8800.51 | 0.001647 | 0.419151 | 0.424557 | 0.422526 |

All four pass R-hat <1.01 and a conservative working adequacy threshold ESS >=2,900. MCSE is approximately 1% of posterior SD. **Proceed without refitting.** Full precision is retained in `derived-data/published_aggregate_route_diagnostics.csv` and recalculated by the public workflow.

The estimator is coda 0.19-4.1: `gelman.diag(..., autoburnin=FALSE, multivariate=FALSE)`, `effectiveSize`, and `spectrum0.ar`. MCSE is `sqrt(sum(chain_spectral_density_at_zero / retained_iterations_per_chain)) / n_chains`. Constant states have undefined R-hat/ESS, not a fabricated passing value.

Fish/index 35 is tag **61899**. Chain-specific Moses probabilities are **0.512647, 0.517941, 0.511176**, pooled 0.513922; Butters is zero and neither is the complement. The chain spread is **0.006765**, under one percentage point. Figure 8 continues to show an uncertain approximately 51% Moses / 49% neither assignment. No substantive route interpretation changes.

The rare intermediate states `Z2_b[35]` and `Z3_b[35]` have R-hat **1.082938**, with chain probabilities 0.000294, 0.002353, 0.001471. These exceptions do not materially alter the displayed route probabilities. All 135 fish-route combinations were compared, including fish associated with other poorly mixing latent states. The largest route-probability chain spread is approximately 3.68 percentage points among uncertain assignments; no strong classification is inferred from near-50/50 probabilities.

Restrict the strong convergence claim to the **19 detection and transition probability parameters**: recomputed maximum R-hat **1.0008670923**, minimum ESS **8838.234688**. The saved R2jags minimum ESS for those parameters is exactly 2,900; estimators must not be conflated. Some latent states have saved ESS 1,900, elevated R-hat or undefined recomputed R-hat. It is incorrect to describe all latent states as R-hat <1.002 or ESS >2,900. Full compact diagnostics retain those exceptions.

### Supplementary Material B and reporting baseline

`Report/Suppl_matB.docx` is now available: SHA256 `26ab4c91a8cabff452cc89d341fce9df8503792ea85de2d15424e68ea0b81a5e`. All **133 Table B1 numeric cells** (19 parameters x seven summaries) match independent posterior summaries at displayed precision. Figures B1/B2 were directly inspected against the source histograms: 12 detection panels and seven transition panels. The public prior overlay uses the exact Beta(1,1) density and quantiles in place of incidental prior-simulation noise.

The main manuscript baseline is `Report/2026-09-07_MS_main_JFB_v2.docx`, SHA256 `b373a5a825e06beaf6d950c6320bf114122e6f570a986d26f93645e26c3af66d`. The author is editing it concurrently. Repository verification uses the audited reporting baseline plus the decisions below; it does not automatically certify subsequent Word revisions. Full page rendering was unavailable; table XML and embedded figures were inspected read-only. No Word file was changed.

`audit/reported_table_cells.csv` contains 1,413 main/supplement reference cells, with the author-approved 2008 correction. It is used only for verification, never as analytical input. A dash for the SD of a receiver with only one detected fish remains undefined, not zero.

**Resolved — Table 2:** the author confirms that the 2008 mean discharge has been corrected to **304.0 m3/s**. No further action is required. The unrounded source value remains 304.0322580645162; this resolution records author confirmation, not a new inspection or edit of the Word file.

### Accepted manuscript corrections

- D6's 21%, D8's 71% and the transition range 6%-92% are **posterior medians**. Figures 6/7 retain medians and central 50%/95% CrI.
- Retain CMR constants **0.270, 0.296, 0.434**, describing them as rounded **products of posterior mean transition probabilities**. Figure 1C instead uses path products calculated within joint posterior draws; cohort route proportions are distinct again.
- Use **9,800 burn-in** and **350,000 total** iterations, thinning 100, three chains. Approximately 10,000 is accepted manuscript rounding. Nominal refit retention is 3,402 draws/chain; preserve the archived **3,400/chain, 10,200 total** without padding or replacement. Archived burn-in metadata says 10,000.
- Restrict Figure 5's caption to its riverine segments ending at **HoT**. Marine travel times remain in Table 1.
- The island-network endpoint is the **last D10/D11 detection**, starting at first D3 detection.
- Figure 2 uses the **eight-hour cutoff** and displayed **yellow/orange/red** classes; no observed transit lies between the competing legacy 480/500-minute cutoffs.
- Use current embedded figures/captions for numbering. Figure **1C** remains included; **1A/1B** remain excluded.

These describe author decisions, not edits already made to Word files by this workflow. Detailed source evidence follows in the archived audit below.

### Construction notes

**Additional reporting review discovered during final verification:** audited Table 3 assigns D8/Rx2a to zone 3, channel **2**. The executed equation `D_Rx2a[i] ~ dbern(Z3_ab_no_dodge[i]*p_Rx2a)` instead maps D8 to the no-dodge state, represented as **(3,3)** in Figure 1C and `data/network_nodes.csv`. The dodge state (3,2) is distinct. This is a cross-output channel-label inconsistency, not a false detection or a change to model connectivity. The public receiver metadata/Table 3 reference retain the audited table value; network nodes and model equations retain the executed model. **Author action:** reconcile Table 3's D8 channel index with Figure 1C/model notation, or explicitly explain a distinct indexing convention. No detection history, probability estimate, convergence conclusion or analytical inclusion changes. No Word file was altered.

The complete seeded bootstrap sequence has now been rerun: 10,000 dispersion replicates yield 13 exceedances, 5,000 likelihood-ratio refits yield five, and all thirteen 2,000-replicate leave-one-year-out runs reproduce their reporting targets. The original sequential seed 20260906 and plus-one correction are preserved.

The 16 eligible historical years include zero-recapture 2002, 2013 and 2018. The independently verified pooled 90th-percentile recapture lag is seven days. The 2022 context window is 17 May-12 June and remains excluded from historical correlations/regressions. All 127 included first recaptures have same-date operating support; the executed annual eligibility rule is unchanged.

Current R2jags overwrites chain RNG fields and internally updates for `max(n.burnin,n.iter/2)`. Optional refits use the already-supported rjags interface to honor explicit seeds and burn-in; a separate 1,000-iteration initialization/adaptation phase precedes the stated MCMC total. Exact historical chain RNG states are unavailable. The original model equations and priors are unchanged.

The CMR comparison preserves the same 28 used input fields and excludes structural constants from convergence failure counts. Split posterior estimates are not presented as reliable estimates. Full long-chain refit runtime/memory have not been measured; model initialization alone does not establish full-refit convergence.

No existing license decision was found. `LICENSE_PENDING.md` preserves that decision for the author. **Analytically verified, but not yet cleared for public release:** the author must select code/data licenses and confirm permission to redistribute the coauthor-supplied curated data. No Git repository, remote, upload, release or public visibility has been created or selected. The pre-existing local `audit/discrepancies.html` was left untouched and is excluded by `.gitignore`; the Markdown report is current.

---

## Archived Pass 1 report (10 September; superseded where resolved above)

The text below records the earlier stop condition and evidence. Its requests and construction-status statements are historical, not current blockers.

Audit date: 10 September 2026. Stage: Pass 1. Construction is paused under the specification's stop condition because unresolved questions affect reported results, interpretation, and input provenance.

The crosswalk identifies 46 required analyses, tables, and figures. The source numerical audit contains **55 PASS, 6 FAIL, and 4 REVIEW** checks. Separately, 1,280 main-manuscript numeric table cells were compared: 1,279 match and one differs. These are source-audit results, not verification of a constructed public workflow.

Paths in the audit CSVs are relative to the original acoustic-tracking project. `../Model_CMR/` denotes the neighboring CMR project. Destination paths in the crosswalk are planned components, not existing files. No raw data, manuscript documents, model workspaces, or posterior draws have been copied into the destination.

## Reporting materials inspected

The available main manuscript is `Report/2026-09-07_MS_main_JFB_v2.docx`, without the requested filename's download suffix `(1)`. Its three tables, captions, relevant text, and eight embedded figures were inspected. The latest saved version was rechecked using shared-read access while Word held the file; no manuscript was edited. Its SHA256 is:

`b373a5a825e06beaf6d950c6320bf114122e6f570a986d26f93645e26c3af66d`

The eight embedded image hashes were unchanged from the earlier visually inspected version. Table comparisons were repeated against the latest saved XML. Later manuscript revisions require a fresh comparison.

`Suppl_matB.docx` was not found in the project, parent study tree, or the directly checked document/download locations. `Report/Suppl_MatA_detailed statespace model .docx` is a different supplement. `just tables and figures.docx` contains an older posterior-summary table and distribution figures labeled C1/C2 under a Supplementary Material B heading; those were used only to identify candidate sources.

Required input: supply the current Supplementary Material B path. Table B1 still requires a cell-by-cell comparison with summaries independently calculated from posterior draws; its current figures must also be inspected.

## Confirmed discrepancies

### 1. Table 2: 2008 mean discharge — resolved

Check `M_Table_2`. The audited manuscript version reported **304.4 m3/s**. The unrounded value in `Output/historical_RST_discharge_screening/annual_discharge_recapture_summary.csv` is **304.0322580645162 m3/s**, which rounds to **304.0**.

The other 109 checked Table 2 cells agree, including totals. All 1,122 Table 1 numeric cells and all 48 Table 3 numeric cells agree at displayed precision. Correlations use unrounded annual source metrics, not manuscript table cells.

Resolved by author confirmation: the 2008 presentation cell has been corrected to 304.0. No further action is required; the source metric and calculations are unchanged.

### 2. State-space convergence claim is too broad

Check `S12`. The current Results explicitly state that all monitored parameters have R-hat below 1.002 and effective sample sizes exceeding 2,900, then attribute broad distributions to limited information rather than poor convergence. The saved fit does not support that full-monitor claim.

Source: `Output/M_5_output.RData`, object `jags_M_5`, including `BUGSoutput$summary` and chain-preserving `BUGSoutput$sims.array`.

- The largest finite R-hat is **1.0829382845** for `Z2_b[35]` and `Z3_b[35]`, both in the saved summary and in the independently recomputed coda diagnostic.
- Other latent-state examples reach 1.065840, 1.060186, and 1.008395; `Z5_a[5]` and `Z5_b[5]` reach 1.002041.
- Saved R2jags effective sample sizes reach **1,900** for nonconstant latent states.
- Recomputed coda R-hat is undefined for `Z2_b[13]` and `Z3_b[13]`. Undefined values cannot be counted as passing.

For the 19 detection/transition probability parameters alone, saved R-hat values are below 1.002 and minimum saved ESS is **exactly 2,900**, not strictly greater. Recalculation with coda 0.19-4.1 on all preserved chains, `autoburnin = FALSE`, gives maximum R-hat **1.0008670923** and minimum ESS **8,838.234688** for these 19 parameters. Different ESS estimators must not be conflated.

Required decision: correct the diagnostic scope and name the estimator, while acknowledging latent-state diagnostic exceptions and reviewing the associated interpretation. Do not delete inconvenient monitors or refit merely to make the statement pass. Detailed parameter-level comparisons remain in the private source audit.

### 3. Posterior medians are called means

Checks `M01` and `M02`. Source: `Output/M_5_output.RData`; Figures 6/7 use medians and 50%/95% credible intervals.

| Reported quantity | Manuscript calls it a mean | Actual posterior mean | Posterior median |
| --- | ---: | ---: | ---: |
| D6 detection probability | 21% | 28.50% | 21.08% |
| D8 detection probability | 71% | 68.48% | 71.21% |
| Transition-probability range | 6%-92% | 8.07%-88.79% | 5.68%-92.06% |

Recommended resolution: label these numbers as posterior medians, consistent with the figures, or deliberately replace them with means. Preserve the distinction from route-use proportions, which correctly use posterior means.

### 4. CMR fixed passage constants have a different derivation

Check `M03`. The current CMR Methods describe 0.270/0.296/0.434 as posterior means of marginal passage probabilities propagated through the network. These executed constants instead match rounded **products of marginal transition means**.

All three summaries below were independently calculated from the saved state-space fit:

| Quantity | Moses | Butters | Neither |
| --- | ---: | ---: | ---: |
| Mean inferred proportion among the 45 fish | 0.2694836601 | 0.3216274510 | 0.4088888889 |
| Mean of joint-draw marginal path probabilities | 0.2696285976 | 0.2946488626 | 0.4357225398 |
| Products of marginal transition means | 0.2701180412 | 0.2957755093 | 0.4341064496 |

`Script/1b_network.R` calculates the plotted `Links$weight` and `Links$weight_sd` from products formed within each joint draw. Figure 1C therefore uses the second definition; the rounded fixed CMR constants match the third. They are not realized cohort proportions.

Recommended resolution: correct the CMR provenance wording. **Retain the executed constants 0.270, 0.296, and 0.434** when reproducing the sensitivity analysis. Substituting newly calculated joint-draw means would change that analysis.

### 5. Total iterations are described as additional sampling

Check `M04`. `Script/3_run_model_M_5.R` and its duplicate `Script/scripts_MS/3_run_state-space_model.R` specify 350,000 for `n.iter`, thin 100, three chains, and R seed 20230122. R2jags interprets `n.iter` as a total-iteration control, not an additional post-burn-in count.

The specification's **9,800 burn-in setting remains authoritative**. Describing that as approximately 10,000 is accepted rounding, not an error and not grounds for a refit. With 9,800, nominal sampling arithmetic is 340,200 post-burn-in iterations, or 3,402 retained draws per chain. The archived array actually contains 3,400 x 3 x 650 values: **10,200 retained draws**, matching the manuscript's 1.02 x 10^4. Archived metadata records 10,000 burn-in.

Recommended resolution: remove the claim of an additional 350,000 iterations after burn-in. Preserve the required 9,800 refit setting and the archived draws without padding or replacement.

The installed R2jags 0.8-9 also overwrites supplied chain RNG fields and internally updates with `max(n.burnin, n.iter/2)`. Construction must verify the interface and use the already-supported rjags interface if necessary to honor explicit chain seeds. Historical package behavior cannot be inferred from the current installation.

## Additional decisions and provenance gaps

### 6. Tide, release, and light timestamp conventions

Check `P04`. `Data/Tide/predictions_02175_Campbellton_2022-05-24.csv` and its companion explicitly label timestamps **ADT**. `Script/1_Explore_raw_data.R` strips that label and parses them with `America/Curacao` (UTC-04). New Brunswick ADT in this 2022 window is UTC-03. Acoustic detections are read as UTC and converted while preserving their actual instants.

Parsing the same tide timestamp under those two conventions produces a verified **3,600-second difference**, shifting the tide curve relative to fish detections. This is an input-interpretation issue, not merely an axis-label choice.

The tagging workbook `Data/2023_exp/Smolt tagging ASF 2022.xlsx` labels release times `release time (NB)`, also parsed as UTC-04. The actual recording convention must be confirmed before concluding whether release-based travel times need correction. Light times in `Data/Sun/NRC_sunset_sunrise.xlsx` require corresponding convention verification.

Required decision: confirm the providers' timestamp conventions, then choose faithful source reproduction with a documented caveat or a reviewed correction with matching manuscript changes. No timestamps have been silently changed.

### 7. False-detection cleaning provenance

Check `P03`. `Script/1_Explore_raw_data.R` and `Script/2_generate_data_jags.R` select the 45 tag IDs and aggregate/pool receiver observations. Some incoming ASF files end in `_edited.csv`, but no explicit false-detection exclusion list, filter, or upstream cleaning certification was located. This does not demonstrate that detections are false.

Required input: identify the cleaning record or confirm which incoming receiver datasets had already been cleaned. Do not invent a new filter when exporting the curated location events.

### 8. Figure 5 caption extends beyond its plotted data

Check `M05`. The embedded `image5.tiff` shows km/day for release-to-Rx1 (n=25), island-network passage (n=21), and release-to-HoT (n=44). It corresponds to `Figures/speed_summaryup_to_HoT_km_day.tif` from `Script/1_Explore_raw_data.R`. Its caption describes speeds through to SoBI.

Required decision: shorten the caption to the actual HoT extent or intentionally expand the figure. Other marine-segment values do appear in Table 1, but that does not make them present in this image.

### 9. Endpoint and figure-label inconsistencies

The island-network Methods, Figure 2, and source calculation use first detection at D3 to **last** detection at D10/D11. One Results sentence instead says first detection at the downstream boundary. The source interval includes receiver residence time. Recommended resolution: correct that Results endpoint wording.

The current Figure 2 is the two-panel tide display, not the single-panel source alternative. Tracks are yellow/orange/red while its caption calls the first color green. Source point/line classification also uses competing 500/480-minute cutoffs. A direct check of all 21 transit times in `data_fig2-3.Rdata` found no fish in the 480-500-minute interval, so this inconsistency does not change the displayed classifications. Use the stated 8-hour boundary consistently when reconstructing the display.

Some in-text figure references and older figure numbers are stale. The crosswalk uses current captions and embedded images: network 1C, tide 2, historical proportions 3, discharge 4, speeds 5, detection 6, transitions 7, routes 8. Figure 1A/1B are excluded. The older table's MM/HoT counts of 45 are superseded: current Table 3 correctly gives 38/44.

## Verified analytical lineage

### Historical recaptures and discharge

There are 16 eligible years and 13 informative years; 2002, 2013, and 2018 have zero downstream recaptures. Informative years contain 21,674 marked fish and 87 Moses plus 40 Butters first recaptures. Curated input must retain eligible zero-recapture years with undefined conditional proportions.

Annual operation records define eligibility. The optional same-date operational-support output confirms positive same-date support for all 127 included first recaptures; this does not replace the annual inclusion rule.

The pooled proportion, exact interval, dispersion, beta-binomial fit, implied among-year SD, and empirical-Bayes estimates agree with reported targets. All 10,000 dispersion replicates were regenerated using seed 20260906 and matched their archive within 1e-12. The 5,000 likelihood-ratio bootstrap and 2,000-per-omission leave-one-out records were checked against archives; their complete simulation/refit sequences have not yet been rerun. Preserve the original sequential RNG stream in construction.

The pooled 90th-percentile marking-to-first-recapture lag was independently recalculated as seven days for 127 fish. Historical coverage is complete. The 2022 window is 17 May-12 June, based on all Kedgwick marking dates plus seven days, not only acoustic release dates. Pearson results, 2022 mean discharge 447.1481481481 m3/s, and CV 0.4819984386 agree with the targets. Exclude 2022 from historical correlations and regressions.

### State-space routes

The saved M5 fit is the final source. Its 12 detection and seven transition priors are Beta(1,1); preserve its equations and connectivity, including `Z4_b` conditional on `Z3_b`.

Moses, Butters, and neither states are `Z2_a`, `Z4_b`, and `Z3_ab_dodge + Z4_ab`. They partition every fish in every saved draw. The 2022 conditional Moses quantity is calculated within each draw as Moses/(Moses + Butters): mean **0.4220780032**, 95% CrI **0.1176470588-0.6285714286**. These and the three reported cohort route means/intervals agree with source results.

### Final CMR comparison

`../Model_CMR/2023-12-21_jfb_with split from acoustic/compare_M5_split_vs_no_split.R` explicitly selects:

- Original: `../Model_CMR/2023-07-30_jfb_model/Output/_simulation_coda_R2jags_final_update_2023-07-30_M5b_case_study.RData`, object `jags_M5_case_study`; model `Models/2023-07-30_case_study_M5b.bug` in that project.
- Split v2: `../Model_CMR/2023-12-21_jfb_with split from acoustic/Output/_simulation_coda_R2jags_final_update_2023-12-21_M5b_case_study_with_split_v2.RData`, object `jags_M5_case_study_split_v2`; model `Models/2026-06-26_case_study_M5b - split_v2.bug` in that project.

The models' 28 actually used input fields are exactly identical, covering 2002-2019. Drop unused `I_tot` from the saved 29-field `data_pooled` export. Saved initial values are available.

Both used two chains, 10,500,000 total iterations, thin 2,000, and R seed 202107. Original burn-in was 500,000 (5,000 draws/chain); split v2 burn-in was 1,000,000 (4,750 draws/chain). Explicit historical per-chain RNG states are not archived.

Original diagnostics meet maximum R-hat 1.012 and minimum ESS 670 after excluding structural constants. Each split downstream effective/conditional probability fails the stated criteria in 13/16 modeled years; abundance fails in 16/18. Only 2009 and 2016 pass simultaneously. Do not present split posterior estimates as reliable estimates or mistake constants' stored ESS of 1 for failed estimation.

## Handoff and resumption

Only the three required source-audit records exist in the destination: the crosswalk, this discrepancy report, and verification results. Private R audit/extraction scripts and their working outputs remain in the original source project, outside the destination.

The numerical source audit last completed in **2.58 seconds** under R 4.5.2. This is not a default-workflow or full-refit runtime. Main-table comparisons ran successfully; all three private R scripts parse. Full rendered manuscript-page QA was unavailable; embedded figure images were inspected directly. No default public workflow, model refit, package restoration, or pair of clean public runs has been performed.

Pass 2 and Pass 3 have not started. There are no public curated inputs, model files, posterior archive, README, lockfile, license, citation file, or rendered outputs yet. Default and full-refit commands and their runtimes are therefore not available. No existing license decision was located; construction should use `LICENSE_PENDING.md` unless the author supplies a decision.

Resume after receiving current Supplementary Material B and the substantive decisions above, particularly diagnostic interpretation and timing/cleaning provenance. Then finish Pass 1, construct the minimal repository, and run the two clean verification passes. Keep raw detections, hydrometric/RST records, drafting files, obsolete branches, and Figure 1A/1B code out of the destination.

**Not ready to initialize or push as a public repository.** No Git repository, remote, release, upload, or publication has been created.
