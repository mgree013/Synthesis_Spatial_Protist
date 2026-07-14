# Synthesis_Spatial_Protist

**Title:** Productivity and Trophic Interactions Alter Spatial Benefits of Metacommunity
Persistence Across Network Sizes and Structures

**Authors:** Matthew D. Green, Clara Woodie, Megan Whitesell, Sean Hayes, and Kurt E. Anderson

**Journal:** Oikos — Manuscript ID **OIK-09524**

**Status:** Submitted (Research Article)

**Keywords:** Connectivity, Data Synthesis, Metacommunity, Predator-Prey, Protists, Trophic

**Source data:**
- Green et al. 2022 — https://doi.org/10.5061/DRYAD.3J9KD51KX
- Holyoak 2014 — https://doi.org/10.5061/DRYAD.SC1PQ
- Holyoak & Lawler 2014 — https://doi.org/10.5061/DRYAD.P1N86

This project synthesizes protist predator-prey microcosm experiments to test how
metacommunity size, spatial connectivity, productivity, and predator identity affect
predator-prey persistence — measured through extinction times, colonization/extinction
dynamics, and occupancy patterns.

---

## Script Order & Dependencies

Scripts must be run in numeric order (`s0` → `s6`); later scripts depend on objects created
by earlier ones.

| Script | Depends on | Creates |
|---|---|---|
| `s0_load_data.R` | raw CSV | `Data`, `reg.all`, `reg.ext`, `loc.all` |
| `s1_plot_struct_syn.R` | none (standalone) | Fig. 1 network diagrams, `synch.datas` |
| `s2_occupancy_analysis.R` | `newer_pa_datas`, `all_pa_dataz` (from s4/s5) | `occupnacy`, `Ext_col_data_network`, Fig. 3 & S2, Table S2 model sets |
| `s3_tim2ext_analysis.R` | `loc.all`, `reg.all` (s0) | Fig. 2 & S1, Table S1 model sets |
| `s4_ext_col_analysis.R` | `Data`, `all_pa_dataz` (s5) | `Datazz`, `new_pa_datas`, `newer_pa_datas`, `Ext_col_data`, Fig. 4, 5 & S3, Table S3 model sets |
| `s5_predicted_occupancy.R` | `Data`, `occupnacy` (s2), `reg.all` (s0) | `all_pa_dataz`, `Ext_col_data`, `pred_Ext_col_data`, `pred_network`, Fig. 6, Table S4 model sets |
| `s6_trophic_anaysis.R` | `loc.all` (s0) | Fig. 7, Table 2 model sets |

**Practical run order** (resolves the circular s2/s4/s5 references above): `s0` → `s4`
(through `newer_pa_datas`) → `s5` (through `all_pa_dataz`) → `s2` → rest of `s5` → `s3` →
`s6`. Consider consolidating shared prep (`Datazz`, `new_pa_datas`, `newer_pa_datas`,
`all_pa_dataz`) into `s0` or a dedicated prep script to remove this ambiguity going forward.

---

## Figures (confirmed against manuscript, OIK-09524)

| Figure | Content | Script |
|---|---|---|
| **Fig. 1** | 16 experimental network topologies | `s1_plot_struct_syn.R` |
| **Fig. 2** | Local time to extinction, prey (a-d) & predator (e-h) vs. productivity/predator ID/meta size/connectivity | `s3_tim2ext_analysis.R` |
| **Fig. 3** | Local occupancy dynamics, prey (a-d) & predator (e-h) | `s2_occupancy_analysis.R` |
| **Fig. 4** | Local colonization probability, prey (a-d) & predator (e-h) | `s4_ext_col_analysis.R` |
| **Fig. 5** | Local extinction probability, prey (a-d) & predator (e-h) | `s4_ext_col_analysis.R` |
| **Fig. 6** | Predicted-vs-observed occupancy (network a,b; local c,d) + colonization (e,f) + extinction (g,h) | `s5_predicted_occupancy.R` |
| **Fig. 7** | Predator-prey occupancy by productivity × species pair (Euplotes-Tetrahymena, Didinium-Colpidium, Didinium-Paramecium) | `s6_trophic_anaysis.R` |
| **Fig. S1** | Local (a,b) & regional (c,d) extinction counts, prey (a,c) & predator (b,d) | `s3_tim2ext_analysis.R` |
| **Fig. S2** | Regional/network-level occupancy dynamics, prey (a-d) & predator (e-h) | `s2_occupancy_analysis.R` |
| **Fig. S3** | Prey occupancy (a) & extinction probability (b) vs. local bottle volume (32mL vs 50mL) sensitivity check | `s4_ext_col_analysis.R` |

All `#Figure N` comments in the scripts have been corrected to match this table (notably,
`s6`'s trophic figure was previously mislabeled `#Figure 8` — it's `Fig. 7`; several S1/S2/S3
supplementary plots had no label at all and now do).

## Tables (confirmed against manuscript, OIK-09524)

| Table | Content | Script |
|---|---|---|
| **Table 1** | Experiment/network metadata (source, predator/prey, productivity, size, connectivity, bottle size/volume) | Manual — not code-generated |
| **Table 2** | Trophic GLMs: prey occupancy ~ predator occupancy, per predator-prey × productivity combo (5 models vs. null) | `s6_trophic_anaysis.R` |
| **Table S1** | GLM model sets: local time to prey/predator extinction (Poisson) | `s3_tim2ext_analysis.R` |
| **Table S2** | GLM model sets: local & regional prey/predator occupancy (binomial) | `s2_occupancy_analysis.R` |
| **Table S3** | GLM model sets: local prey/predator colonization & extinction probability (binomial) | `s4_ext_col_analysis.R` |
| **Table S4** | Beta regression: observed vs. predicted occupancy, colonization/extinction vs. observed occupancy (local & network scale) | `s5_predicted_occupancy.R` |

Every `reported.table2 <- bbmle::AICtab(...)` call across all scripts is now preceded by a
comment identifying exactly which manuscript table and response variable it corresponds to
(these were previously unlabeled and reused the same generic variable name throughout,
making it impossible to tell blocks apart without re-reading the surrounding code).

---

## Core Data Objects

### `Data` (s0)
Raw dataset (`upd.datas.all.csv`) with two added columns: `pred.prey.oc` (1 if both predator
and prey occupied same day) and `predator` relabeled to title case (`"Didinium"`/`"Euplotes"`).

> ⚠️ **`bottle` vs `bottle.number`:** `bottle` (427 unique values) is *not* a stable bottle
> identifier across days — it increments per sampling event/row. `bottle.number` (25 unique
> values) is the correct stable per-network bottle ID. Always build unique bottle IDs from
> `bottle.number`, never from `bottle`.

### `reg.all` (s0) — regional/network-level summary
One row per network (`newID`), aggregated across bottles and sampling days (day 3–75,
`number.bottles > 1`). Contains `prey.oc`/`pred.oc` (mean daily network occupancy),
`prey.time.2.ext`/`pred.time.2.ext`, persistence, CV, and extinction labels.

### `reg.ext` (s0)
Slim lookup: `newID`, `reg.prey.ext`, `reg.pred.ext` — joined into `loc.all`.

### `loc.all` (s0) — local/bottle-level summary
One row per bottle per network (`newBottleID`), aggregated across sampling days. Contains
per-bottle fractional occupancy, extinction timing, persistence, CV, and species traits.

> **Fixed bug:** `unite()` calls building `newID`/`newBottleID` previously used positional
> range syntax (`number.bottles:predator`) which silently swept in unintended columns
> (including `day`), fragmenting every bottle's time series into single-day groups and
> collapsing `prey.oc`/`pred.oc` to 0/1 instead of true fractional occupancy. Fixed by using
> explicit column-name vectors in every `unite()` call.

### `Datazz`, `new_pa_datas`, `newer_pa_datas` (s4)
Add lagged occupancy columns (`pred.oc-1`, `prey.oc-1`) per bottle to detect day-to-day
colonization/extinction transitions.

### `occupnacy` (s2) — local occupancy + colonization/extinction rates, bottle level
### `Ext_col_data_network` (s2) — same, aggregated to network level
### `Ext_col_data` (s4, s5) — colonization/extinction transition probabilities per bottle,
including `pred.prey.oc`/`pred.pred.oc`: predicted equilibrium occupancy from a Levins-style
metapopulation model.
### `all_pa_dataz` (s5) — bottle-level observed occupancy + covariates lookup
### `pred_Ext_col_data` (s5) — local-scale predicted-vs-observed occupancy table (feeds Fig. 6c-h)
### `pred_network` (s5) — network-scale predicted-vs-observed occupancy table (feeds Fig. 6a,b)
### `loc.all.plot` + trophic-pair subsets (s6) — relabeled `loc.all` split by predator-prey
pair and productivity level, feeding Fig. 7 / Table 2.

---

## Known Data-Quality Notes / Open Issues

1. **`bottle` column is not a stable bottle identifier** — use `bottle.number` for any
   grouping/uniting meant to represent "one physical bottle over time."
2. **Avoid positional `:` ranges in `unite()`/`select()`** (e.g. `number.bottles:predator`) —
   these depend on column order in `Data` and will silently include/exclude the wrong
   columns if the source CSV's column order ever changes. Use explicit `c(col1, col2, ...)`
   instead.
3. **`s2`, `s4`, `s5` have circular-looking dependencies** — see run-order note above.
   Recommend refactoring shared data prep into `s0` or a dedicated shared-prep script.
4. **`raster::cv()`** is used for coefficient of variation; `raster` also masks
   `dplyr::select`, `dplyr::filter`, etc. — prefix with `dplyr::` where needed to avoid
   silent function-masking bugs after package updates.
5. **Figure/table comments have now been corrected and added** throughout all scripts to
   match the current manuscript numbering (previously several were missing, mislabeled, or
   used generic variable names with no indication of which manuscript output they fed).
