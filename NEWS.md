# aquacropr (development version)

Initial development. Not yet released. Features are tracked here as they're
added, grouped by area.

* `expand_factors()` and `suggest_calibrate_factors()` have been removed.
  Use `expand_names()` and `suggest_calibrate_names()`. The argument formerly
  called `factors` is now `names`. Sensitivity index tables use a `name`
  column (not `factor`). Metamodel objects store `param_names`.

## Running AquaCrop

* `set_aquacrop_path()` / `get_aquacrop_path()` - session default for the
  FAO stand-alone executable (`options(aquacropr.aquacrop_exe)`), with
  fallback to `AQUACROP_EXE` and common install names (`aquacrop.exe`,
  `ACsaV73.exe`, ...).
* `run_aquacrop()` - run the plugin from a working directory that already
  contains `LIST/`, `SIMUL/`, and `OUTP/`. Optional `cmd` glue template
  for HPC/Singularity. Default
  `progress_window = FALSE` suppresses the AquaCrop 7.3 Tk progress
  dialog by briefly renaming `_internal/_tkinter.pyd` (FAO already
  runs headless when Tk cannot import). `program_parameters = TRUE`
  (default) lets AquaCrop load sibling `.PP1`/`.PPn` files;
  `FALSE` hides them for that run so Table 2.19a defaults apply.
* `prepare_plugin_workdir()` - copy `SIMUL/`, copy `.PRO`/`.PRM` files into
  `LIST/`, write `LIST/ListProjects.txt`, and create `OUTP/`. Optional
  `daily_results` writes `DailyResults.SIM` so the plugin emits daily
  `.OUT` files.
* `write_daily_results_sim()` - select daily output codes (1–8).
* `write_project()` - construct a `.PRO` (one run) or `.PRM`
  (several seasons) from crop, soil, climate, dates, and optional
  irrigation / field-management / calendar / groundwater / initial /
  off-season / observation files. Optional files are written as
  `(None)` (FAO defaults). Directory lines are relativized to the
  plugin folder unless `relativize_to = FALSE`. Optional
  `program_parameters = TRUE` (or a named list of overrides) writes
  a sibling `.PP1`/`.PPn` via `write_pp()`.
* `write_pp()` - construct a `.PP1` (single run) or `.PPn`
  (multiple seasons) program-parameter file. Same basename as the
  project, not referenced inside the `.PRM`. FAO defaults unless
  you override named fields (`ke_max`, `cn_amc`, ...).
* `set_simul_dir()` / `get_simul_dir()` - default `SIMUL/` location (the
  plugin's own `SIMUL` folder, or a copy of it).

## Dates

* `aquacrop_day_number()` / `aquacrop_date()` - convert between `Date` and
  AquaCrop day numbers (days since 0 January 1901, FAO formula valid
  1901–2099).

## Climate files

* `write_weather()` - write `.Tnx`, `.ETo`, or `.PLU` files from a daily
  table.
* `write_cli()` - write a `.CLI` climate wrapper naming the four weather
  files.
* `write_sol()` - write a 7.x `.SOL` profile (up to five horizons).
  Default CN, REW, and CRa/CRb follow FAO (Ksat class, Table 2.23s-6,
  Janssens 2006).
* `write_irr()` - write a `.IRR` file in any of the three FAO modes:
  specified events, generated schedule, or net irrigation requirement.

## Reading output

* `read_out()` - parse daily `*CROP.OUT`, `*CLIM.OUT`, `*WABAL.OUT`,
  and AquaCrop 7.3 combined `*PRMday.OUT`. Adds `Date` and `Run`.
* `list_outputs()` - list `.OUT` files in an `OUTP/` directory, omitting
  plugin status files `AllDone.OUT` and `ListProjectsLoaded.OUT`.

## Parameters and templating

* YAML `parameters:` list of scalar placeholders (`{name}`) in AquaCrop
  text files (`.CRO`, `.SOL`, `.PRM`, ...). Class `aquacropr_parameters`.
  Placeholders are `{name}` (single braces).
* `read_parameters()` / `as_parameters()` / `validate_parameters()` /
  `parameter_names()` / `parameter_defaults()` / `fill_values()` /
  `expand_names()` / `render_templates()`.

## Calibration

* `evaluate_candidate()` renders templates, runs the plugin, and scores
  one parameter vector.
* `calibrate()` wraps that in `DEoptim` (default), `BOBYQA` (`minqa`),
  `multi-BOBYQA` (several BOBYQA starts), `DDS` (Tolson & Shoemaker 2007,
  in-package), or `CMA-ES` (`cmaes`).
  Optional `names` fit a subset while the rest stay at defaults.

## Sensitivity analysis

* `sa_design()` / `run_sa_design()` / `complete_sa_analysis()` /
  `plot_sa()` / `suggest_calibrate_names()` — Morris and Sobol-Jansen.

## Designs and metamodels

* `generate_param_design()` / `extend_param_design()` - LHS, Dice LHS, or
  Sobol' training designs over `min`/`max`.
* `run_param_design()` / `read_param_design()` - one plugin run per design
  row; archive `.OUT` files as `run_<id>_<filename>`.
* `score_param_design()` / `fit_metamodel()` / `predict()` /
  `validate_metamodel()` - Kriging surrogate (`DiceKriging`).
* `calibrate_ego()` - Efficient Global Optimization (`DiceOptim`).
* `create_parameters()` - scaffold a starter `parameters.yaml`.

## Objectives

* `compute_generic_objective()`, `objective_spec()`,
  `composite_objective()`, `evaluate_objective()`, defaulting to
  `read_out()`.
