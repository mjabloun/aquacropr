# Notes for working on aquacropr

## Feature tracking

Update `NEWS.md` when a feature is added or changed.

## Engine

This package wraps the FAO AquaCrop **stand-alone plugin** (Fortran
`aquacrop` / `aquacrop.exe` from v7.0 onward). It does **not** reimplement
the model.

The executable takes **no project-file argument**. From a working directory
it expects:

- `LIST/ListProjects.txt` — one `.PRO`/`.PRM` basename per line
- `LIST/<project>.PRM` (or `.PRO`)
- `SIMUL/` — copied from the plugin install (MaunaLoa.CO2, `*.PAR`, ...)
- `OUTP/` — created empty; results land here as `<project>CROP.OUT`, etc.

Do not vendor FAO `SIMUL/` files into the package.

Project files: `write_project()` emits FAO Table 2.23w-1 layout
(description, version, then per-run dates + 14 file triplets). `.PRO` is
one simulation; `.PRM` is successive seasons. Optional files are `(None)`.
Program parameters live in a sibling `.PP1`/`.PPn` (`write_pp()`), not
inside the project file. `run_aquacrop(program_parameters = FALSE)` hides
those siblings for one run.

Soil files: `write_sol()` writes AquaCrop 7.x `.SOL` (CN, REW, up to
five horizons with CRa/CRb). Defaults match Fortran `DetermineCN_default`,
Table 2.23s-6 (REW), and `DetermineParametersCR`.

Irrigation: `write_irr()` writes Table 2.23q `.IRR` (events / generate /
net). Rainfed remains `(None)` in the project file.

Parameters: YAML with a top-level `parameters:` key.
Class `aquacropr_parameters`. Calibration, SA, and metamodel designs take
that object. `create_parameters()` scaffolds a starter YAML.

## Verification

Unit tests must not require `aquacrop.exe`. Live plugin runs are manual,
once the user has shared a plugin path and a known-good project.
