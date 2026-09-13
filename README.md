# aquacropr

R driver for the **FAO AquaCrop stand-alone (plugin)** executable: write
inputs, run the official binary, parse outputs, and score simulations
against observations.

This is not a reimplementation of AquaCrop.

## What you need

1. The FAO / KU Leuven **stand-alone plugin** (v7.x), e.g. `aquacrop.exe`
   from [KUL-RSDA/AquaCrop releases](https://github.com/KUL-RSDA/AquaCrop/releases).
2. The plugin’s **`SIMUL/`** folder (CO2 file, `*.PAR` defaults). The
   executable is launched with **no arguments**; it reads
   `LIST/ListProjects.txt` from the current working directory.
3. At least one working **project** (`.PRO` or `.PRM`) plus the crop, soil,
   climate, and other files it points at. Build one with `write_project()`
   or copy a GUI project and `relativize_project_paths()`.

You do not need to put the plugin *inside* this package. Point at it:

```r
library(aquacropr)
set_aquacrop_path("C:/path/to/aquacrop.exe")
set_simul_dir("C:/path/to/plugin/SIMUL")
```

Or set environment variables `AQUACROP_EXE` and `AQUACROP_SIMUL`.

## Typical run

```r
wd <- prepare_plugin_workdir(
  path = tempfile("ac-run-"),
  projects = "path/to/MyField.PRM",
  simul_dir = get_simul_dir()
)
# copy or write climate/crop/soil files to the directories named in the PRM
run_aquacrop(working_dir = wd)
crop <- read_out(file.path(wd, "OUTP", "MyFieldCROP.OUT"))
```

## Status

Driver, writers, output reader, YAML parameters, objectives, `calibrate()`
(`DEoptim`, `BOBYQA`, `multi-BOBYQA`, `DDS`, `CMA-ES`) and `calibrate_ego()`, Morris / Sobol, and space-filling
metamodel designs (`generate_param_design()`, `run_param_design()`,
`fit_metamodel()`).

AquaCrop itself remains FAO’s model; see
<https://www.fao.org/aquacrop/>.
