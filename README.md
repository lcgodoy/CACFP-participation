## Project structure

The project contains two directories:

- `code` - contains the `R` scripts for reproducing the analyses in the paper;
- `log` - contains the outputs from the scripts in `code`.

The data used in the paper is not publicly available and therefore is omitted.

## `code` directory

There are six scripts in the `code` directory. The results produced by each
script are detailed below.

- The scripts `code/regions-model-full.R` and `code/regions-model-lowinc.R`
generate the results presented,respectively, in the second and third column in
Table 1.

- `code/lowinc-model.R` generates the results included in Table 2.

- `code/full-model.R` fits the same model fitted in the low-income areas to all
  the centers. That is, it also includes the centers not located in low-income
  areas. The output of this model is also used to produce one of the maps
  presented in the paper (could not find the map in the paper).
  
- `code/lowinc-capacity-model.R` fits the model to the low-income centers that
  also contain information on capacity.
  
- `code/split-maps.R` uses the results from the model to produce maps of
  adjusted participation rates.


