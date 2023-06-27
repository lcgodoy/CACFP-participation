## Project structure

The project contains two directories:

- `src` - contains the `R` scripts for reproducing the analyses in the paper;
- `log` - contains the outputs from the scripts in `src`.

The data used in the paper is not publicly available and therefore is omitted.

## `src` directory

There are six scripts in the `src` directory. The results produced by each
script are detailed below.

- The scripts `src/regions-model-full_reprex.R` and
  `src/regions-model-lowinc_reprex.R` generate the results
  presented,respectively, in the second and third column in Table 1.

- `src/lowinc-model_reprex.R` generates the results included in Table 2.

- `src/full-model_reprex.R` fits the same model fitted in the low-income areas
  to all the centers. That is, it also includes the centers not located in
  low-income areas. The output of this model is also used to produce one of the
  maps presented in the paper (could not find the map in the paper).
  
- `src/lowinc-capacity-model_reprex.R` fits the model to the low-income centers
  that also contain information on capacity.
  
- `src/split-maps_reprex.R` uses the results from the model to produce maps of
  adjusted participation rates.


