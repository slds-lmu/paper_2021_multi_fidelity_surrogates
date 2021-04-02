
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Surrogates for Multi-Fidelity Problems

This package contains several surrogates that approximate the
hyperparameter response surface for several interesting machine learing
algorithms across several tasks.

## Example

``` r
test = paradox::generate_design_random(ps, 1000)$data
predict_onnx(model, data = test, dicts = dicts)
```
