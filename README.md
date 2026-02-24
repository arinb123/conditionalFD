## Summary ##
conditionalFD is an R package to identify valid front-door sets and their associated adjustment sets in directed acyclic graphs (DAGs).
Using DAGs defined via dagitty, the package enables researchers to specify exposure and outcome nodes and systematically search for front-door identification strategies.

## Why this package? ##
- Legible, explicit output: adjustment sets and mediators are printed in user-friendly format.
- Supports different adjustment set formats.
- Speed and accuracy as priorities.
- Provides information on how identification works, rather than simply indicating whether a DAG is identifiable.


## Example usage:

find_fd takes in parameters:

(required)
- `dag` (dagitty object) : the graph in which to find mediators and adjustment sets
- `X` (string) : the exposure node name
- `Y` (string) : the outcome node name

(optional)
- `verbose=TRUE` (boolean) : whether to print more detailed output
- `adj_type="minimal"` (string) : type of adjustment sets to find (see dagitty documentation for more)

find_fd outputs:
- printed information about mediators and adjustment sets
- returns an S3 object with attributes (as lists of lists of strings)
    - `adjustment_X_Z` : valid adjustment sets between the exposure and mediator(s)
    - `adjustment_Y_Z` : valid adjustment sets between the mediator(s) and outcome
    - `Z` : valid mediator nodes

#### INPUT:
```r
dag1 <- dagitty::dagitty("dag {
X -> M
M -> Y
X [pos=\"0,0\"]
  M [pos=\"1,0\"]
  Y [pos=\"2,0\"]
}")

find_fd(dag1, "X", "Y")
```
#### OUTPUT:
```
---- Adjustment I (block X <-> Z) ----
{M}: found 1 W set(s). Example: {}
---- Adjustment II (block Z <-> Y given X) ----
{M}: 1 T set(s). Example: {}
==== Final front-door solutions ====
Front-door | Adjustment I (X-M) | Adjustment II (M-Y)
        {M}                 {}                 {}
```
Explanation: M is a valid front-door between X and Y. There are no additional confounders to condition on for this front-door to work.
#### INPUT:
```r
dag2 <- dagitty::dagitty("dag {
A -> B
B -> C
U1 -> A
U1 -> B
U2 -> B
U2 -> C
U1 [pos=\"0,1\"]
  A  [pos=\"0,0\"]
  B  [pos=\"1,0.5\"]
  C  [pos=\"2,0\"]
  U2 [pos=\"2,1\"]
}")

find_fd(dag2, "A", "C", verbose=FALSE, adj_type="canonical")
```
#### OUTPUT:
```
B
---- Exposure-Mediator Adjustment (block X <-> M) ----
{B}: found 1 W set(s). Example: {U1,U2}
---- Mediator-Outcome Adjustment (block M <-> Y given X) ----
{B}: 1 T set(s). Example: {A,U1,U2}
==== Final front-door solutions ====
 Front-door |  Adjustment I (block X <-> Z) | Adjustment II (block Z <-> Y)
          {B}                         {U1,U2}                     {A,U1,U2}
                     
```
Explanation: {B} is a valid front-door set, but identification requires conditioning. {U1, U2} blocks all backdoors on A ~> B and {A, U1, U2} blocks backdoors on B ~> C conditional on A. If we ran with adj_type="minimal", we would have smaller adjustment sets: namely, {U1} and {U2}, respectively.

## Citations

Glynn, A. N., & Kashin, K. (2018). Front-door versus back-door adjustment with unmeasured confounding: Bias formulas for front-door and hybrid adjustments with application to a job training program.
https://doi.org/10.1080/01621459.2017.1398657

Pearl, J. (2009) _Causality: Models, Reasoning and Inference_. 2nd Edition, Cambridge University Press, Cambridge.
https://doi.org/10.1017/CBO9780511803161

Textor, J., van der Zander, B., Gilthorpe, M. S., Liśkiewicz, M., & Ellison, G. T. H. (2016). Robust causal inference using directed acyclic graphs: the R package 'dagitty'. _International Journal of Epidemiology, 45_(6), 1887–1894.
https://doi.org/10.1093/ije/dyw341

Thoemmes, F., & Kim, Y. (2023). Bias and Sensitivity Analyses for Linear Front-Door Models. _Methodology, 19_(3), Article e9205.
https://doi.org/10.5964/meth.9205

