## Summary ##
conditionalFD is an R package to identify valid front-door sets and their corresponding adjustment sets in a directed acyclic graph (DAG).
It operates on DAGs defined using dagitty, and takes in exposure/outcome node names as strings.

## Why this package? ##
- Direct integration with dagitty, as opposed to igraph or other DAG libraries.
- Legible, explicit output: adjustment sets and mediators are printed in user-friendly format.
- Supports different adjustment set formats.
- Speed and accuracy as priorities.


### Example usage:

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
---- Adjustment A (block X <-> Z) ----
{M}: found 1 W set(s). Example: {}
---- Adjustment B (block Z <-> Y given X) ----
{M}: 1 T set(s). Example: {}
==== Final front-door solutions ====
 Front-door Adjustment A (X-Z) Adjustment B (Z-Y)
        {M}                 {}                 {}
```
Explanation: M is a valid front-door between X and Y. There are no additional confounders to condition on for this FD to work.
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
==== Final front-door solutions ====
 Front-door Adjustment A (X-Z) Adjustment B (Z-Y)
        {B}            {U1,U2}          {A,U1,U2}
```
Explanation: {B} is a valid front-door set, but identification requires conditioning. {U1, U2} blocks all backdoors on A ~> B and {A, U1, U2} blocks backdoors on B ~> C conditional on A. If we ran with adj_type="minimal", we would have smaller adjustment sets: namely, {U1} and {U2}, respectively.

## Citations ##
Glynn, A. N., & Kashin, K. (2018). Front-door versus back-door adjustment with unmeasured confounding: Bias formulas for front-door and hybrid adjustments with application to a job training program. https://doi.org/10.1080/01621459.2017.1398657
Pearl, J. (2009) Causality: Models, Reasoning and Inference. 2nd Edition, Cambridge University Press, Cambridge. https://doi.org/10.1017/CBO9780511803161
Textor, J., van der Zander, B., Gilthorpe, M. S., Liśkiewicz, M., & Ellison, G. T. H. (2016). Robust causal inference using directed acyclic graphs: the R package 'dagitty'. International Journal of Epidemiology, 45(6), 1887–1894. https://doi.org/10.1093/ije/dyw341
Thoemmes, F., & Kim, Y. (2023). Bias and Sensitivity Analyses for Linear Front-Door Models. Methodology, 19(3), Article e9205. https://doi.org/10.5964/meth.9205

