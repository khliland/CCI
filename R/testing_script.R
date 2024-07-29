# Testing script.
# This script is for running code bits and chunks, feel free to overwrite as you see fit. 
library(dagitty)
library(xgboost)
dag <- dagitty('dag {
bb="-3.56,-3.795,5.103,4.955"
j [pos="-2.168,-1.552"]
v [pos="-1.153,-3.000"]
w [pos="-0.262,-2.952"]
x [pos="-2.162,-3.450"]
y [pos="-3.102,-2.913"]
z [pos="-2.162,-2.434"]
j -> z
v -> x
v -> z
w -> v
x -> y
z -> y
}
')
plot(dag)
class(dag)


