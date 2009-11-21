#mask functions for ****standard**** generics
print = function (...) base::print (...)
format = function (...) base::format (...)
summary = function (...) base::summary (...)
plot = function (...) graphics::plot (...)
lines = function (...) graphics::lines (...)
predict = function (...) stats::predict (...)
fitted = function (...) stats::fitted (...)
residuals = function (...) stats::residuals (...)
weights = function (...) stats::weights (...)
as.list = function (...) base::as.list (...)
as.data.frame = function (...) base::as.data.frame (...)
mean = function (...) base::mean (...)

#not sure what is happening with length

#new generics
map = function (...) UseMethod ("map")
fit = function (...) UseMethod ("fit")
evaluate = function (...) UseMethod ("evaluate")
reset = function (...) UseMethod ("reset")
clone = function (...) UseMethod ("clone")
mutate = function (...) UseMethod ("mutate")
gf = function (...) UseMethod ("gf")
as.r = function (...) UseMethod ("as.r")
as.latex = function (...) UseMethod ("as.latex")

