### This Script carries out the main analysis
panel.matrices = function(panel, unit = 1, time = 2, outcome = 3, treatment = 4, treated.last = TRUE) {
  # TODO: add support for covariates X, i.e. could keep all other columns
  keep = c(unit, time, outcome, treatment)
  if (!all(keep %in% 1:ncol(panel) | keep %in% colnames(panel))) {
    stop("Column identifiers should be either integer or column names in `panel`.")
  }
  index.to.name = function(x) { if(x %in% 1:ncol(panel)) { colnames(panel)[x] } else { x } }
  unit = index.to.name(unit)
  time = index.to.name(time)
  outcome = index.to.name(outcome)
  treatment = index.to.name(treatment)
  keep = c(unit, time, outcome, treatment)
  
  panel = panel[keep]
  if (!is.data.frame(panel)){
    stop("Unsupported input type `panel.`")
  }
  if (anyNA(panel)) {
    stop("Missing values in `panel`.")
  }
  if (length(unique(panel[, treatment])) == 1) {
    stop("There is no variation in treatment status.")
  }
  if (!all(panel[, treatment] %in% c(0, 1))) {
    stop("The treatment status should be in 0 or 1.")
  }
  # Convert potential factor/date columns to character
  panel = data.frame(
    lapply(panel, function(col) {if (is.factor(col) || inherits(col, "Date")) as.character(col) else col}), stringsAsFactors = FALSE
  )
  val <- as.vector(table(panel[, unit], panel[, time]))
  if (!all(val == 1)) {
    stop("Input `panel` must be a balanced panel: it must have an observation for every unit at every time.")
  }
  
  panel = panel[order(panel[, unit], panel[, time]), ]
  num.years = length(unique(panel[, time]))
  num.units = length(unique(panel[, unit]))
  Y = matrix(panel[,outcome], num.units, num.years, byrow = TRUE,
             dimnames = list(unique(panel[,unit]), unique(panel[,time])))
  W = matrix(panel[,treatment], num.units, num.years, byrow = TRUE,
             dimnames = list(unique(panel[,unit]), unique(panel[,time])))
  w = apply(W, 1, any)                         # indicator for units that are treated at any time
  T0 = unname(which(apply(W, 2, any))[1]-1)    # last period nobody is treated
  N0 = sum(!w)
  
  if(! (all(W[!w,] == 0) && all(W[,1:T0] == 0) && all(W[w, (T0+1):ncol(Y)]==1))) {
    stop("The package cannot use this data. Treatment adoption is not simultaneous.")
  }
  
  unit.order = if(treated.last) { order(W[,T0+1], rownames(Y)) } else { 1:nrow(Y) }
  list(Y = Y[unit.order, ], N0 = N0, T0 = T0, W = W[unit.order, ])
}

# MC packages test
data(gsynth)
#data('california_prop99')
dat = simdata[, 1:4]


# system.time(
gsynthest = gsynth(Y ~ D, data = dat, estimator = "mc",
                     index = c("id","time"), force = "none", 
                     se = F, 
                     parallel = T)

fectest = fect(Y ~ D, data = dat, index = c("id","time"), force = "none", 
               se = F, method = "mc")
# )
plot(out, main = "Estimated ATT (MC)")


#setup = panel.matrices(dat)
M = t(gsynthest$Y.dat)
mask = rbind(t(gsynthest$D.tr), matrix(0, nrow = 45, ncol = ncol(t(gsynthest$D.tr))))

M2 = gsynthest$Y.dat
mask2 = cbind(gsynthest$D.tr, matrix(0, nrow = nrow(gsynthest$D.tr), ncol = 45))


MCPaneltest = mcnnm_cv(M, mask)
MCPaneltest2 = mcnnm_cv(M2, mask2)

min(gsynthest$MSPE)
(min(MCPaneltest$Avg_RMSE))^2

# for fixed lambda
mcpt2 = mcnnm_cv(M, mask, rel_tol = 1e-7)
gst2 = gsynth(Y ~ D, data = dat, estimator = "mc",
                     index = c("id","time"), force = "two-way", 
                     parallel = T, se = F, tol = 1e-7)

sqrt(gst2$MSPE)
mean((mcpt2$Avg_RMSE))

mcpL = mcpt2$L
gsynthL = gst2$Y.ct

head(mcpL[,21:30])
head(gsynthL)
