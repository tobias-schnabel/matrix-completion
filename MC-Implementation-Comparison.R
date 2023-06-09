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

print(sqrt(gst2$MSPE))
print(mean((mcpt2$Avg_RMSE)))

# Gsynth has lower RMSE