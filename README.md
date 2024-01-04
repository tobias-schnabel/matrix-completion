# matrix-completion
All work carried out for my Bachelor Thesis in the B.Sc. Econometrics & Operations Research at Maastricht University.

## Abstract
Athey et al. (2021) introduce the method of Matrix Completion Estimation from the Statistics and Computer Science literatures to Panel Data Econometrics. They show that it outperforms the canonical Difference-in-Differences estimator in a variety of panel data settings, including ones with differential timing of treatment adoption. They also highlight this new method’s reliance on only two identifying assumptions, SUTVA and exogeneity, which are both also needed for canonical DiD. This stands in contrast to the minimum of four additional necessary assumptions for canonical DiD. This thesis extends their work by comparing this new method not only to the canonical DiD estimator, but also to prominent methods developed in recent years to address the shortcomings of canonical DiD specifically in differential timing settings. I motivate and describe these estimators in a unified framework before comparing their performance in eight Monte Carlo simulations. My results show that the Matrix Completion Estimator shows promising potential in estimating treatment effects in such settings, but does not outperform all new DiD estimators in all eight simulations. I conclude that the Matrix Completion Estimator is preferable to canonical DiD in all settings I investigate, but not universally preferable to all recent DiD methods.

Interactive results (significantly updated since I submitted my thesis) available [here](Results-html.html)

Proposal: [Thesis_Proposal_Schnabel.pdf](https://github.com/tobias-schnabel/matrix-completion/files/11892374/Thesis_Proposal_Schnabel.pdf)

Final version of Thesis is [here](Thesis_Schnabel.pdf)

Poster: [Poster Schnabel.pdf](https://github.com/tobias-schnabel/matrix-completion/files/11892370/Poster.Schnabel.pdf)

To replicate, see instructions [here](Replication.md)

