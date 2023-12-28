# BIC-LP

## BIC-LP is a hybrid dynamic Bayesian network score function for Gene Regulatory Network(GRN) reconstruction.

GRNs are probabilistic and gene expression data contains data noise, which brings great challenges for the reverse engineering of GRN reconstruction. Dynamic Bayesian network is a excellent model for GRN reconstruction. However, using simple Bayesian-based score function always leads to many false positive/negetive edges. In others words, the score of the gold standard network is high but not the highest among any possible network. That is to say the score functions could be improved to make the gold standard network scores the highest as far as possible. Thus, the terms based on Linear regression and correlation are integrated into the BIC score function here, yielding BIC-LP score function.

Experimental results on both synthetic and real-world datasets show that BIC-LP can reasonably eliminate some false positive edges while retaining most true positive edges, so as to achieve better GRN reconstruction performance. 

## Data Preparation

Before conducting BIC-LP, you need to generate two types of coefficients, Lasso linear Regrassion coefficients (LRC) and Pearson's correlation coefficients (PCC). Current version of BIC-LP supports only second-order Markov. Thus, the size LRC matrix should be 2*p × p. The element e<sub>ij<\sub> means the relationship between _i_ and _j_, where _i_ comes from _t_-1 or _t_-2 time stamp and _j_ comes from the time stamp of _t_. And you need to switch the non-zero elements in M<sub>LRC<\sub> to range(0.5, 1) via any tool you like. 

## Contact
If you have any question, don't hesitate to contact us: 

*Prof.* Wang. wangzq@bmie.neu.edu.cn\
*Prof.* Xin. xinjunchang@mail.neu.edu.cn\
or *Mr.* Wang mingcan_wl@163.com
