# BIC-LP

## BIC-LP is a hybrid dynamic Bayesian network score function for Gene Regulatory Network(GRN) reconstruction.

GRNs are probabilistic and gene expression data contains data noise, which brings great challenges for the reverse engineering of GRN reconstruction. Dynamic Bayesian network is a excellent model for GRN reconstruction. However, using simple Bayesian-based score function always leads to many false positive/negetive edges. In others words, the score of the gold standard network is high but not the highest among any possible network. That is to say the score functions could be improved to make the gold standard network scores the highest as far as possible. Thus, the terms based on Linear regression and correlation are integrated into the BIC score function here, yielding BIC-LP score function.

Experimental results on both synthetic and real-world datasets show that BIC-LP can reasonably eliminate some false positive edges while retaining most true positive edges, so as to achieve better GRN reconstruction performance. 

## Data Preparation

Before conducting BIC-LP, you need to generate two types of coefficients, Lasso linear Regrassion coefficients (LRC) and Pearson's correlation coefficients (PCC). Current version of BIC-LP supports only second-order Markov. Thus, the size LRC matrix should be 2*p × p. The element _e<sub>ij </sub>_ means the relationship between _i_ and _j_, where _i_ comes from _t_-1 or _t_-2 time stamp and _j_ comes from the time stamp of _t_. And you need to switch the non-zero elements in _M<sub>LRC </sub>_ to range(0.5, 1) via any tool. 

Then calculate the PCC of genes within the same time stamp. 
> B = corrcoef(gene_expression_data)   % MATLAB

As the input file of BIC-LP, "10-1.xlsx" contains 4 sheets:

&nbsp;&nbsp;&nbsp;&nbsp;Sheet1: time-series gene expression data

&nbsp;&nbsp;&nbsp;&nbsp;Sheet2: gold standard network

&nbsp;&nbsp;&nbsp;&nbsp;Sheet3: M<sub>LRC </sub> (Switched)

&nbsp;&nbsp;&nbsp;&nbsp;Sheet4: PCC 

## Run

Run demo.R to search the best-fit HO-DBN, thereby searching the best-fit GRN. SA is applied in BIC-LP, since it is proved to infinitely close to the optimal solution given perfect condition. Thus, some parameters in simulated annealing needs changing if the number of genes of your dataset is too small or large. It should be noticed that the BIC score implemented by 'bnlearn' is a little different from the widely accepted one, because the larger bic means the fitter as is maden clear in the document of 'bnlearn'. Do not misunderstand our codes.

## Contact
If you have any question, don't hesitate to contact us: 

*Prof.* Wang. wangzq@bmie.neu.edu.cn\
*Prof.* Xin. xinjunchang@mail.neu.edu.cn\
or *Mr.* Wang mingcan_wl@163.com
