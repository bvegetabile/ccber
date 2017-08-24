`ccber`: an `R` Package for the Estimation of Behavioral Entropy Rate - Developed for the Conte Center @ UCI
============================================================================================================

See reference: *Early Life Exposure to Unpredictable Maternal Sensory
Signals Influences Cognitive Development: A Cross-Species Approach*
Elysia Davis, Stephanie Stour, Jenny Molet, Brian Vegetabile, Laura
Glynn, Curt Sandman, Kevin Heins, Hal Stern, and Tallie Baram: To appear
in the Proceedings of the National Academy of Sciences

Installing `ccber`
------------------

The package `devtools` is required to install this `R` package from this
Github repository. Install this package first if it is not already
installed.

    install.packages('devtools', dependencies = TRUE)

Once that package has been installed, use the following to install
`ccber`

    devtools::install_github('bvegetabile/ccber')

Load the package to begin analysis!

    library('ccber')

Using `ccber` with Observer Files
---------------------------------

A Longer Example of How to Estimate Entropy Rate using `ccber`
--------------------------------------------------------------

Consider the following transition matrix of a first-order Markov chain
with three states,

    P = matrix(c(0.2, 0.3, 0.5, 
                 0.7, 0.1, 0.2,
                 0.2, 0.2, 0.6), 3,3, byrow = T)

We can simulate from a Markov process with this using the function
`SimulateMarkovChain`

    mc_chain <- SimulateMarkovChain(trans_mat = P, n_sims = 5000)
    head(mc_chain, n = 20)

    ##  [1] 3 3 3 3 1 3 3 3 1 1 3 3 2 3 3 1 1 3 3 3

From this we can calculate a matrix of transition counts

    tc <- CalcTransitionCounts(mc_chain)
    tc

    ##      [,1] [,2] [,3]
    ## [1,]  311  494  742
    ## [2,]  773   90  210
    ## [3,]  463  489 1427

And then estimate a transition matrix,

    tm <- CalcTransitionMatrix(tc)
    tm

    ##           [,1]       [,2]      [,3]
    ## [1,] 0.2010343 0.31932773 0.4796380
    ## [2,] 0.7204101 0.08387698 0.1957130
    ## [3,] 0.1946196 0.20554855 0.5998319

which agrees fairly well with the true `P`. Additionally we can estimate
the stationary distribution of the process in a one of two ways. The
first way is an empirical estimate from the observed sequence.

    emp_sm <- CalcEmpiricalStationary(mc_chain, state_space = 1:3)
    emp_sm

    ##        [,1]   [,2]  [,3]
    ## [1,] 0.3094 0.2146 0.476

The second way is an eigendecomposition of the observed transition
matrix, though the preferred method is through the empirical estimation
procedure.

    eig_sm <- CalcEigenStationary(tm)
    eig_sm

    ## [1] 0.3094619 0.2146429 0.4758952

Using both the stationary distribution estimate and the estimate of the
transition matrix, the entropy rate of the process can be estimated
using the following commands

    entrate1 <- CalcMarkovEntropyRate(tm, emp_sm)
    entrate1

    ## [1] 1.352909

    entrate2 <- CalcMarkovEntropyRate(tm, eig_sm)
    entrate2

    ## [1] 1.352906

Both of these values agree very well with the true entropy rate,

    true_entropy_rate <- CalcMarkovEntropyRate(P, CalcEigenStationary(P))
    true_entropy_rate

    ## [1] 1.360979

A Shorter Example
-----------------

If the method of estimation for the stationary distrbution is known, a
more simple function is provided to estimate the entropy rate as well.

Both of these values agree very well with the true entropy rate,

    quicker_estimate <- CalcEntropyRate(mc_chain, 
                                        state_space = 1:3, 
                                        stat_method = "Empirical")
    quicker_estimate

    ## [1] 1.352909
