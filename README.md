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

Example of Estimating Entropy Rate using `ccber`
------------------------------------------------

Consider the following transition matrix of a first-order Markov chain
with three states,

$P = \\left(\\begin{array}{ccc} 0.2 & 0.3 & 0.5 \\\\ 0.7 & 0.1 & 0.2 \\\\ 0.2 & 0.2 & 0.6 \\end{array}\\right)$

    P = matrix(c(0.2, 0.3, 0.5, 
                 0.7, 0.1, 0.2,
                 0.2, 0.2, 0.6), 3,3, byrow = T)
