
- <a href="#rifreg-estimate-recentered-influence-function-regressions"
  id="toc-rifreg-estimate-recentered-influence-function-regressions"><code>rifreg</code>:
  Estimate recentered influence function regressions</a>
  - <a href="#overview" id="toc-overview">Overview</a>
  - <a href="#installation" id="toc-installation">Installation</a>
  - <a href="#background" id="toc-background">Background</a>
    - <a href="#unconditional-partial-effects"
      id="toc-unconditional-partial-effects">Unconditional partial effects</a>
    - <a href="#estimation" id="toc-estimation">Estimation</a>
    - <a href="#inference" id="toc-inference">Inference</a>
  - <a href="#example" id="toc-example">Example</a>
    - <a href="#unconditional-quantile-regressions"
      id="toc-unconditional-quantile-regressions">Unconditional quantile
      regressions</a>
    - <a href="#bootstrapping-standard-errors"
      id="toc-bootstrapping-standard-errors">Bootstrapping standard errors</a>
    - <a href="#other-distributional-statistics"
      id="toc-other-distributional-statistics">Other distributional
      statistics</a>
    - <a href="#user-written-rif-functions"
      id="toc-user-written-rif-functions">User-written RIF functions</a>
  - <a href="#validation" id="toc-validation">Validation</a>
    - <a href="#data-preparation" id="toc-data-preparation">Data
      preparation</a>
    - <a href="#rif-regression" id="toc-rif-regression">RIF Regression</a>
    - <a href="#results" id="toc-results">Results</a>
  - <a href="#credits" id="toc-credits">Credits</a>
  - <a href="#references" id="toc-references">References</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rifreg`: Estimate recentered influence function regressions

## Overview

RIF regressions estimate the effects of small location shifts in
continuous explanatory variables on a distributional statistic (e.g.,
quantile, variance or Gini coefficient) of an outcome variable as
proposed by [Firpo, Fortin, and Lemieux
(2009a)](https://doi.org/10.3982/ECTA6822). In the special case of
quantiles, RIF regressions are ‘unconditional quantile regressions’
capturing the association between explanatory variables and quantiles of
the marginal distribution of an outcome variable.

## Installation

You can either install the CRAN version of `rifreg`

``` r
install.packages("rifreg")
```

or the latest development version from GitHub:

``` r
devtools::install_github("samumei/rifreg")
```

## Background

### Unconditional partial effects

Firpo, Fortin, and Lemieux (2009a) propose recentred influence function
(RIF) regressions to estimate unconditional partial effects (UPE), i.e.,
the effects of small locations shifts in a continuous explanatory
variable $X$ on a distributional statistic $\nu=\nu(F_Y)$ of an outcome
variable $Y$.

An influence function, as concept from robust statistics, informs about
the extent a statistic of an empirical marginal distribution changes due
to increasing the probability mass at value $y$ by a small amount. A
recentred influence functions is defined as the influence function
recentred around the original distributional statistic:
$$\text{RIF}(y;\nu,F_Y)= \nu(F_Y)+ \text{IF}(y;\nu,F_Y)$$

The expected value of a recentred influence function equals the original
distributional statistic. Firpo et al. apply the law of iterated
expectations to reformulate the distributional statistic in terms of the
conditional expecations of the explanatory variables:
$$\nu(F_Y) = \int \text{RIF}(y;\nu,F_Y) dF_Y=  \int E(\text{RIF}(Y;\nu,F_Y)|X=x) dF_X(x)$$

This allows them to express the unconditional partial effects as average
derivatives
$$\alpha(\nu) = \int \frac{d E(\text{RIF}(Y;\nu,F_Y)|X=x)}{dx} dF_X(x).$$

### Estimation

Firpo et al. propose to approximate the conditional expectation of the
RIF given the explanatory variables with a linear regression. The
regression coefficients can be consistent estimates of the average
derivatives $\widehat{\alpha}(\nu)$ if the conditional expectations of
the RIF are linear in $X$ (see Firpo et al., 2009, [Rothe 2015:
328](https://doi.org/10.1080/07350015.2014.948959)).

`rifreg` implements this approach. It first calculates the RIF of the
outcome variable $Y$ and a distributional statistic of interest $\nu$.
Then, it runs an OLS regression of the transformed outcome variable $Y$
on the explanatory variables $X$.

By default, the RIF of quantiles, the mean, the variance, the Gini
coefficient, the interquantile range and the quantile ratio are
available in `rifreg`. Moreover, it allows to calculate the RIF for
additional statistics with user-written functions (see example below).
[Cowell and Flachaire
(2007)](https://doi.org/10.1016/j.jeconom.2007.01.001), [Essama-Nssah &
Lambert (2012)](https://doi.org/10.1108/S1049-2585(2012)0000020009), and
[Rios-Avila (2020)](https://doi.org/10.1177/1536867X20909690) derive the
influence funtions for an array of distributional statistics.

For the sake of illustration, consider the RIF of a quantile
$q_\tau = \inf_q \{q: F_Y(q) \geq \tau\}$. It is defined as
$$\text{RIF}(y;q_\tau,F_Y) = q_\tau  + \frac{\tau-1\{y \leq q_\tau\}}{f_Y(q_\tau)}, $$
where $1\{\}$ is an indicator function and $f_Y(q_\tau)$ is the density
at the quantile of interest. Thus, calculating the RIF requires
estimating the sample quantile and the kernel density. The regression in
the second step essentially amounts to a linear probability model (see
Firpo et al., 2009a: 958, 961).

### Inference

`rifreg` allows to bootstrap standard errors. Analytical standard errors
can be nontrivial when the RIF introduces an additional estimation step.
In particular, this is the case for quantiles where the density has to
be estimated (see [Firpo, Fortin, and Lemieux,
2009b](https://www.econometricsociety.org/publications/econometrica/2009/05/01/unconditional-quantile-regressions/supp/6822_extensions_0.pdf)).

Per default, `rifreg` returns heteroscedasticity-consistent standard
errors estimated by `sandwich::sandwich()` if the variance is not
bootstrapped. Note, however, that these standard errors do not take the
variance introduced by the RIF estimation step into account.

## Example

In this basic example, we use a sample of the male wage data from the
Current Population Survey from 1983 to 1985 as used in Firpo et
al. (2009a).

``` r
library(rifreg)
#> Loading required package: ggplot2
data("men8385")
```

### Unconditional quantile regressions

We are interested in the unconditional quantile partial effects (UQPE)
of union membership on log hourly wages. We therefore estimate RIF
regressions on union status and control for demographic characteristics.
The parameter `statistic` specifies quantiles as our distributional
statistic of interest, while `probs` defines the probabilities of the
quantiles.

``` r
ffl_model <- log(wage) ~ union + nonwhite + married + education + experience
fit_uqr <- rifreg(ffl_model,
                  data=men8385,
                  weights=weights,
                  statistic="quantiles",
                  probs=1:9/10)
```

The`summary` method returns the estimated coefficients and the standard
errors. Per default, `summary` returns heteroscedasticity-consistent
standard errors estimated if the variance is not bootstrapped.

``` r
summary(fit_uqr)
#> 
#> WARNING: Standard errors have not been bootstrapped!
#>          Analytical s.e. do not take variance introduced by
#>          estimating the RIF into account.
#> 
#> RIF regression coefficients for quantile_0.1
#> 
#>                            Estimate Analytical s.e. Lower bound   Upper bound
#> (Intercept)             0.970857088     0.012185135  0.94697422  0.9947399540
#> unionyes                0.198602743     0.005980177  0.18688160  0.2103238913
#> nonwhiteyes            -0.119114400     0.013371549 -0.14532264 -0.0929061640
#> marriedyes              0.201798293     0.008591823  0.18495832  0.2186382667
#> educationElementary    -0.330658130     0.019756304 -0.36938049 -0.2919357750
#> educationHS dropout    -0.366434292     0.014826154 -0.39549355 -0.3373750302
#> educationSome College   0.049659609     0.009615449  0.03081333  0.0685058899
#> educationCollege        0.201144576     0.008633975  0.18422199  0.2180671664
#> educationPost-graduate  0.141083238     0.008983631  0.12347532  0.1586911543
#> experience0-4          -0.567399813     0.016267265 -0.59928365 -0.5355159749
#> experience5-9          -0.083794606     0.011736915 -0.10679896 -0.0607902529
#> experience10-14        -0.032102547     0.010573264 -0.05282614 -0.0113789501
#> experience15-19        -0.021649604     0.010818365 -0.04285360 -0.0004456077
#> experience25-29         0.004401242     0.011690929 -0.01851298  0.0273154629
#> experience30-34        -0.004767943     0.012481078 -0.02923086  0.0196949709
#> experience35-39        -0.003914586     0.014313694 -0.03196943  0.0241402543
#> experience>=40          0.064694355     0.015639508  0.03404092  0.0953477901
#> 
#> Residual standard error: 57.46236 on 53374 degrees of freedom
#> Multiple R-squared: 0.1918716,   Adjusted R-squared: 0.1916294
#> 
#> 
#> RIF regression coefficients for quantile_0.2
#> 
#>                            Estimate Analytical s.e. Lower bound Upper bound
#> (Intercept)             1.179109721     0.013026531  1.15357772  1.20464172
#> unionyes                0.298897702     0.006616377  0.28592960  0.31186580
#> nonwhiteyes            -0.169377662     0.012585680 -0.19404559 -0.14470973
#> marriedyes              0.235994063     0.008950381  0.21845132  0.25353681
#> educationElementary    -0.458475630     0.018754756 -0.49523495 -0.42171631
#> educationHS dropout    -0.314803541     0.012626671 -0.33955182 -0.29005527
#> educationSome College   0.110356109     0.009402963  0.09192630  0.12878592
#> educationCollege        0.328957233     0.009469352  0.31039730  0.34751716
#> educationPost-graduate  0.278262795     0.009802209  0.25905047  0.29747512
#> experience0-4          -0.675621784     0.015427400 -0.70585949 -0.64538408
#> experience5-9          -0.159418282     0.012898422 -0.18469919 -0.13413737
#> experience10-14        -0.076472362     0.011967879 -0.09992940 -0.05301532
#> experience15-19        -0.036089249     0.012121088 -0.05984658 -0.01233192
#> experience25-29         0.003212986     0.013185279 -0.02263016  0.02905613
#> experience30-34        -0.006481069     0.013864995 -0.03365646  0.02069432
#> experience35-39         0.006038244     0.015103569 -0.02356475  0.03564124
#> experience>=40          0.057780569     0.016622467  0.02520053  0.09036060
#> 
#> Residual standard error: 55.49563 on 53374 degrees of freedom
#> Multiple R-squared: 0.2720633,   Adjusted R-squared: 0.2718451
#> 
#> 
#> RIF regression coefficients for quantile_0.3
#> 
#>                            Estimate Analytical s.e.  Lower bound Upper bound
#> (Intercept)             1.358386467     0.014343257  1.330273683  1.38649925
#> unionyes                0.386994339     0.007679055  0.371943392  0.40204529
#> nonwhiteyes            -0.213310984     0.012671208 -0.238146551 -0.18847542
#> marriedyes              0.252969436     0.009567014  0.234218088  0.27172078
#> educationElementary    -0.541317352     0.018019345 -0.576635269 -0.50599944
#> educationHS dropout    -0.278251916     0.012285724 -0.302331936 -0.25417190
#> educationSome College   0.151998138     0.010097314  0.132207403  0.17178887
#> educationCollege        0.446340964     0.010723221  0.425323451  0.46735848
#> educationPost-graduate  0.411585674     0.011219227  0.389595989  0.43357536
#> experience0-4          -0.744535859     0.015732749 -0.775372048 -0.71369967
#> experience5-9          -0.253447553     0.014388718 -0.281649440 -0.22524567
#> experience10-14        -0.116601430     0.013662899 -0.143380713 -0.08982215
#> experience15-19        -0.045319698     0.013925997 -0.072614652 -0.01802474
#> experience25-29         0.008231836     0.015155645 -0.021473228  0.03793690
#> experience30-34         0.002965612     0.015764638 -0.027933079  0.03386430
#> experience35-39         0.019524965     0.017097294 -0.013985731  0.05303566
#> experience>=40          0.034726270     0.018548691 -0.001629164  0.07108171
#> 
#> Residual standard error: 58.49942 on 53374 degrees of freedom
#> Multiple R-squared: 0.3062559,   Adjusted R-squared: 0.3060479
#> 
#> 
#> RIF regression coefficients for quantile_0.4
#> 
#>                            Estimate Analytical s.e.  Lower bound Upper bound
#> (Intercept)             1.576432103     0.013525123  1.549922862  1.60294134
#> unionyes                0.364913513     0.007587787  0.350041451  0.37978558
#> nonwhiteyes            -0.199584659     0.010978696 -0.221102902 -0.17806641
#> marriedyes              0.194950211     0.008611974  0.178070743  0.21182968
#> educationElementary    -0.516700583     0.015102973 -0.546302410 -0.48709876
#> educationHS dropout    -0.231527526     0.010646419 -0.252394507 -0.21066055
#> educationSome College   0.173841919     0.009235893  0.155739568  0.19194427
#> educationCollege        0.457356514     0.010411410  0.436950152  0.47776288
#> educationPost-graduate  0.461351523     0.010776224  0.440230123  0.48247292
#> experience0-4          -0.687106540     0.014067078 -0.714678012 -0.65953507
#> experience5-9          -0.307969066     0.013598063 -0.334621270 -0.28131686
#> experience10-14        -0.152827202     0.013262115 -0.178820947 -0.12683346
#> experience15-19        -0.071870567     0.013576400 -0.098480311 -0.04526082
#> experience25-29         0.002968522     0.014840414 -0.026118689  0.03205573
#> experience30-34         0.012083234     0.015256236 -0.017818988  0.04198546
#> experience35-39         0.022326253     0.016415231 -0.009847601  0.05450011
#> experience>=40          0.001684911     0.017372323 -0.032364842  0.03573466
#> 
#> Residual standard error: 53.72022 on 53374 degrees of freedom
#> Multiple R-squared: 0.3073782,   Adjusted R-squared: 0.3071706
#> 
#> 
#> RIF regression coefficients for quantile_0.5
#> 
#>                           Estimate Analytical s.e.  Lower bound Upper bound
#> (Intercept)             1.74646597     0.014131803  1.718767631  1.77416430
#> unionyes                0.34030574     0.008126569  0.324377663  0.35623381
#> nonwhiteyes            -0.19077337     0.010743837 -0.211831293 -0.16971545
#> marriedyes              0.15861935     0.008568522  0.141825044  0.17541365
#> educationElementary    -0.50630503     0.013837627 -0.533426780 -0.47918328
#> educationHS dropout    -0.20853794     0.010243141 -0.228614492 -0.18846138
#> educationSome College   0.19289983     0.009412240  0.174451842  0.21134782
#> educationCollege        0.49512703     0.011015354  0.473536937  0.51671713
#> educationPost-graduate  0.53302843     0.011633145  0.510227462  0.55582939
#> experience0-4          -0.64823661     0.014293317 -0.676251514 -0.62022171
#> experience5-9          -0.36149797     0.014211845 -0.389353184 -0.33364275
#> experience10-14        -0.18921874     0.014172573 -0.216996980 -0.16144049
#> experience15-19        -0.06551663     0.014612406 -0.094156942 -0.03687631
#> experience25-29         0.01863508     0.016045649 -0.012814391  0.05008455
#> experience30-34         0.03899880     0.016386416  0.006881428  0.07111618
#> experience35-39         0.03524896     0.017457410  0.001032436  0.06946548
#> experience>=40          0.01020244     0.017893229 -0.024868292  0.04527317
#> 
#> Residual standard error: 54.79901 on 53374 degrees of freedom
#> Multiple R-squared: 0.2875822,   Adjusted R-squared: 0.2873686
#> 
#> 
#> RIF regression coefficients for quantile_0.6
#> 
#>                           Estimate Analytical s.e.  Lower bound Upper bound
#> (Intercept)             1.90337416     0.012027699  1.879799870  1.92694845
#> unionyes                0.22422100     0.007019567  0.210462651  0.23797935
#> nonwhiteyes            -0.15140862     0.008577199 -0.168219931 -0.13459731
#> marriedyes              0.11051797     0.006969412  0.096857921  0.12417802
#> educationElementary    -0.37539155     0.010607297 -0.396181851 -0.35460125
#> educationHS dropout    -0.14443228     0.008044627 -0.160199752 -0.12866482
#> educationSome College   0.15746081     0.007896764  0.141983156  0.17293847
#> educationCollege        0.42651001     0.009549651  0.407792695  0.44522733
#> educationPost-graduate  0.48655626     0.010366426  0.466238063  0.50687445
#> experience0-4          -0.47173107     0.011971475 -0.495195156 -0.44826698
#> experience5-9          -0.29791375     0.012039594 -0.321511351 -0.27431614
#> experience10-14        -0.16379394     0.012255093 -0.187813924 -0.13977396
#> experience15-19        -0.04962905     0.012910688 -0.074933997 -0.02432410
#> experience25-29         0.03043321     0.014109064  0.002779443  0.05808697
#> experience30-34         0.04962817     0.014431421  0.021342589  0.07791376
#> experience35-39         0.04742285     0.015359492  0.017318246  0.07752745
#> experience>=40          0.00867607     0.015084566 -0.020889680  0.03824182
#> 
#> Residual standard error: 45.81622 on 53374 degrees of freedom
#> Multiple R-squared: 0.2540804,   Adjusted R-squared: 0.2538568
#> 
#> 
#> RIF regression coefficients for quantile_0.7
#> 
#>                           Estimate Analytical s.e.  Lower bound Upper bound
#> (Intercept)             2.08335156     0.012503679  2.058844349  2.10785877
#> unionyes                0.13231888     0.007288707  0.118033014  0.14660474
#> nonwhiteyes            -0.12267681     0.008396715 -0.139134374 -0.10621925
#> marriedyes              0.07976414     0.006977978  0.066087308  0.09344098
#> educationElementary    -0.32376628     0.009921338 -0.343212099 -0.30432046
#> educationHS dropout    -0.12677063     0.007654939 -0.141774316 -0.11176695
#> educationSome College   0.15809893     0.008073949  0.142273993  0.17392387
#> educationCollege        0.44437130     0.010253779  0.424273889  0.46446870
#> educationPost-graduate  0.54341643     0.011408993  0.521054807  0.56577806
#> experience0-4          -0.43860774     0.012350911 -0.462815530 -0.41439996
#> experience5-9          -0.31332993     0.012465294 -0.337761907 -0.28889796
#> experience10-14        -0.19495274     0.012857646 -0.220153731 -0.16975176
#> experience15-19        -0.07479452     0.013724751 -0.101695036 -0.04789401
#> experience25-29         0.02868761     0.015110181 -0.000928344  0.05830357
#> experience30-34         0.04377733     0.015491815  0.013413375  0.07414129
#> experience35-39         0.02694454     0.016277778 -0.004959900  0.05884899
#> experience>=40         -0.01456717     0.015470875 -0.044890090  0.01575574
#> 
#> Residual standard error: 47.0271 on 53374 degrees of freedom
#> Multiple R-squared: 0.2169289,   Adjusted R-squared: 0.2166942
#> 
#> 
#> RIF regression coefficients for quantile_0.8
#> 
#>                            Estimate Analytical s.e.  Lower bound Upper bound
#> (Intercept)             2.243138267     0.013164736  2.217335383  2.26894115
#> unionyes                0.008239871     0.007411720 -0.006287101  0.02276684
#> nonwhiteyes            -0.112884094     0.008231296 -0.129017434 -0.09675076
#> marriedyes              0.057116207     0.007164746  0.043073305  0.07115911
#> educationElementary    -0.256429944     0.009600742 -0.275247398 -0.23761249
#> educationHS dropout    -0.102227401     0.007096387 -0.116136320 -0.08831848
#> educationSome College   0.148460496     0.008296227  0.132199892  0.16472110
#> educationCollege        0.454287435     0.011447160  0.431851003  0.47672387
#> educationPost-graduate  0.616454232     0.013358216  0.590272128  0.64263634
#> experience0-4          -0.391417522     0.013110253 -0.417113617 -0.36572143
#> experience5-9          -0.299200841     0.013143288 -0.324961685 -0.27344000
#> experience10-14        -0.190724443     0.013717366 -0.217610481 -0.16383840
#> experience15-19        -0.085321103     0.014702198 -0.114137412 -0.05650479
#> experience25-29         0.045826795     0.016468888  0.013547774  0.07810581
#> experience30-34         0.078268103     0.017142538  0.044668729  0.11186748
#> experience35-39         0.028296285     0.017592020 -0.006184073  0.06277664
#> experience>=40         -0.015007184     0.016210144 -0.046779067  0.01676470
#> 
#> Residual standard error: 49.41651 on 53374 degrees of freedom
#> Multiple R-squared: 0.1861847,   Adjusted R-squared: 0.1859407
#> 
#> 
#> RIF regression coefficients for quantile_0.9
#> 
#>                           Estimate Analytical s.e. Lower bound Upper bound
#> (Intercept)             2.50910141     0.019820996  2.47025226  2.54795056
#> unionyes               -0.15515221     0.009864905 -0.17448742 -0.13581699
#> nonwhiteyes            -0.11827633     0.011426289 -0.14067186 -0.09588081
#> marriedyes              0.05285737     0.010182733  0.03289921  0.07281553
#> educationElementary    -0.26626430     0.011096336 -0.28801312 -0.24451548
#> educationHS dropout    -0.08536648     0.008296047 -0.10162674 -0.06910623
#> educationSome College   0.16684309     0.011524590  0.14425489  0.18943128
#> educationCollege        0.60641514     0.018831237  0.56950592  0.64332437
#> educationPost-graduate  0.89050987     0.023630234  0.84419461  0.93682513
#> experience0-4          -0.45705327     0.019977324 -0.49620883 -0.41789772
#> experience5-9          -0.36542130     0.020099962 -0.40481723 -0.32602537
#> experience10-14        -0.24722551     0.020988287 -0.28836255 -0.20608847
#> experience15-19        -0.11087371     0.022544959 -0.15506183 -0.06668559
#> experience25-29         0.08953774     0.026177089  0.03823064  0.14084483
#> experience30-34         0.07987727     0.026769300  0.02740945  0.13234510
#> experience35-39         0.06769310     0.027365119  0.01405747  0.12132874
#> experience>=40         -0.02721068     0.023673575 -0.07361089  0.01918952
#> 
#> Residual standard error: 73.61963 on 53374 degrees of freedom
#> Multiple R-squared: 0.1492248,   Adjusted R-squared: 0.1489697
```

`rifreg` comes with a convenient plot function. It illustrates the
estimated UQPE across the distribution. The confidence bands are based
on the same standard errors as displayed by `summary`.

``` r
plot(fit_uqr, varselect="unionyes")
#> Warning in plot.rifreg(fit_uqr, varselect = "unionyes"): Standard errors have not been bootstrapped!
#> Analytical s.e. do not take variance introduced by
#> estimating the RIF into account.
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Bootstrapping standard errors

Setting `bootstrap=TRUE` bootstraps standard errors by resampling from
all observations and reestimating both the RIF and the regression in
every iteration. We can set number of `bootstrap_iterations` and the
number of `cores`.

``` r
fit_uqr <- rifreg(ffl_model,
                  data=men8385,
                  weights=weights,
                  statistic="quantiles",
                  bootstrap=TRUE,
                  bootstrap_iterations=2, #100
                  cores=4,
                  probs=1:9/10)
#> Bootstrapping Standard Errors...
plot(fit_uqr,
     varselect="unionyes",
     confidence_interval=0.95)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### Other distributional statistics

`rifreg` performs RIF regressions for other distributional statistics
than quantiles. Per default, the “mean”, “variance”, “quantiles”,
“gini”, “interquantile_range” and “interquantile_ratio” are available.

``` r
ffl_model2 <- wage ~ union + nonwhite + married + education + experience
fit_gini <- rifreg(ffl_model2,
                  data=men8385,
                  weights=weights,
                  statistic="gini")
fit_d9d1 <- rifreg(ffl_model2,
                  data=men8385,
                  weights=weights,
                  statistic="interquantile_ratio",
                  probs=c(0.9, 0.1))
fit_gini
#> Rifreg coefficients:
#>                           rif_gini
#> (Intercept)            -3.60225797
#> unionyes               -0.98769909
#> nonwhiteyes             0.83273753
#> marriedyes             -0.70975056
#> educationElementary     2.12101685
#> educationHS dropout     0.97020642
#> educationSome College  -0.85371897
#> educationCollege       -2.49171458
#> educationPost-graduate -2.92713536
#> experience0-4           2.94517028
#> experience5-9           1.63033635
#> experience10-14         0.93208423
#> experience15-19         0.37262020
#> experience25-29        -0.17086373
#> experience30-34        -0.24904725
#> experience35-39        -0.17830041
#> experience>=40         -0.01665794
fit_d9d1
#> Rifreg coefficients:
#>                        rif_iq_ratio_0.9_0.1
#> (Intercept)                       4.8851827
#> unionyes                         -3.1983486
#> nonwhiteyes                       0.9804660
#> marriedyes                       -2.3156881
#> educationElementary               2.9971699
#> educationHS dropout               4.2520301
#> educationSome College             0.1127903
#> educationCollege                  0.1489916
#> educationPost-graduate            2.1686951
#> experience0-4                     5.1423852
#> experience5-9                    -0.5625992
#> experience10-14                  -0.6913670
#> experience15-19                  -0.2184045
#> experience25-29                   0.3417035
#> experience30-34                   0.4147115
#> experience35-39                   0.3498597
#> experience>=40                   -0.9383281
```

### User-written RIF functions

Users may write their own RIF function. Custom RIF functions must
specify a `dep_var` parameter for the outcome variable $Y$ and a
`weights` for potential sample weights.

The following example shows how to write the RIF for the top 10 percent
income share and, then, to estimate the RIF regression using this custom
function. The formula for this specific RIF can be found in Essam-Nssah
& Lambert (2012) or Rios-Avila (2020).

``` r
ffl_model2 <- wage ~ union + nonwhite + married + education + experience

# custom RIF function for top 10% percent income share
custom_top_inc_share <- function(dep_var,
                                 weights,
                                 top_share=0.1){
  top_share <- 1-top_share
  weighted_mean <- weighted.mean(x = dep_var, 
                                 w = weights)
  weighted_quantile <- Hmisc::wtd.quantile(x = dep_var,
                                           weights = weights,
                                           probs = top_share)
  lorenz_ordinate <- sum(dep_var[which(dep_var<=weighted_quantile)]*
                           weights[which(dep_var<=weighted_quantile)])/
    sum(dep_var*weights)
  if_lorenz_ordinate <- -(dep_var/weighted_mean) * lorenz_ordinate +
                          ifelse(dep_var < weighted_quantile,
                                 dep_var - (1-top_share)*weighted_quantile,
                                 top_share*weighted_quantile)/weighted_mean
  rif_top_income_share <-  (1-lorenz_ordinate) - if_lorenz_ordinate
  rif <- data.frame(rif_top_income_share, weights)
  names(rif) <- c("rif_top_income_share", "weights")
  return(rif)
}

fit_top_10 <- rifreg(ffl_model2,
                     data=men8385,
                     weights=weights,
                     statistic="custom",
                     custom_rif_function=custom_top_inc_share,
                     top_share=0.1)

fit_top_10
#> Rifreg coefficients:
#>                        rif_top_income_share
#> (Intercept)                     0.256731886
#> unionyes                       -0.068997798
#> nonwhiteyes                     0.007531907
#> marriedyes                     -0.017344324
#> educationElementary             0.024329590
#> educationHS dropout             0.022054504
#> educationSome College           0.002197374
#> educationCollege                0.031589544
#> educationPost-graduate          0.080400757
#> experience0-4                   0.014372720
#> experience5-9                  -0.018702959
#> experience10-14                -0.026032457
#> experience15-19                -0.013896898
#> experience25-29                 0.007969714
#> experience30-34                 0.008465597
#> experience35-39                 0.005562271
#> experience>=40                  0.002429636
```

## Validation

To validate the functions and provide users with an additional example,
we reproduced the RIF regression estimates in Firpo, Fortin, and Lemieux
(2009a: 962-966). In their empirical example, Firpo, Fortin, and Lemieux
estimate the impact of union status on log wages using a large sample of
266’956 male U.S. workers from 1983-1985. based on the Outgoing Rotation
Group (ORG) supplement of the Current Population Survey. To reproduce
the code below, make sure you download the [entire data set from the
journal’s
website](https://www.econometricsociety.org/publications/econometrica/2009/05/01/unconditional-quantile-regressions/supp/6822_data%20and%20programs_0.zip).

### Data preparation

``` r
library("dplyr")
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

## getting the data from the journal's website
# url <- "https://www.econometricsociety.org/publications/econometrica/2009/05/01/unconditional-quantile-regressions/supp/6822_data%20and%20programs_0.zip"
# download.file(url = url, destfile = "6822_data%20and%20programs_0.zip")
# men8385 <- readstata13::read.dta13(file = unzip("6822_data%20and%20programs_0.zip","men8385.dta"))

## Load data
men8385 <- readstata13::read.dta13("men8385.dta")

# Save dummies as factor variables
# nine potential experience categories (each of five years gap)
men8385$experience <- 5
men8385[which(men8385$ex1==1),"experience"] <- 1
men8385[which(men8385$ex2==1),"experience"] <- 2
men8385[which(men8385$ex3==1),"experience"] <- 3
men8385[which(men8385$ex4==1),"experience"] <- 4
# 5 = reference group
men8385[which(men8385$ex6==1),"experience"] <- 6
men8385[which(men8385$ex7==1),"experience"] <- 7
men8385[which(men8385$ex8==1),"experience"] <- 8
men8385[which(men8385$ex9==1),"experience"] <- 9

# Education
men8385$education <- 2
men8385[which(men8385$ed0==1),"education"] <- 0
men8385[which(men8385$ed1==1),"education"] <- 1
# high school = reference group
men8385[which(men8385$ed3==1),"education"] <- 3
men8385[which(men8385$ed4==1),"education"] <- 4
men8385[which(men8385$ed5==1),"education"] <- 5

men8385$education <- as.character(men8385$education)
men8385$experience <- as.character(men8385$experience)
men8385$experience <- dplyr::recode_factor(men8385$experience,
                                              "5"="20-24",
                                              "1"="0-4",
                                              "2"="5-9",
                                              "3"="10-14",
                                              "4"="15-19",
                                              "6"="25-29",
                                              "7"="30-34",
                                              "8"="35-39",
                                              "9"=">=40")

men8385$education <- dplyr::recode_factor(men8385$education,
                                             "2"="High School",
                                             "0"="Elementary",
                                             "1"="HS dropout",
                                             "3"="Some College",
                                             "4"="College",
                                             "5"="Post-graduate")


# Save log wage as wage hourly wage in dollars
men8385$wage <- exp(men8385$lwage)

# Rename/relevel remaining indicators
men8385$union <- as.factor(men8385$covered)
men8385$covered <- NULL

men8385$married <- as.factor(men8385$marr)
men8385$marr <- NULL
men8385$nonwhite <- as.factor(men8385$nonwhite)
levels(men8385$married) <- levels(men8385$nonwhite) <-  levels(men8385$union) <-  c("no","yes")

# Rename weight and education in years variable
names(men8385)[names(men8385)=="eweight"] <- "weights"
names(men8385)[names(men8385)=="educ"] <- "education_in_years"
names(men8385)[names(men8385)=="exper"] <- "experience_in_years"

# Check experience and age groups
men8385 %>%
  group_by(experience) %>%
  dplyr::summarise(min=min(experience_in_years,na.rm=TRUE),
                   max=max(experience_in_years,na.rm=TRUE))
#> # A tibble: 9 × 3
#>   experience   min   max
#>   <fct>      <dbl> <dbl>
#> 1 20-24         20    24
#> 2 0-4            0     4
#> 3 5-9            5     9
#> 4 10-14         10    14
#> 5 15-19         15    19
#> 6 25-29         25    29
#> 7 30-34         30    34
#> 8 35-39         35    39
#> 9 >=40          40    58

men8385 %>%
  group_by(education) %>%
  dplyr::summarise(min=min(education_in_years,na.rm=TRUE),
                   max=max(education_in_years,na.rm=TRUE))
#> # A tibble: 6 × 3
#>   education       min   max
#>   <fct>         <dbl> <dbl>
#> 1 High School      12    12
#> 2 Elementary        0     8
#> 3 HS dropout        9    11
#> 4 Some College     13    15
#> 5 College          16    16
#> 6 Post-graduate    17    18

# Select relevant variables
sel_vars <- c("wage", "union", "nonwhite", "married", "education", "experience", "weights", "age","education_in_years","experience_in_years")
men8385 <- men8385[, sel_vars]
```

### RIF Regression

The model is specified as in Firpo, Fortin, and Lemieux (2007a),
omitting weights, computing bootstrapped standard errors with 200
iterations and setting a fixed bandwidth of 0.06 for the kernel density
estimation. We also compute a OLS model for comparison with the original
results and to validate our data preparation.

``` r
library(rifreg)

set.seed(121095)
ffl_model_2009 <- log(wage) ~ union + nonwhite + married + education + experience
rifreg <- rifreg(formula = ffl_model_2009, 
                 data = men8385,
                 statistic = "quantiles",  
                 probs = seq(0.05, 0.95, 0.05),
                 bw = 0.06, 
                 bootstrap = TRUE, 
                 bootstrap_iterations = 3, #200
                 cores=1) 
#> Bootstrapping Standard Errors...

ols <- lm(formula = ffl_model_2009, 
          data = men8385)
```

### Results

``` r
estimates <- data.frame(rifreg$estimates[1:9, c("rif_quantile_0.1", "rif_quantile_0.5", "rif_quantile_0.9")])
standard_errors <- data.frame(rifreg$bootstrap_se[1:9, c("rif_quantile_0.1", "rif_quantile_0.5", "rif_quantile_0.9")])
results <- cbind(estimates, standard_errors)[, c(1, 4, 2, 5, 3, 6)]
results$ols <- ols$coefficients[1:9]
names(results) <- c("Coefficient 0.1", "SE", "Coefficient 0.5", "SE", "Coefficient 0.9", "SE", "OLS")

knitr::kable(results)
```

|                        | Coefficient 0.1 |        SE | Coefficient 0.5 |        SE | Coefficient 0.9 |        SE |        OLS |
|:-----------------------|----------------:|----------:|----------------:|----------:|----------------:|----------:|-----------:|
| (Intercept)            |       0.9704165 | 0.0071128 |       1.7346767 | 0.0044596 |       2.5112831 | 0.0024535 |  1.7418195 |
| unionyes               |       0.1950164 | 0.0050750 |       0.3361029 | 0.0049680 |      -0.1345583 | 0.0084842 |  0.1790582 |
| nonwhiteyes            |      -0.1161961 | 0.0078975 |      -0.1635135 | 0.0039054 |      -0.0989621 | 0.0044894 | -0.1339497 |
| marriedyes             |       0.1943921 | 0.0026328 |       0.1556403 | 0.0040446 |       0.0430328 | 0.0039915 |  0.1395608 |
| educationElementary    |      -0.3063238 | 0.0013907 |      -0.4510738 | 0.0005457 |      -0.2396602 | 0.0014175 | -0.3512943 |
| educationHS dropout    |      -0.3438799 | 0.0013068 |      -0.1942754 | 0.0058462 |      -0.0679424 | 0.0008502 | -0.1897010 |
| educationSome College  |       0.0579543 | 0.0055985 |       0.1782772 | 0.0012220 |       0.1533879 | 0.0053009 |  0.1333362 |
| educationCollege       |       0.1962486 | 0.0050459 |       0.4635776 | 0.0043778 |       0.5810668 | 0.0084767 |  0.4064842 |
| educationPost-graduate |       0.1379375 | 0.0062589 |       0.5212637 | 0.0013806 |       0.8430019 | 0.0123854 |  0.4777400 |

Our results largely match those by Firpo, Fortin, and Lemieux (2009a:
962-966). The OLS results are identical as expected. The RIF regression
results differ only in a few instances at the third decimal place from
the original paper.

``` r
rifreg_plot <- plot(rifreg, varselect = "unionyes") 
```

``` r
rifreg_plot + 
  geom_hline(yintercept = ols$coefficients["unionyes"], linetype = 'dashed') +
  geom_text(aes(x = 0.8, y = ols$coefficients["unionyes"]  + 0.03, label = "OLS estimate"), color = "black")  +
  ylab("Effect of Union") +
  xlab("Quantiles") + 
  labs(title = "Unconditional Quantile Regression in R") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />
Looking at the plots, we see that our plots correspond to those
presented by Firpo, Fortin, and Lemieux (2009a: 965). The plot example
illustrates, how the plot from the generic `rifreg::plot()` function can
be further enhanced, for instance with a horizontal line indicating the
OLS coefficient for comparison.

This validation example illustrates that the `rifreg` package works as
intended in computing RIF regressions and reliably yields the expected
results.

## Credits

David Gallusser & Samuel Meier

## References

Cowell, Frank A., and Emmanuel Flachaire. 2007. “Income distribution and
inequality measurement: The problem of extreme values.” *Journal of
Econometrics* 141: 1044–1072.

Essama-Nssah, Boniface, and Peter J. Lambert. 2012. “Influence Functions
for Policy Impact Analysis.” In John A. Bishop and Rafael Salas, eds.,
*Inequality, Mobility and Segregation: Essays in Honor of Jacques
Silber*. Bigley, UK: Emerald Group Publishing Limited.

Firpo, Sergio, Nicole M. Fortin, and Thomas Lemieux. 2007a.
“Unconditional Quantile Regressions.” *Tech- nical Working Paper 339,
National Bureau of Economic Research*. Cambridge, MA.

Firpo, Sergio, Nicole M. Fortin, and Thomas Lemieux. 2009a.
“Unconditional Quantile Regressions.” *Econometrica* 77(3): 953-973.

Firpo, Sergio, Nicole M. Fortin, and Thomas Lemieux. 2009b. “Supplement
to ‘Unconditional Quantile Regressions’.” *Econometrica Supplemental
Material*, 77.

Rios-Avila, Fernando (2020): “Recentered influence functions (RIFs) in
Stata: RIF regression and RIF decomposition.” *The Stata Journal* 20(1):
51-94.

Rothe, Christoph (2015): “Decomposing the Composition Effect. The Role
of Covariates in Determining Between-Group Differences in Economic
Outcomes.” *Journal of Business & Economic Statistics* 33(3): 323-337.
