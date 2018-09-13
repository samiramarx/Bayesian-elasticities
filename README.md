# Estimating fare elasticities in rail demand in Great Britain using Bayesian inference

## Executive Summary

The purpose of this study is to present an alternative method to estimate coherent and consistent rail fare elasticities in Great Britain, including cross effects of different types of tickets.

Making usage of Bayesian econometrics, the goal is to overcome the elementary issue of previous studies of estimating correct algebraic signs estimates: negative own elasticities and positive cross elasticities, as it is usual to expect from normal competitors goods. 

Because of their features, Bayesian methods provide a simple approach to incorporate prior knowledge in the estimation process and restrict the elasticity domain to assure theoretical consistency of estimates' signs. The literature has shown that they have been successfully applied to market research and elasticity estimation in other industries. 

In practical terms, this work studies Bayesian regressions for demand models of each ticket type. In these models, the predictors are the fares and two complementary variables representing the other drivers of demand, in accordance with the PDFH - *Passenger Demand Forecasting Handbook*: gross value added (GVA), covering the effects of the external factors, and generalised journey time (GJT), covering the effects of quality variables. This study has applied the *Rail Users and Drivers Dataset* - RUDD, subsetted for non-London long-distance journeys.

Because there are different circumstances of competition among tickets, the data was subsetted in 4 markets, according to the ticket availability. Market 1 covered all routes for which the available fares were *Standard Full* and *Standard Reduced*. In Market 2 the *Standard Advance* fare was included. For Markets 3 and 4 the *First Class* tickets were included: together as a first class effect in Market 3, and segregated in *Full*, *Reduced* and *Advance* in Market 4. 

The market categories provided also a complexity scale. Thus, Market 1 is the simplest one and Market 4 is most complex one - indeed, an estimation that has not been covered in previous studies.

The results have shown that the Bayesian regression has successfully estimated correct algebraic signs for all elasticities in all four markets. However, a drawback that must be mentioned is that, even though the estimation process has achieved satisfactory measures of convergence and autocorrelation for the Markov Chains of Monte Carlo, divergent transitions were reported, which might signalise biased estimates. Because this issue is likely to be related with the domain constraint applied to the estimates, it was not judged as a harm, since it is a necessary evil for the solution adopted.

Additionally, in which regard the interpretation of the coefficients it was noticeable that as complexity increases in the markets, the estimates have lost in precision, but still better than SURE/OLS estimates. Regarding their magnitude, the results have present some unusual values, which must be further investigated. It should be recognised, however, that a proper analysis of magnitude should draw deeper considerations and such complexity was out of the scope of this work.

The overall conclusion is that, as a proof-of-concept study, this work has demonstrated that elasticities estimated by Bayesian methods potentially have practical application for rail demand forecasting. Further developments can bring it closer to reality with the adoption of dynamic effects - short and long-run, and restrictions on the supply side, particularly for \textit{Advance} tickets.

Beyond that, another development that could bring more precision for the train operating companies forecast is the estimation of elasticities applying hierarchical models, which might allow for the estimation route by route, even when few data are available.
