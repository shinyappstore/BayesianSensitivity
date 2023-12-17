In this hypothetical scenario, the researcher had some prior knowledge that they wanted to incorporate into the model (see plots to the right). 


Most of the priors selected are only moderately informative. For example, the prior on the intercept of Cynicism is centered around 41 and has a variance of 10. As such, roughly 95% of the density is between 34.67 and 47.32. The priors for the regression coefficient associated with the predictor 'Sex' is centered on zero and has a variance of 10. Thus, this regression coefficient can potentially take on a wide range of values.


The researcher **did** have substantial prior knowledge about the regression coefficient for the predictor 'Lack of trust'. Therefore, the researcher centered the prior around 6 with a variance of 1. By utilizing this prior, about 95% of the density is between 4 and 8. Such a narrow prior suggests the researcher has a strong expectation that a one-point increase in lack of trust is related to a 4 to 8 point increase in Cynicism.

The prior for the residual variance follows an inverse gamma distribution. For that reason, the researcher selected inverse gamma priors for (residual) variance parameters; previous research has shown that the specification used here by the researcher is weakly informative.

