# fasano.franceschini.test 2.0.1
* Minor speedups introduced, particularly for the brute force method.
* The default computation method is now `'o'` for optimize: the test statistic is computed using both the range tree and brute force methods, and the faster of the two is used for the permutation test.

# fasano.franceschini.test 2.0.0
* Major overhaul of the package. Most computations are now done in C++ now rather than R.
* Testing has been extended from two dimensions to arbitrary dimensions.
* A second method for computing the test statistic has been added which has a loglinear time complexity rather than quadratic.
* The `cores` argument has been renamed `threads`. `cores` is still allowed for backwards compatibility but will be removed in the next release.
* A `seed` argument has been added to ensure reproducibility in the p-value estimation.
* A `verbose` argument has been added to print a progress bar during the permutation test.
* The p-value distributional approximation has been removed, as the approximation is only valid in two dimensions.
* A confidence interval for the p-value estimate is now returned.
* Points that are tied along certain axes no longer add fractional counts to the test statistic. See paper for exact details on test statistic computation. 

# fasano.franceschini.test 1.1.0

* The `nBootstrap` argument is deprecated, and will be removed in the next version of this package. Use `nPermute` instead of `nBootstrap` to compute the empirical null distribution for your data.

# fasano.franceschini.test 1.0.1

* More efficient `quadCount()` calculation reducing runtime.
* Test result now of class `Htest` for improved integration with other packages.
* Bug fix in bootstrapping procedure ensuring p-value is bounded between 0 and 1.

# fasano.franceschini.test 1.0.0

**The `fasano.franceschini.test` has been released into the wild!**
  
* For a comprehensive analysis and discussion of the `fasano.franceschini.test`, see the accompanying [paper](https://arxiv.org/abs/2106.10539).
