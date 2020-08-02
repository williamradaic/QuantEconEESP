import random
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t, beta, lognorm, expon, gamma, uniform, cauchy
from scipy.stats import gaussian_kde, poisson, binom, norm, chi2
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.collections import PolyCollection
from scipy.linalg import inv, sqrtm

n = 100

# Arbitrary collection of distributions

distributions = {
    "student's t with 10 degrees of freedom": t(10),
    "beta(2,2)": beta(2,2),
    "lognormal LN(0,1/2)": lognorm(0.5),
    "γ(5, 1/2)": gamma(5, scale=2),
    "poisson(4)": poisson(4),
    "exponential with λ = 1": expon(1)
    }

# Create a figure and some axes

num_plots = 3
fig, axes = plt.subplots(num_plots, 1, figsize=(10,20))

# Set some plotting parameters to improve layout

bbox = (0., 1.02, 1., .102)
legend_args = {
    'ncol': 2,
    'bbox_to_anchor': bbox,
    'loc': 3,
    'mode': 'expand'
}

plt.subplots_adjust(hspace=0.5)

for ax in axes:
    # Choose a randomly selected distribution
    name = random.choice(list(distributions.keys()))
    distribution = distributions.pop(name)

    # Generate n draws from the distribution
    data = distribution.rvs(n)

    # Compute sample mean at each n
    sample_mean = np.empty(n)
    for i in range(n):
        sample_mean[i] = np.mean(data[:i+1])
    ax.plot(list(range(n)), data, 'o', color='grey', alpha=0.5)
    axlabel = '$\\bar X_n$ for $X_i \sim$' + name
    ax.plot(list(range(n)), sample_mean, 'g-', lw=3, alpha=0.6, label = axlabel)
    m = distribution.mean()
    ax.plot(list(range(n)), [m] * n, 'k--', lw = 1.5, label = '$\mu$')
    ax.vlines(list(range(n)), m, data, lw = 0.2)
    ax.legend(**legend_args, fontsize = 12)

plt.show()

