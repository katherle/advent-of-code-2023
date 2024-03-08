import pandas as pd
import matplotlib.pyplot as plt

ex1_results = pd.read_csv("ex1_results.txt", names = ["data", "mu_grid", "lnL"], delim_whitespace=True)
mu = 10.
sigma = 1.

plt.figure(figsize = (10, 7))
plt.hist(ex1_results["data"], bins = 30)
plt.xlabel('Amplitude',size=15)
plt.ylabel('Bin Count',size=15)
plt.axvline(mu,color='k',linestyle='--')
plt.axvline(mu-sigma,color='k',linestyle='--')
plt.axvline(mu+sigma,color='k',linestyle='--')
plt.show()

plt.figure(figsize = (10, 7))
plt.plot(ex1_results["mu_grid"], ex1_results["lnL"])
plt.axvline(9.974136, color = "k", linestyle = "--", linewidth = 1)
plt.xlabel(r"$\mu$")
plt.ylabel(r"$\mathcal{L}$")
plt.show()