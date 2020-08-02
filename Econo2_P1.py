import quandl
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats
from pandas.plotting import register_matplotlib_converters
import datetime as dt 
import pdb

usd = pd.read_excel("RS_USD.xlsx")
# Renaming columns

usd = pd.DataFrame(usd)
usd = usd.rename(columns = {"Data": "date", "R$/US$": "t","Variação (em %)": "%"})

# Time shift loop 
print(usd)
print("Choose n (time lag): ")
n = 5 #input()

usd_shift = pd.concat([usd, usd.shift(1), usd.shift(2), usd.shift(3)], axis=1)

usd_shift
usd_shift = usd

n=5

for i in range(4, n+4):
    usd["lag_"+str(i)] = usd.t.shift(i)


print(usd)



#usd_shift = usd.shift(periods=1)
#price = usd_shift["t"]
#price = pd.DataFrame(price) 
#price = price.rename(columns = {"t": "t - " + str(1)})
#price


#price


