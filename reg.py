
#%%
import pandas as pd
from sklearn.linear_model import LinearRegression

promedios_mun = pd.read_csv('../output/promedios_mun.csv')

#%%
X = pd.DataFrame(promedios_mun['cantidad_promedio'])
Y = promedios_mun['ingcor_promedio']

model = LinearRegression(positive=True).fit(X, Y)

coefficients = model.coef_
print(f'Coefficients: {coefficients}')

# %%
