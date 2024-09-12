# %%
import xarray as xr
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import RegularGridInterpolator
import os
cwd = os.getcwd()

# Define a function to convert the numeric time format back to datetime
def numeric_to_datetime(numeric_time):
    year = int(numeric_time)
    day_of_year = (numeric_time - year) * 365.25
    return pd.Timestamp(year=year, month=1, day=1) + pd.to_timedelta(day_of_year, unit='D')


#%%
path = r'C:\projecten\RWS\zeespiegelmonitor\sealevelmonitor\data\copernicus\era5\adaptor.mars.internal-1719383727.0796268-6723-3-bd385cf1-ecd9-4668-9858-a62a44a878fd.nc'
nlstation= r'C:\projecten\RWS\zeespiegelmonitor\sealevelmonitor\data\psmsl\NLstations.csv'

df = pd.read_csv(nlstation, sep = ';')
ds = xr.open_dataset(path)
#our data has both expver 1 and 5 https://forum.ecmwf.int/t/era5-cds-requests-which-return-a-mixture-of-era5-and-era5t-data/781
#however. we only need the expver 1 because we need data till 31-dec-2023 and the data is downloaded later then that
ds = ds.sel(expver=1)
ds = ds.drop_vars('sp') #sp = surface pressure, not needed so it is dropped
ds = ds.resample(time='1Y').mean()
# %%
# Extract the necessary data
grid_lat = ds['latitude'].values
grid_lon = ds['longitude'].values

# Convert the time dimension to a numeric format (e.g., year as float)
ds['time'] = ds['time'].dt.year + (ds['time'].dt.dayofyear - 1) / 365.25
grid_time = ds['time'].values

# Create interpolator functions for u and v
u_interpolator = RegularGridInterpolator((grid_time, grid_lat, grid_lon), ds['u10'].values)
v_interpolator = RegularGridInterpolator((grid_time, grid_lat, grid_lon), ds['v10'].values)

# Dictionary to store the results
results = {'lat': [], 'lon': [], 'stationname': [],'year': [], 'u_mean': [], 'v_mean': []}

for index, row in df.iterrows():
    lat = row['Lat']
    lon = row['Lon']
    stationname = row['StationName']
    
    # Prepare the points for interpolation
    points = [(time, lat, lon) for time in grid_time]
    
    # Interpolate the yearly averaged data to the location
    u_values = u_interpolator(points)
    v_values = v_interpolator(points)
    
    # Collect the results
    for i, time in enumerate(grid_time):
        results['lat'].append(lat)
        results['lon'].append(lon)
        results['stationname'].append(stationname)
        results['year'].append(time)
        results['u_mean'].append(u_values[i])
        results['v_mean'].append(v_values[i])

interpolated_results = pd.DataFrame(results)
interpolated_results['year'] = interpolated_results['year'].apply(numeric_to_datetime)

interpolated_results.to_csv(r'C:\projecten\RWS\zeespiegelmonitor\sealevelmonitor\data\copernicus\era5\era5_wind_for_analysis.csv')
# %%
