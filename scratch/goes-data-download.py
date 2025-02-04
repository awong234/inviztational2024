import xarray as xr
import requests
import netCDF4
import boto3
from botocore import UNSIGNED
from botocore.config import Config
import matplotlib.pyplot as plt
import pandas as pd
from datetime import datetime,timedelta
from tqdm import tqdm
import gc
import os

bucket_name = 'noaa-goes16'
product_name = 'ABI-L1b-RadF'#'ABI-L2-DMWF'#'ABI-L2-CMIPF'#'ABI-L1b-RadF'#ABI-L2-ACHTF#'ABI-L2-ACTPF'#
year = 2017
day_of_year = 298
hour = 12
band = 11

s3_client = boto3.client('s3', config=Config(signature_version=UNSIGNED))

def get_s3_keys(bucket, s3_client, prefix = ''):
    """
    Generate the keys in an S3 bucket.

    :param bucket: Name of the S3 bucket.
    :param prefix: Only fetch keys that start with this prefix (optional).
    """

    kwargs = {'Bucket': bucket}

    if isinstance(prefix, str):
        kwargs['Prefix'] = prefix

    while True:
        resp = s3_client.list_objects_v2(**kwargs)
        for obj in resp['Contents']:
            key = obj['Key']
            if key.startswith(prefix):
                yield key


        try:
            kwargs['ContinuationToken'] = resp['NextContinuationToken']
        except KeyError:
            break

print(f'{day_of_year:02.0f}')
print(f'{product_name}/{year}/{day_of_year:03.0f}/{hour:02.0f}/OR_{product_name}-M*C{band:02.0f}')
#OR_ABI-L1b-RadF-M6C16_G16
keys = get_s3_keys(bucket_name,
                   s3_client,
                   prefix = f'{product_name}/{year}/{day_of_year:03.0f}/{hour:02.0f}/OR_{product_name}-M3C{band:02.0f}'
                  )
all_keys= [key for key in keys]
key = all_keys[0] # selecting the first measurement taken within the hour
resp = requests.get(f'https://{bucket_name}.s3.amazonaws.com/{key}')

file_name = key.split('/')[-1].split('.')[0]
nc4_ds = netCDF4.Dataset(file_name, memory = resp.content)

fig = plt.figure(figsize=(12, 12))
rad=nc4_ds.variables['Rad'][:]
# plt.imshow(rad, cmap='gray')
plt.imshow(rad)
plt.axis('off')
plt.show()

nc4_ds.variables
