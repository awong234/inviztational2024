from goes2go import GOES
from netCDF4 import Dataset
import sqlite3
import pathlib
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from matplotlib.colors import LinearSegmentedColormap
import os
import toml

con = sqlite3.connect('data/data.db')
G = GOES(satellite=16, product="ABI-L1b-Rad", domain='F', channel=11)

config = toml.load(os.path.expanduser("~/.config/goes2go/config.toml"))
goes_path = pathlib.Path(config['default']['save_dir']) / config['default']['satellite'] / (config['default']['product'] + config['default']['domain'])

##### Some errant files have 0 size. Remove -------------


def remove_empty_files(goes_path):
    get_files = lambda path: (os.path.join(root, file) for root, dirs, files in os.walk(path) for file in files)
    file_sizes = []
    file_paths = []
    for file in get_files(goes_path):
        file_paths.append(file)
        file_sizes.append(os.path.getsize(file))

    file_sizes = np.array(file_sizes)
    files_to_delete = np.array(file_paths)[file_sizes==0]
    for file in files_to_delete:
        try:
            os.remove(file)
            print(f"Removed {file}")
        except:
            pass

remove_empty_files(goes_path)

# For each image, find the nearest storm track in time and center the image on
# that location. Crop to the maximal radius of the storm.

def gen_image(m,datetime,grad,margin=10,center_lat=45,center_lon=-70,SID='',my_dpi=200,show=False,save_img=True,show_lines=True):
    datetime_str = str(datetime).replace(' ', 'T').replace(':','-')
    filename=rf"./img/storm_centered/{SID}/centered_{datetime_str}.png"
    if os.path.exists(filename):
        # If output file exists, don't compose it again
        return None

    try:
        g16nc = G.nearesttime(datetime)
    except:
        # If there are any empty files, remove them. This can be done en-masse too, as above, but at least we can try again here.
        year=str(datetime.year)
        yday=str(datetime.day_of_year)
        hour="%02.0f" % datetime.hour
        goes_path = pathlib.Path(r"D:\goes\noaa-goes16\ABI-L1b-RadF")
        image_path = goes_path / year / yday / hour
        files = os.listdir(image_path)
        abspaths = [image_path / f for f in files]
        sizes = np.array([os.path.getsize(x) for x in abspaths])
        for path, size in zip(abspaths, sizes):
            if size==0:
                os.remove(path)
        # Try downloading the image again
        g16nc = G.nearesttime(datetime)

    rad=g16nc.variables['Rad'][:]

    fig=plt.figure(figsize=(300/my_dpi, 300/my_dpi), dpi=my_dpi)

    im=m.imshow(np.flipud(rad).astype('uint8'),cmap=grad,vmin=0,vmax=100,alpha=1)

    lllon = center_lon-margin
    urlon = center_lon+margin
    lllat = center_lat-margin
    urlat = center_lat+margin

    xmin, ymin = m(lllon, lllat)
    xmax, ymax = m(urlon, urlat)
    xrange=xmax-xmin
    yrange=ymax-ymin
    xcenter=(xmin+xmax)/2
    ycenter=(ymin+ymax)/2
    usable_range=min(xrange,yrange)
    xmin=xcenter-usable_range/2
    xmax=xcenter+usable_range/2
    ymin=ycenter-usable_range/2
    ymax=ycenter+usable_range/2
    ax = plt.gca()

    ax.set_xlim([xmin, xmax]);
    ax.set_ylim([ymin, ymax]);
    if show_lines:
        m.drawcoastlines(linewidth=0.5, color='white')
    if save_img:
        plt.axis('off')
        folder = os.path.dirname(filename)
        if not os.path.exists(folder):
            os.makedirs(folder, exist_ok=True)
        plt.savefig(filename, bbox_inches='tight',pad_inches=0,transparent=True);
    if show:
        plt.show()
    plt.close(fig) #memory leak if you don't close fig
    return im

grad = LinearSegmentedColormap.from_list('my_gradient', (
    # Edit this gradient at https://eltos.github.io/gradient/#0:FF002B-13.2:FFED00-23.5:0BF620-40.1:1E2DE4-53.5:6C6C6C-100:FFFFFF
    (0.000, (1.000, 0.000, 0.169)),
    (0.132, (1.000, 0.929, 0.000)),
    (0.235, (0.043, 0.965, 0.125)),
    (0.401, (0.118, 0.176, 0.894)),
    (0.535, (0.424, 0.424, 0.424)),
    (1.000, (1.000, 1.000, 1.000))))

projection = {
    "2017": (0.0, -89.5),
    "2019": (0.0, -75.0),
    "2022": (0.0, -75.0),
}

basemaps = {
    "2017": Basemap(resolution='l', projection='geos', lon_0=projection['2017'][1],lat_0=projection['2017'][0],rsphere=(6378137.00,6356752.3142)),
    "2019": Basemap(resolution='l', projection='geos', lon_0=projection['2019'][1],lat_0=projection['2019'][0],rsphere=(6378137.00,6356752.3142)),
    "2022": Basemap(resolution='l', projection='geos', lon_0=projection['2022'][1],lat_0=projection['2022'][0],rsphere=(6378137.00,6356752.3142)),
}

storms = pd.read_sql("select hurricane_id, datetime, latitude, longitude from paths_interp where hurricane_id in (select ID from chosen_storms) order by hurricane_id, datetime", con, parse_dates = ['datetime'])

for row in storms.iterrows():
    hurricane, datetime, mylat, mylon = row[1]
    print(hurricane, datetime, mylat, mylon)
    try:
        gen_image(basemaps[str(datetime.year)], datetime=datetime, grad=grad, SID=hurricane, center_lat=mylat, center_lon=mylon, margin=11, show=False,show_lines=True)
    except:
        pass
