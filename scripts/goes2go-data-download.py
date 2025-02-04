from goes2go import GOES
import sqlite3
import pandas as pd
from datetime import datetime, timedelta

con = sqlite3.connect('data/data.db')

dates = pd.read_sql('select distinct datetime from paths where hurricane_id in (select id from chosen_storms)', con, parse_dates = ['datetime'])
dates = [datetime.strftime(x, '%Y-%m-%d') for x in dates.datetime]
dates = list(set(dates))
dates.sort()

G = GOES(satellite=16, product="ABI-L1b-Rad", domain='F', channel=11)

for date in dates:
    t1 = datetime.strptime(date + ' 00:00:00', "%Y-%m-%d %H:%M:%S")
    t2 = t1 + timedelta(days=1)
    G.timerange(start=t1, end=t2)
