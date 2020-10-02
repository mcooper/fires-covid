import ee
from datetime import date
from datetime import datetime
from datetime import timedelta
import calendar
import os
import pandas as pd

ee.Initialize()

have = pd.read_csv("/home/mattcoop/fires-covid/data/ee_res.csv")
have = have[(have['id'] != 'id') & (~pd.isnull(have['date']))]

d = have.date.apply(lambda x: datetime.strptime(x, "%Y-%m-%d"))

#Get list of days by midnight UTC in Unix timestamps
#Start with March 15th (beginning of pandemic, after DST)
start = d.max()
end = datetime.today()
date_list = [start + timedelta(days=x) for x in range(0, (end-start).days)] 

def makeCaliTime(x):
    #Convert to Unix
    time = calendar.timegm(x.timetuple())
    #Add 7 hours to get to Cali
    time += 7*3600
    #Comvert to milisections
    time = time*1000
    return(time)

#Get points as a feature collection
ll = pd.read_csv('/home/mattcoop/fires-covid/data/station_county_ll.csv')

points = []
for i in ll.iterrows():
    geom = ee.Geometry.Point(i[1]["X"], i[1]["Y"])
    feat = ee.Feature(geom, {'id': i[1]['id']})
    points.append(feat)

fc = ee.FeatureCollection(points)

#for x in range(1, len(date_list)):
for x in [11]:
    print(date_list[x])
    try:
        data = ee.ImageCollection("COPERNICUS/S5P/NRTI/L3_AER_AI") \
                .filterDate(makeCaliTime(date_list[x - 1]), makeCaliTime(date_list[x])) \
                .select(0) \
                .median()
        res = data.reduceRegions(collection=fc, reducer=ee.Reducer.mean(), scale=50).getInfo()
        df = pd.DataFrame([x['properties'] for x in res['features']])
        df['date'] = date_list[x-1]
        df.to_csv("/home/mattcoop/fires-covid/data/ee_res.csv", index=False, mode='a')
    except Exception as e:
        print("Failure on", date_list[x], ':', e)
        os.system('~/telegram.sh "Error"')

os.system('~/telegram.sh "Donezo!"')


