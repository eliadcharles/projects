#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Understanding GIS: Assessment 2
@author [9477699]

An Implementation Weighted Redistribution Algorithm (Huck et al.)
"""

from time import time

# set start time
start_time = time()	# NO CODE ABOVE HERE

from rasterio import open as rio_open
from numpy import zeros, argmax
from geopandas import read_file
from shapely.geometry import Point
from rasterio.plot import show as rio_show
from matplotlib_scalebar.scalebar import ScaleBar
from matplotlib.cm import ScalarMappable
from matplotlib.colors import  Normalize
from math import hypot, floor, ceil, sqrt, pi
from matplotlib.pyplot import subplots, savefig, get_cmap, colorbar
from matplotlib.patches import Patch
from numpy.random import randint

    
#import relevant data 
districts = read_file("./wr/gm-districts.shp")
tweets = read_file("./wr/level3-tweets-subset.shp")


#extract population density raster 
with rio_open( './wr/100m_pop_2019.tif') as pop:
    pop_den = pop.read(1)

#get all the point coordinates in the data set as a list
points = list(tweets.geometry)

#initialsing parameters 

w = 20 # weight given to underlying raster distribution population density
s = 0.01 # spatial ambiguity 

#produce blank numpy matrix to store results from distribution
output = zeros((pop.height, pop.width))

#loop through each district
for district in districts.NAME:
    
    #get the polygon for each district
    poly = districts[(districts.NAME) == district]['geometry'].iloc[0]
    
    #get the bounding box for each district
    bounds = poly.bounds
    
    #comupute radius in cordinate space
    radius = sqrt((poly.area * s)/pi)
    
    # convert the radius (m) to pixels
    radius_px = int(radius / pop.res[0])
    
    #start counter for datapoints in each district
    datapoints = 0
    
    # loop through points in dataset
    for point in points:
        #if point is within the district polygon count it
        if point.within(poly):
            datapoints += 1
            
            #remove point so it will not be checked again  
            points.remove(point)
            
    #empty list to store pseudorandomly generated seeds which fall within the polygon
    stored_points = []
    
    # # create a random data point for each actual data point which came from the district 
    for tweet in range(0,datapoints):
        
        candidate_points = []
        
        #to ensure that at there is at least 1 point which is chosen as a potential seed for low values of w. 
        non_zero_candidates = False
        
        while not non_zero_candidates:
            
            # make w random datapoints for each tweet 
            for i in range(0,w):
                        
                    #get random coordinate in bounding box of district
                    x , y = randint(bounds[0],bounds[2]),randint(bounds[1],bounds[3])
                    
                    #store as a point to check is inside polygon
                    coordinate = Point(x,y)
                
                    #if random coordinate is within polygon, store it. 
                    if coordinate.within(poly):
                        candidate_points.append((x,y))
            
            #break while loop if at least one seed location has been found            
            if len(candidate_points) > 0:
                non_zero_candidates = True
            
        # store population density values for each random point 
        values = [pop_den[(pop.index(j[0],j[1]))] for j in candidate_points]
        
        #get new seed from candiates based on max pop den value
        seed = (candidate_points[argmax(values)][0], candidate_points[argmax(values)][1])
        
        #store seeds
        stored_points.append(seed)
    
    for x , y in stored_points:
        
        #convert to image space
        seed_row, seed_col = pop.index(x,y)
        
        #get coordinates in image space for the bounding box of imaginary circle around seed coordinate
        top_left_row, top_left_col, bottom_right_row, bottom_right_col = Point(seed_row, seed_col).buffer(radius_px).bounds
       
        
        try:
            #loop through from top left to bottom right of the bounding box
            for row in range(int(top_left_row),int(bottom_right_row)):            
                for col in range(int(top_left_col),int(bottom_right_col)):
                    
                    # calculate distance in image space
                    distance = hypot((seed_row-row),(seed_col-col))
                   
                    #only want distances within the radius to get circular shape and to be able to normalise
                    if distance < radius_px:
                        
                        #finish of calculating v
                        v = 1 - (distance/radius_px)
                        
                        # assign value to output np.array
                        output[row][col] += v
        
        except IndexError:
            pass 


# normalize data to between 0 and 1
output = (output-output.min())/(output.max()-output.min()) 


# plot the dataset
fig, my_ax = subplots(1, 1, figsize=(16, 10))
my_ax.set(title=f"Likely pattern of tweets accross 9 districts in Manchester \n Raster Weighting = {w}, Spatial Ambiguity = {s}")

my_ax.axis('off')

cmap = get_cmap('YlOrRd')


# add the DEM
districts.boundary.plot(
    ax=my_ax,
    edgecolor='black'
    )       

# add the drawing layer
rio_show(
    output,
    ax=my_ax,
    transform=pop.transform,
    cmap = cmap
    )


# add a colour bar
cb = colorbar(ScalarMappable(norm=Normalize(vmin=floor(output.min()), vmax=ceil(output.max())), cmap=cmap), ax=my_ax)
cb.ax.set_ylabel('Activity',fontsize=18,rotation=270)


# add north arrow
x, y, arrow_length = 0.97, 0.99, 0.1
my_ax.annotate('N', xy=(x, y), xytext=(x, y-arrow_length),
    arrowprops=dict(facecolor='black', width=5, headwidth=15),
    ha='center', va='center', fontsize=20, xycoords=my_ax.transAxes)

# add scalebar
my_ax.add_artist(ScaleBar(dx=1, units="m", location="lower right"))


# save the result
savefig(f'./out/Tweet_Distribution w {w}, s {s}.png', bbox_inches='tight')


fig, my_ax1 = subplots(1, 1, figsize=(16, 10))

my_ax1.set(title="Raw Twitter Data")

my_ax1.axis('off')

# add the DEM
districts.boundary.plot(
    ax=my_ax1,
    edgecolor='black'
    )

districts.centroid.plot(
    ax= my_ax1,
    color = 'red')        

tweets.geometry.plot(
    ax = my_ax1,
    color = 'blue')

for idx, row in tweets.iterrows():
    my_ax1.annotate(s=row['l3'], xy=(row['geometry'].x , row['geometry'].y),
                  horizontalalignment='center'
                  )

# add north arrow
x, y, arrow_length = 0.97, 0.99, 0.1
my_ax1.annotate('N', xy=(x, y), xytext=(x, y-arrow_length),
    arrowprops=dict(facecolor='black', width=5, headwidth=15),
    ha='center', va='center', fontsize=20, xycoords=my_ax1.transAxes)

# add scalebar
my_ax1.add_artist(ScaleBar(dx=1, units="m", location="lower right"))

# manually draw a legend
my_ax1.legend([
    Patch(facecolor='blue', label='Tweets'),
    Patch(facecolor = 'red', label = 'District Centroid')],
	['Tweets','District Centroid'], loc='upper left')

# save the result
savefig('./out/Raw-TwitterData.png', bbox_inches='tight')

# report runtime
print(f"completed in: {time() - start_time} seconds")	# NO CODE BELOW HERE

