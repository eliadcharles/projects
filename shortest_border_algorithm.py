#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Understanding GIS: Assessment 1
@author [9477699]

Calculate the length of the World's Shortest Border, as fast as possible
"""


from time import time

# set start time
start_time = time()	# NO CODE ABOVE HERE


from pyproj import Geod, CRS, Transformer
from shapely.geometry import shape
from geopandas import read_file, GeoSeries
from matplotlib.pyplot import subplots, savefig
from matplotlib.patches import Patch
from matplotlib_scalebar.scalebar import ScaleBar
from rtree import index
from numpy import arange
from numpy.random import uniform
from shapely.geometry import  Polygon
from math import hypot, sin, cos, radians 


def get_effective_area(a, b, c):
	"""
	Calculate the area of a triangle made from the points a, b and c
	Simple 'secondary school' approach: area = half * base * height
	"""
	return abs( (a[0]-c[0]) * (b[1]-a[1]) - (a[0]-b[0]) * (c[1]-a[1]) ) * 0.5


def visvalingam_whyatt(node_list, n_nodes):
	"""
	* Simplify a line using point elimination based on effective area
	"""

	# calculate and store the effective area for each point, excluding the end points
	areas = [ {"point": node_list[i], "area": get_effective_area(node_list[i-1],
		node_list[i], node_list[i+1])} for i in range(1, len(node_list)-1) ]

	# add the end points back in
	areas.insert(0, {"point": node_list[0], "area": 0})
	areas.insert(len(areas), {"point": node_list[len(node_list)-1], "area": 0})

	# take a copy of the list so that we don't edit the original
	nodes = areas.copy()

	# keep going until we run out of nodes
	while len(nodes) > n_nodes:

		# init min area with a large number
		min_area = float("inf")

		# loop through every point, excluding the end points
		for i in range(1, len(nodes)-1):

			# if the effective area of this point is smaller than the previous minimum
			if nodes[i]['area'] < min_area:

				# store the mew minimum area and the index of the point
				min_area = nodes[i]['area']
				current_point = i

		# remove the current point from the list
		nodes.pop(current_point)

		# recalculate effective area to the left of the deleted node
		nodes[current_point-1]['area'] = get_effective_area(nodes[current_point-2]['point'],
			nodes[current_point-1]['point'], nodes[current_point]['point'])		# left

		# if there is a node to the right of the deleted node, recalculate the effective area
		if current_point < len(nodes)-1:
			nodes[current_point]['area'] = get_effective_area(nodes[current_point-1]['point'],
				nodes[current_point]['point'], nodes[current_point+1]['point'])	# right

	# extract the nodes and return
	return [ node['point'] for node in nodes ]


def border_calc(border):
    
    """ Calculate the border of LineString and Multistring types, if border 
    is a point return a really high number to diregard it as points have no length
    or area """
    
    cumulative_length = 0

    if border.geom_type == 'Point':
        
        cumulative_length = float("inf")
    
    elif border.geom_type == 'LineString':
        
        #convert linestring into subscriptable object
        segment = list(shape(border).coords)
        
        #calculate distance
        azF, azB, dist = g.inv(segment[0][0], segment[0][1],
                       segment[1][0], segment[1][1])
        cumulative_length = dist 
    
    else:
        
        for segment in list(border):
    
        	# calculate the forward azimuth, backward azimuth and direction of the current segment
        	azF, azB, distance = g.inv(segment.coords[0][0], segment.coords[0][1],
        		segment.coords[1][0], segment.coords[1][1])
    
    	# add the distance to our cumulative total
        	cumulative_length += distance
            
    return cumulative_length

def offset(x, y, distance, direction):
    """
    * Offset a location by a given distance and direction
    """
    x2 = x + cos(radians(direction)) * distance
    y2 = y + sin(radians(direction)) * distance
    return (x2, y2)


def evaluate_distortion(transformer, minx, miny, maxx, maxy, sample_number):
    """
    * Calculate a selection of distortion measures, based on Canters et al. (2005)
    *  and Gosling & Symeonakis (2020)
    """

    ''' FINITE AREAL AND SHAPE DISTORTION - Canters et al. (2005) '''

    # calculate the required number of random locations (x and y separately) plus radius
    xs = uniform(low=minx, high=maxx, size=sample_number)
    ys = uniform(low=miny, high=maxy, size=sample_number)
    rs = uniform(low=1000, high=1000000, size=sample_number)

    # offset distances
    forward_azimuths = arange(0, 360, 22.5)
    n = len(forward_azimuths)

    # loop through the points
    planar_areas = []
    shape_indices = []
    ellipsoidal_areas = []
    
    for x, y, r in zip(xs, ys, rs):

        # construct a circle around the centre point on the ellipsoid
        lons, lats, bAz = g.fwd([x]*n, [y]*n, forward_azimuths, [r]*n)

        # project the result, calculate area, append to the list
        e_coords = [ transformer.transform(lon, lat, direction='FORWARD') for lon, lat in zip(lons, lats) ]
        ellipsoidal_areas.append(Polygon(e_coords).area)

        # transform the centre point to the projected CRS
        px, py = transformer.transform(x, y, direction='FORWARD')

        # construct a circle around the projected point on a plane, calculate area, append to list
        p_coords = [ offset(px, py, r, az) for az in forward_azimuths ]
        planar_areas.append(Polygon(p_coords).area)

        # get radial distances frpm the centre to each of the 16 points on the circle
        ellipsoidal_radial_distances = [ hypot(px - ex, py - ey) for ex, ey in e_coords ]

        # get the sum of the distances, and the expected value for each distance
        total_radial_dist = sum(ellipsoidal_radial_distances)
        expected_distance = total_radial_dist / n

        # get the difference between the actual and expected radial distance for each 'spoke'
        shape_distortion = [ abs((expected_distance / total_radial_dist) - (d / total_radial_dist)) for d in ellipsoidal_radial_distances ]
        shape_indices.append(sum(shape_distortion))

    # calculate shape distortion
    Es = sum(shape_indices) / len(shape_indices)


    # calculate areal distortion
    diff_sum = 0
    for e, p in zip(ellipsoidal_areas, planar_areas):
        diff_sum += abs(e - p) / abs(e + p)
    Ea = 1 / sample_number * diff_sum
    Ka = (1 + Ea) / (1 - Ea)


    ''' FINITE DISTANCE DISTORTION - Gosling & Symeonakis (2020) '''

    # loop once per sample required
    planar_distances = []
    ellipsoidal_distances = []
    
    for i in range(sample_number):

        # get two random locations (x and y separately)
        xs = uniform(low=minx, high=maxx, size=2)
        ys = uniform(low=miny, high=maxy, size=2)

        # calculate the distance along the ellipsoid
        ellipsoidal_distances.append(g.line_length(xs, ys))

        # transform the coordinates
        origin = transformer.transform(xs[0], ys[0], direction='FORWARD')
        destination = transformer.transform(xs[1], ys[1], direction='FORWARD')

        # calculate the planar distance
        planar_distances.append(hypot(origin[0] - destination[0], origin[1] - destination[1]))

    # calculate distance distortion
    diff_sum = 0
    for e, p in zip(ellipsoidal_distances, planar_distances):
        diff_sum += abs(e - p) / abs (e + p)
    Ep = 1 / sample_number * diff_sum

    # return all of the measures
    return Ep, Es, Ea, Ka


# setting  Geodesic Ellipsoid projection for calulating border length
g = Geod(ellps='WGS84')

world = read_file("./natural-earth/ne_10m_admin_0_countries.shp")

#create a copy to manipluate 
test = world.copy()


#create country list to index in alogorithm
country_list = list(world.ISO_A3)

#set inital border to infinity
min_border = float("inf")

#initalize no value for countries being compared.
country_a = 0
country_b = 0

#create spatial index 
idx = index.Index()
for id, ctry in test.iterrows():
    idx.insert(id, ctry.geometry.bounds)


# initialise while loop 
while len(test.index) >= 1:
    
    for a in range(len(test.index)):
        
        #get country geometry
        country_a_geom = test.loc[(world.ISO_A3 == country_list[a])].geometry.iloc[0]
        
        #get bounding box
        coords = country_a_geom.bounds
        
        #remove country from spatial index so it doesnt compare to itself and to gradually 
        #reduce spatial index to avoid comparing countries twice
        idx.delete(a, coords)
        
        #get possible matches
        possible_matches = test.loc[list(idx.intersection(coords))]
        
        #if there are no matches from spatial index, drop country from df
        if len(possible_matches) == 0:
            
            test = test.drop(index = a)
            
            continue 
        
        #get precise matches using intersects function which returns an array of boolean values 
        #which can be used to index possible matches to get countries which actually 
        #border with country a
        precise_matches = possible_matches.loc[possible_matches.intersects(country_a_geom)]
        
         # if country has no neighbours, skip it rather than do any calculation. 
        if len(precise_matches) == 0:
            
            test = test.drop(index = a)
            
            continue 
            
        #get a list of neighbours 
        neighbours = list(precise_matches.ISO_A3) 
        
        #remove country from test daataframe to avoid countries being compared twice 
        # and to reduce number of values in index to complete while loop.
        test = test.drop(index = a)
        

        # if country does have neighbours calculate borders with neighbours 
        for b in neighbours:
        #get geomtry of neighbouring country
            country_b_geom =  precise_matches.loc[(precise_matches.ISO_A3 == b)].geometry.iloc[0]
            
            # get coords of border 
            border = country_a_geom.intersection(country_b_geom)
            
            #calculate border distance 
            dist = border_calc(border)
            
            #see what its the is being done 
            #print(dist,world.NAME_EN.iloc[a], precise_matches['NAME_EN'][(precise_matches.ISO_A3 == b)])
            
            #save boarder, and the two corresponding country codes it is shorter than the last
            if dist < min_border:
                min_border = dist
                country_a = country_list[a]
                country_b = b

#print countries and border
print(f'Border between {country_a} and {country_b} =  {min_border}  (ellipsoidal projection)')


#evaluate distortion of WSG84 projection for distances and local Italian projection
geo_string = "+proj=longlat +datum=WGS84 +no_defs"
italy_proj = "+proj=tmerc +lat_0=0 +lon_0=12 +k=0.9985000000000001 +x_0=7000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "


#get relevant
italy = world.loc[(world.ISO_A3 == country_a)]
vatican = world.loc[(world.ISO_A3 == country_b)]

#set transformer to compare two projections 
transformer = Transformer.from_crs(CRS.from_proj4(geo_string), CRS.from_proj4(italy_proj), always_xy=True)

# get the bounds of italy
minx_ita, miny_ita, maxx_ita, maxy_ita = italy.total_bounds
# get the bounds of the vatican 
minx_vat, miny_vat, maxx_vat, maxy_vat = vatican.total_bounds

# calculate the distortion
Ep_i, Es_i, Ea_i, Ka_i = evaluate_distortion(transformer, minx_ita, miny_ita, maxx_ita, maxy_ita, 1000)

# report to Distortion of Italy
print('Italy Polygon Distortion \n')
print(f"Distance distortion: {Ep_i:.6f}")
print(f"Shape distortion: {Es_i:.6f}")
print(f"Area distortion: {Ea_i:.6f}")
print(f"Scale factor : {Ka_i:.6f}")

# open a dataset of all contries in the world
# calculate the distortion
Ep_v, Es_v, Ea_v, Ka_v = evaluate_distortion(transformer, minx_vat, miny_vat, maxx_vat, maxy_vat, 1000)

# report to Vatican Distortion
print('\nVatican Polygon Distortion \n')
print(f"Distance distortion {Ep_v:.6f}")
print(f"Shape distortion {Es_v:.6f}")
print(f"Area distortion {Ea_v:.6f}")
print(f"Scale factor {Ka_v:.6f}")

#project using the local map projections because they tend to be more accurate
italy_geom = italy.to_crs(italy_proj).geometry.iloc[0]
vatican_geom = vatican.to_crs(italy_proj).geometry.iloc[0]

#as border is equal to the perimiter of the vatican polygon the length function can be used rather than 
# interection
border_dist = vatican_geom.length

# compare distance between Local Projection and Ellipsoidal Projection for border distance
print(f'\nBorder Distance using Italian Projection: {border_dist} (Difference = {min_border - border_dist}) ')

#generalisation of Coast of Italy to show where Vatican is with respect to whole country.
# set the percentage of nodes that you want to remove

#specified percentage of nodes which will remain after generalisation 
SIMPLIFICATION_PERC = 95

# obtaining only mainland Italy by interating through multipolygon and selecting
#polygon with the highest area 
coord_list = []
biggest_area = 0

for poly in italy_geom:

 	# if it is the biggest so far, update biggest area store the coordinates
    if poly.area > biggest_area:
        biggest_area = poly.area
        coord_list = list(poly.exterior.coords)



#specify number of nodees according to the percentage you want to keep 
n_nodes = int(len(coord_list) / 100.0 * (100 - SIMPLIFICATION_PERC))

# remove one node and overwrite it with the new, shorter list
simplified_nodes = visvalingam_whyatt(coord_list, n_nodes)

# create map axis object
my_fig, (my_ax, my_ax1) = subplots(1, 2, figsize=(16, 10))

# remove axes
my_ax.axis('off')

# set title
my_ax.set(title= f"The smallest boarder between two countries: Italy and the Vatican: {border_dist/ 1000:.4f} km long.")

buffer = 100
my_ax.set_xlim([vatican_geom.bounds[0] - buffer, vatican_geom.bounds[2] + buffer])
my_ax.set_ylim([vatican_geom.bounds[1] - buffer, vatican_geom.bounds[3] + buffer])

# plot data
italy.to_crs(italy_proj).plot(
    ax = my_ax,
    color = '#f0e0e0',
    edgecolor = '#660000',
    linewidth = 0.5,
    )
vatican.to_crs(italy_proj).plot(
    ax = my_ax,
    color = '#e0f0e0',
    edgecolor = '#21209e',
    linewidth = 1.0,
    )

# manually draw a legend
my_ax.legend([
    Patch(facecolor='#f0e0e0', edgecolor='#660000', label='Italy'),
    Patch(facecolor= '#e0f0e0', edgecolor='#006600', label='Vatican')],
	['Italy', 'Vatican'], loc='upper left')

# add north arrow
x, y, arrow_length = 0.98, 0.99, 0.1
my_ax.annotate('N', xy=(x, y), xytext=(x, y-arrow_length),
	arrowprops=dict(facecolor='black', width=5, headwidth=15),
	ha='center', va='center', fontsize=20, xycoords=my_ax.transAxes)

# add scalebar
my_ax.add_artist(ScaleBar(dx=1, units="m", location="lower left", length_fraction=0.25))

#remove axes
my_ax1.axis('off')

# set title
my_ax1.set(title= "Simplified Coast Line of Italy and Point Location of the Vatican")

# plot country
GeoSeries(Polygon(simplified_nodes)).plot(
    ax = my_ax1,
    color = '#f0e0e0',
    edgecolor = '#660000',
    linewidth = 0.5,
    )

# plot planar circle
GeoSeries(vatican_geom.centroid).plot(
    ax = my_ax1,
    marker = 'o',
    color = 'black',
    alpha = 0.5,
    edgecolor = '#006600',
    markersize = 50,
    )

# add north arrow
x, y, arrow_length = 0.98, 0.99, 0.1
my_ax1.annotate('N', xy=(x, y), xytext=(x, y-arrow_length),
 	arrowprops=dict(facecolor='black', width=5, headwidth=15),
 	ha='center', va='center', fontsize=20, xycoords=my_ax1.transAxes)

# add scalebar
my_ax1.add_artist(ScaleBar(dx=1, units="m", location="lower left", length_fraction=0.25))

savefig(f'out/Vatican_Italy.png', bbox_inches='tight')

print(f"completed in: {time() - start_time} seconds")	# NO CODE BELOW HERE

