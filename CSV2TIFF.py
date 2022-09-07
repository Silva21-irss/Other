# Convert CSV file to raster
import arcpy, timeit
from arcpy import env
from arcpy.sa import *
workspace = "Y://ByProject//Silv21//Climate_30m//Acadia//New"
arcpy.env.workspace = workspace # Set environment settings
out_table = "Y://ByProject//Silv21//Climate_30m//Acadia//New//"

myCSVs = arcpy.ListFiles("*.csv") # Get all relevant CSV files

for i in myCSVs:
    arcpy.management.XYTableToPoint(workspace + "//" + i,out_table + i[0:-4],'Longitude','Latitude','Elevation',arcpy.SpatialReference(4326))

mySHPs = arcpy.ListFeatureClasses('*.shp')

for i in mySHPs:
    clipped = out_table + i[0:-4] + "_clip.shp"
    arcpy.Clip_analysis(i,"D:/Shapefiles/Acadia_Boundary.shp",clipped)
    arcpy.conversion.PointToRaster(clipped,'Value',out_table + i[0:-4] + ".tif",'','',"0.000388") # That last parameter, you can play around with changing the cell size, but leave it alone for the time being


# IF LINES EXIST IN THE DATA
##mySHPs = arcpy.ListFeatureClasses('*_clip.shp')
##
##for i in mySHPs:
##    arcpy.conversion.PointToRaster(i,'Value',out_table + i[0:-4] + ".tif",'','',"0.00045") # That last parameter, you can play around with changing the cell size, but leave it alone for the time being
