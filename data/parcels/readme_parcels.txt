This folder contains classified and non-classified shapefiles of all parcels in the DC and Boston area that intersect with solar panels that have either been manually drawn by hand or 
automatically detected by ORNL's automatic detection algorithm. For more details about how these parcel datasets have been developed see the report.  The following text describes 
each subfolder in greater detail:

subfolders:

1.) "classified" - contains the DC and Boston parcel shapefiles that have parcels that have been classified as residential or commercial. only parcels that intersect with 
either solar panels manually identified by hand or automatically identified by ORNL's automatic detection algorithm are included.

2.) "not_classified" - contains the DC and Boston parcel shapefiles that have parcels that have not yet been classified as residential or commercial. only parcels that intersect with 
either solar panels manually identified by hand or automatically identified by ORNL's automatic detection algorithm are included.

subsubfolders:

3.) "boston" - contains the Boston parcel shapefiles that have parcels that have either not yet been classified or have been classified as residential or commercial. only parcels 
that intersect with either solar panels manually identified by hand or automatically identified by ORNL's automatic detection algorithm are included.

4.) "dc" - contains the DC parcel shapefiles that have parcels that have either not yet been classified or have been classified as residential or commercial. only parcels 
that intersect with either solar panels manually identified by hand or automatically identified by ORNL's automatic detection algorithm are included. 

subsubsubfolders:

5.) "parcels_aut_city" - contains shapefiles with parcels containing solar panels that were automatically detected by ORNL's automatic detection algorithm
for either city = boston or city = dc. these shapefiles have not yet been classified. 

6.) "parcels_man_city" - contains shapefiles with parcels containing solar panels that were manually detected by hand for either city = boston or city = dc. 
these shapefiles have not yet been classified. 

7.) "parcels_class_aut_city" - contains shapefiles with parcels containing solar panels that were automatically detected by ORNL's automatic detection algorithm for either 
city = boston, city = dc, or city = combo (combo is the case when the classification algorithm was trained on DC and applied to Boston). these shapefiles were
classified. 

8.) "parcels_class_man_city" - contains shapefiles with parcels containing solar panels that were manually detected by hand for either city = boston or city = dc.
these shapefiles were classified. 
