# gbtraffic 1.2.2

* Minor code refactor

# gbtraffic 1.2.1

* Improve CSV file download for traffic data ensuring that scientific notation is never used for all_mv_traffic_all_roads and cars_taxis_traffic_all_roads

# gbtraffic 1.2.0

* Data for 2023 added

# gbtraffic 1.1.7

* fix bug with selecting multiple count drop down, due to stray ending comma in a req()

# gbtraffic 1.1.5

* removed Cronitor RUM script
* add clicky traffic tracking

# gbtraffic 1.1.4

* Added help modal for AADF major road links

# gbtraffic 1.1.3

* Added Cronitor RUM script - for user stats

# gbtraffic 1.1.2

* Fixed bug where application would crash if the currently selected count point marker was clicked
* Fixed warning messages for missing values when generating traffic % change plots (no data for base year)
* Consistent use of 'count point' rather than 'countpoint'
* Alter count point download so that local_authority_id and region_id numbers are replaced by the names

# gbtraffic 1.1.0

* Fixed various issues highlighted by the W3C Markup Validation Service

# gbtraffic 1.0.5

* Add meta description tag

# gbtraffic 1.0.4

* Enhanced filtering by road type and vehicle class for Traffic Volume data.

# gbtraffic 1.0.3

* refactored on/off behaviour of the countpoints and AADF Edges layers. This was to ensure that
where practicable the layer is simply hidden and not cleared, meaning it can be displayed again
without the overhead of the data being added to the map again.

# gbtraffic 1.0.2

* Fix issue wtih cycle scaling factor

# gbtraffic 1.0.1

* Separate aadf major links class control from the countpoints class control
* Add conditional panels to hide options for the map layers until they are selected to be shown on the map

# gbtraffic 1.0.0

Initial release
