DO $$
BEGIN
RAISE EXCEPTION 'trapped running the script - comment me out to really run everything';
END $$;

-- Requires data downloaded from DfT
-- see /dev/get_dft_data.R

-- if starting from scratch
DROP SCHEMA traffic_gb CASCADE;
CREATE SCHEMA traffic_gb;

-- Traffic (km)

CREATE TABLE gb_traffic (
	id serial PRIMARY KEY,
	local_authority_id int4,
	local_authority_name text,
	year int2,
	link_length_km numeric,
	link_length_miles numeric,
	cars_and_taxis numeric,
	all_motor_vehicles numeric
	);

COPY gb_traffic (local_authority_id, local_authority_name, year, link_length_km, link_length_miles, cars_and_taxis, all_motor_vehicles)
FROM 'C:/Users/myoun/git-repos/gbtraffic/dev/dft_csv/gb_traffic.csv'
DELIMITER ','
CSV HEADER;

-- AADF by direction

CREATE TABLE "aadf_by_direction" (
  id serial PRIMARY KEY,
	"count_point_id" int4,
  "year" int2,
	"region_id" int2,
	"local_authority_id" int2,
  "direction_of_travel" varchar(1),
  "estimation_method" text,
  "estimation_method_detail" text,
  "pedal_cycles" int4,
  "two_wheeled_mv" int4,
  "cars_and_taxis" int4,
  "buses_and_coaches" int4,
  "lgvs" int4,
  "hgvs_2_rigid" int4,
  "hgvs_3_rigid" int4,
  "hgvs_4plus_rigid" int4,
  "hgvs_3or4_artic" int4,
  "hgvs_5_artic" int4,
  "hgvs_6_artic" int4,
  "hgvs_all" int4,
  "all_mv" int4,
  "data_quality" int2
);

COPY aadf_by_direction (	"count_point_id",
  "year",
	"region_id",
	"local_authority_id",
	"direction_of_travel",
  "estimation_method",
  "estimation_method_detail",
  "pedal_cycles",
  "two_wheeled_mv",
  "cars_and_taxis",
  "buses_and_coaches",
  "lgvs",
  "hgvs_2_rigid",
  "hgvs_3_rigid",
  "hgvs_4plus_rigid",
  "hgvs_3or4_artic",
  "hgvs_5_artic",
  "hgvs_6_artic",
  "hgvs_all",
  "all_mv")
FROM 'C:/Users/myoun/git-repos/gbtraffic/dev/dft_csv/gb_aadf_dir_import.csv'
DELIMITER ','
CSV HEADER;


-- countpoints

CREATE TABLE "countpoints" (
  "count_point_id" int4 PRIMARY KEY,
  "region_id" int4,
	"region_name" text,
  "local_authority_id" int4,
	"local_authority_name" text,
  "easting" int4,
  "northing" int4,
  "latitude" numeric,
  "longitude" numeric,
  "geom" geometry(Point, 27700) DEFAULT NULL,
  "road_type" text,
	"road_name"	text,
	"start_junction_road_name"	text,
	"end_junction_road_name"	text,
	"length_km"	numeric,
	"length_miles"	numeric,
  CONSTRAINT "check_road_type" CHECK (road_type IN ('major', 'minor') )
);
COMMENT ON CONSTRAINT "check_road_type" ON "countpoints" IS 'Road type must be major or minor';

COPY countpoints(count_point_id, region_id, region_name, local_authority_id, local_authority_name, easting, northing, latitude, longitude, road_type, road_name, start_junction_road_name, end_junction_road_name, length_km, length_miles)
FROM 'C:/Users/myoun/git-repos/gbtraffic/dev/dft_csv/countpoints.csv'
DELIMITER ','
CSV HEADER;

-- check for countpoints that exist in aadf_by_direction but not in the countpoints table
select * from aadf_by_direction where count_point_id not in (select distinct count_point_id from countpoints);

-- populate geom
update countpoints
set geom = ST_GeomFromText('POINT('||easting||' '||northing||')', 27700);

-- index geom
CREATE INDEX countpoints_geom_idx
  ON countpoints
  USING GIST (geom);

ALTER TABLE "aadf_by_direction" ADD CONSTRAINT "fk_aadf" FOREIGN KEY ("count_point_id") REFERENCES "countpoints" ("count_point_id") ON DELETE NO ACTION ON UPDATE NO ACTION;


-- raw counts
CREATE TABLE "raw_counts" (
  id serial PRIMARY KEY,
	"count_point_id" int4 NOT NULL,
  "year" int2,
  "count_date" date,
  "direction_of_travel" varchar(1),
  "count_hour" int2,
  "pedal_cycles" int4,
  "two_wheeled_mv" int4,
  "cars_and_taxis" int4,
  "buses_and_coaches" int4,
  "lgvs" int4,
  "hgvs_2_rigid" int4,
  "hgvs_3_rigid" int4,
  "hgvs_4plus_rigid" int4,
  "hgvs_3or4_artic" int4,
  "hgvs_5_artic" int4,
  "hgvs_6_artic" int4,
  "hgvs_all" int4,
  "all_mv" int4
);

COPY raw_counts ("count_point_id",
  "year",
	"count_date",
	"direction_of_travel",
	"count_hour",
  "pedal_cycles",
  "two_wheeled_mv",
  "cars_and_taxis",
  "buses_and_coaches",
  "lgvs",
  "hgvs_2_rigid",
  "hgvs_3_rigid",
  "hgvs_4plus_rigid",
  "hgvs_3or4_artic",
  "hgvs_5_artic",
  "hgvs_6_artic",
  "hgvs_all",
  "all_mv")
from 'C:/Users/myoun/git-repos/gbtraffic/dev/dft_csv/gb_raw_counts_import.csv'
DELIMITER ','
CSV HEADER;

ALTER TABLE "raw_counts" ADD CONSTRAINT "fk_raw_counts" FOREIGN KEY ("count_point_id") REFERENCES "countpoints" ("count_point_id") ON DELETE NO ACTION ON UPDATE NO ACTION;

-- update raw counts direction_of_travel ensure all upper case
update raw_counts
set direction_of_travel = upper(direction_of_travel);


-- major roads

-- tables for each year generated from R see /dev/get_dft_data.R
-- going to use 2023 only
-- create table with new column names

create table major_road_links as
select "CP_Number" as link_id, "RoadNumber" as road_number, geometry as geom from major_road_links_2023;

-- Now to deal automatically with countpoints that are missing from the major_road_links

create MATERIALIZED view mv_missing_countpoints as
select * from countpoints
where count_point_id not in
(select link_id from major_road_links)
and road_type = 'major';

CREATE INDEX missing_countpoints_geom_idx
  ON mv_missing_countpoints
  USING GIST (geom);

	-- 3380 based on 2023 data
	-- we are only going to automatically fix those that are in an identical location to other countpoints
	-- lets identfy those (1144 in 2023)

alter table major_road_links
add column alternative_count_ids int[];

with tmp as
(
 select a.count_point_id as missing_countpoint, b.count_point_id as matching_countpoint from mv_missing_countpoints a
 left join countpoints b on st_intersects(a.geom, b.geom)
 -- don't want to match on itself
 where a.count_point_id != b.count_point_id
 -- don't want to match to a missing count_point_id!
 and b.count_point_id not in (select count_point_id from mv_missing_countpoints)
 ), tmp2 as
(
select a.matching_countpoint, array_agg(a.missing_countpoint) as alternative_countpoints
from tmp a
group by matching_countpoint)
update major_road_links
set alternative_count_ids = tmp2.alternative_countpoints
from tmp2
where link_id = tmp2.matching_countpoint;

-- views

CREATE SEQUENCE view_aadf_vid_seq CYCLE;
ALTER SEQUENCE  view_aadf_vid_seq RESTART WITH 1;

CREATE VIEW "view_aadf_anydir" AS select
	nextval('view_aadf_vid_seq'::regclass) AS vid,
	a.count_point_id,
	year,
	max (a.local_authority_id) as local_authority_id,
	max (a.region_id) as region_id,
	max (b.length_km) as length_km,
	max (b.length_miles) as length_miles,
	max (b.road_type) as road_type,
	max (estimation_method) as estimation_method,
	max (estimation_method_detail) as estimation_method_detail,
	sum ( pedal_cycles ) as pedal_cycles,
	sum ( two_wheeled_mv ) as two_wheeled_mv,
	sum ( cars_and_taxis ) as cars_and_taxis,
	sum ( buses_and_coaches ) as buses_and_coaches,
	sum ( lgvs ) as lgvs,
	sum ( hgvs_2_rigid ) as hgvs_2_rigid,
	sum ( hgvs_3_rigid ) as hgvs_3_rigid,
	sum ( hgvs_4plus_rigid ) as hgvs_4plus_rigid,
	sum ( hgvs_3or4_artic ) as hgvs_3or4_artic,
	sum ( hgvs_5_artic ) as hgvs_5_artic,
	sum ( hgvs_6_artic ) as hgvs_6_artic,
	sum ( hgvs_all ) as hgvs_all,
	sum ( all_mv ) as all_mv,
	max (data_quality) as data_quality
from
	aadf_by_direction a
left join countpoints b on a.count_point_id = b.count_point_id
GROUP BY
	a.count_point_id,
	year
order by
	a.count_point_id asc,
	year asc;


CREATE VIEW "view_raw_counts_anydir" AS select
	count_point_id,
	count_hour,
	year,
	count_date,
	sum ( pedal_cycles ) as pedal_cycles,
	sum ( two_wheeled_mv ) as two_wheeled_mv,
	sum ( cars_and_taxis ) as cars_and_taxis,
	sum ( buses_and_coaches ) as buses_and_coaches,
	sum ( lgvs ) as lgvs,
	sum ( hgvs_2_rigid ) as hgvs_2_rigid,
	sum ( hgvs_3_rigid ) as hgvs_3_rigid,
	sum ( hgvs_4plus_rigid ) as hgvs_4plus_rigid,
	sum ( hgvs_3or4_artic ) as hgvs_3or4_artic,
	sum ( hgvs_5_artic ) as hgvs_5_artic,
	sum ( hgvs_6_artic ) as hgvs_6_artic,
	sum ( hgvs_all ) as hgvs_all,
	sum ( all_mv ) as all_mv
from
	raw_counts
GROUP BY
	count_point_id,
	year,
	count_date,
count_hour
order by
	count_point_id asc,
	count_date asc,
count_hour asc;

CREATE VIEW "view_raw_counts_12hr_totals" AS select
	count_point_id,
	year,
	count_date,
	sum ( pedal_cycles ) as pedal_cycles,
	sum ( two_wheeled_mv ) as two_wheeled_mv,
	sum ( cars_and_taxis ) as cars_and_taxis,
	sum ( buses_and_coaches ) as buses_and_coaches,
	sum ( lgvs ) as lgvs,
	sum ( hgvs_2_rigid ) as hgvs_2_rigid,
	sum ( hgvs_3_rigid ) as hgvs_3_rigid,
	sum ( hgvs_4plus_rigid ) as hgvs_4plus_rigid,
	sum ( hgvs_3or4_artic ) as hgvs_3or4_artic,
	sum ( hgvs_5_artic ) as hgvs_5_artic,
	sum ( hgvs_6_artic ) as hgvs_6_artic,
	sum ( hgvs_all ) as hgvs_all,
	sum ( all_mv ) as all_mv
from
	view_raw_counts_anydir
GROUP BY
	count_point_id,
	year,
	count_date
order by
	count_point_id asc,
	count_date asc;

-- materialized views

-- aadf_anydir
create materialized view mv_aadf_anydir as
select * from view_aadf_anydir;

-- raw_counts_anydir
create materialized view mv_raw_counts_anydir as
select * from view_raw_counts_anydir;

-- la_traffic

-- first update gb_traffic to add region_id field

alter table gb_traffic
add column region_id int;

with tmp as (
SELECT distinct local_authority_id, region_id from countpoints order by local_authority_id asc
)
update gb_traffic
set region_id = b.region_id
from tmp b
where gb_traffic.local_authority_id = b.local_authority_id;


-- materialized view for local authority traffic km
CREATE SEQUENCE mview_la_traffic_oid_seq CYCLE;
ALTER SEQUENCE mview_la_traffic_oid_seq RESTART WITH 1;

create materialized view mv_la_traffic as

with tmp as (
SELECT *, q2.all_mv_traffic_all_roads - q1.all_mv_calc_major_traffic as all_mv_calc_minor_traffic, q2.cars_taxis_traffic_all_roads - q1.cars_taxis_calc_major_traffic as cars_taxis_calc_minor_traffic
FROM
( select count(*) as major_countpoints_sum, year, local_authority_id, sum(length_miles) as major_link_length,
 sum(all_mv * length_miles * 365) as "all_mv_calc_major_traffic",
 sum(cars_and_taxis * length_miles * 365) as "cars_taxis_calc_major_traffic"
 from view_aadf_anydir where road_type = 'major' group by local_authority_id, year) q1

LEFT JOIN

( select year as traffic_year, local_authority_id as traffic_la_id, link_length_miles as total_link_length, all_motor_vehicles as all_mv_traffic_all_roads,  cars_and_taxis as cars_taxis_traffic_all_roads  from gb_traffic) q2
ON q1.local_authority_id = q2.traffic_la_id AND q1.year = q2.traffic_year

LEFT JOIN

(select count(*) as all_countpoints_sum, local_authority_id as cp_id, year as cp_year  from view_aadf_anydir GROUP BY local_authority_id, year) q3
ON q3.cp_id = q1.local_authority_id and q3.cp_year = q1.year

order by q1.local_authority_id asc

)
, tmp2 as (
SELECT *, q2.all_mv_traffic_all_roads - q1.all_mv_calc_major_traffic as all_mv_calc_minor_traffic, q2.cars_taxis_traffic_all_roads - q1.cars_taxis_calc_major_traffic as cars_taxis_calc_minor_traffic
FROM
( select count(*) as major_countpoints_sum, year, region_id, sum(length_miles) as major_link_length,
 sum(all_mv * length_miles * 365) as "all_mv_calc_major_traffic",
 sum(cars_and_taxis * length_miles * 365) as "cars_taxis_calc_major_traffic"
 from view_aadf_anydir where road_type = 'major' group by region_id, year) q1

 LEFT JOIN

( select year as traffic_year, region_id as traffic_region_id, sum(link_length_miles) as total_link_length, sum(all_motor_vehicles) as all_mv_traffic_all_roads,  sum(cars_and_taxis) as cars_taxis_traffic_all_roads  from gb_traffic group by region_id, year) q2
ON q1.region_id = q2.traffic_region_id AND q1.year = q2.traffic_year

LEFT JOIN

(select count(*) as all_countpoints_sum, region_id as cp_id, year as cp_year from view_aadf_anydir GROUP BY region_id, year) q3
ON q3.cp_id = q1.region_id and q3.cp_year = q1.year

order by q1.region_id asc
)
select nextval( 'mview_la_traffic_oid_seq' :: regclass ) AS oid, year, local_authority_id as traffic_area_id, all_countpoints_sum, major_countpoints_sum, total_link_length, major_link_length, total_link_length - major_link_length as minor_link_length, all_mv_traffic_all_roads, cars_taxis_traffic_all_roads, all_mv_calc_major_traffic, cars_taxis_calc_major_traffic, all_mv_calc_minor_traffic, cars_taxis_calc_minor_traffic from tmp q1
UNION
select nextval( 'mview_la_traffic_oid_seq' :: regclass ) AS oid, year, region_id + 10000 as traffic_area_id, all_countpoints_sum, major_countpoints_sum, total_link_length, major_link_length, total_link_length - major_link_length as minor_link_length, all_mv_traffic_all_roads, cars_taxis_traffic_all_roads, all_mv_calc_major_traffic, cars_taxis_calc_major_traffic, all_mv_calc_minor_traffic, cars_taxis_calc_minor_traffic from tmp2 q2
order by traffic_area_id, year asc;


-- materialized view for visualising aadf on edge network
-- need to amend region_id to match regions list used in the Shiny App

CREATE SEQUENCE mview_aadf_roads_oid_seq CYCLE;
ALTER SEQUENCE mview_aadf_roads_oid_seq RESTART WITH 1;

create materialized view mv_aadf_roads as
	select
		nextval( 'mview_aadf_roads_oid_seq' :: regclass ) AS oid,
		a.count_point_id,
		a.year,
		a.local_authority_id,
		a.region_id + 10000 as region_id,
		to_date( a.year :: varchar, 'yyyymmdd' ) as date,
		a.estimation_method,
		a.data_quality,
		b.link_id,
		pedal_cycles,
	a.two_wheeled_mv,
	a.cars_and_taxis,
	a.buses_and_coaches,
	a.lgvs,
	a.hgvs_2_rigid,
	a.hgvs_3_rigid,
	a.hgvs_4plus_rigid,
	a.hgvs_3or4_artic,
	a.hgvs_5_artic,
	a.hgvs_6_artic,
	a.hgvs_all,
	a.all_mv,
	b.geom
	from
		view_aadf_anydir a
		inner join major_road_links b
		-- join where count_point_id matches a link_id or any of the alternative_count_ids.
		on a.count_point_id = b.link_id or a.count_point_id = ANY (b.alternative_count_ids)
	order by
		count_point_id;


create unique index on mv_aadf_roads (oid);

CREATE INDEX aadf_roads_geom_idx
  ON mv_aadf_roads
  USING GIST (geom);


-- indexes on count_point_id

CREATE INDEX aadf_by_direction_idx_count_point_id ON aadf_by_direction (count_point_id);
CREATE INDEX raw_counts_idx_count_point_id ON raw_counts (count_point_id);
CREATE INDEX mv_aadf_anydir_idx_count_point_id ON mv_aadf_anydir (count_point_id);
CREATE INDEX mv_raw_counts_anydir_idx_count_point_id ON mv_raw_counts_anydir (count_point_id);
CREATE INDEX mv_la_traffic_idx_traffic_area_id ON mv_la_traffic (traffic_area_id);

-- set permissions
GRANT USAGE ON SCHEMA traffic_gb TO gbtraffic;
GRANT USAGE ON SEQUENCE traffic_gb.view_aadf_vid_seq TO gbtraffic;
GRANT SELECT ON traffic_gb.aadf_by_direction TO gbtraffic;
GRANT SELECT ON traffic_gb.major_road_links TO gbtraffic;
GRANT SELECT ON traffic_gb.countpoints TO gbtraffic;
GRANT SELECT ON traffic_gb.gb_traffic TO gbtraffic;
GRANT SELECT ON traffic_gb.raw_counts TO gbtraffic;
GRANT SELECT ON traffic_gb.view_aadf_anydir TO gbtraffic;
GRANT SELECT ON traffic_gb.view_raw_counts_12hr_totals TO gbtraffic;
GRANT SELECT ON traffic_gb.view_raw_counts_anydir TO gbtraffic;
GRANT SELECT ON traffic_gb.mv_aadf_roads TO gbtraffic;
GRANT SELECT ON traffic_gb.mv_aadf_anydir TO gbtraffic;
GRANT SELECT ON traffic_gb.mv_raw_counts_anydir TO gbtraffic;
GRANT SELECT ON traffic_gb.mv_la_traffic TO gbtraffic;
