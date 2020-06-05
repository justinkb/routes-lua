----------------------------------
--[[
Ant Colony Optimization (ACO) for Travelling Salesman Problem (TSP)
for Routes (a World of Warcraft addon)

Copyright (C) 2011 Xinhuan

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA  02110-1301, USA.
]]

---------------------------------
--[[
Ant Colony Optimization and the Travelling Salesman Problem

The Travelling Salesman Problem (TSP) consists of finding the shortest tour
between n cities visiting each once only and ending at the starting point. Let
d(i,j) be the distance between cities i and j and t(i,j) the amount of pheromone
on the edge that connects i and j. t(i,j) is initially set to a small value
t(0), the same for all edges (i,j). The algorithm consists of a series of
iterations.

One iteration of the simplest ACO algorithm applied to the TSP can be summarized
as follows: (1) a set of m artificial ants are initially located at randomly
selected cities; (2) each ant, denoted by k, constructs a complete tour,
visiting each city exactly once, always maintaining a list J(k) of cities that
remain to be visited; (3) an ant located at city i hops to a city j, selected
among the cities that have not yet been visited, according to probability
p(k,i,j) = (t(i,j)^a * d(i,j)^-b) / sum(t(i,l)^a * d(i,l)^-b, all l in J(k))
where a and b are two positive parameters which govern the respective influences
of pheromone and distance; (4) when every ant has completed a tour, pheromone
trails are updated: t(i,j) = (1-p) * t(i,j) + D(t(i,j)), where p is the
evaporation rate and D(t(i,j)) is the amount of reinforcement received by edge
(i,j). D(t(i,j)) is proportional to the quality of the solutions in which (i,j)
was used by one ant or more. More precisely, if L(k) is the length of the tour
T(k) constructed by ant k, then D(t(i,j)) = sum(D(t(k,i,j)), 1 to m) with
D(t(k,i,j)) = Q / L(k) if (i,j) is in T(k) and D(t(k,i,j)) = 0 otherwise, where
Q is a positive parameter. This reinforcement procedure reflects the idea that
pheromone density should be lower on a longer path because a longer trail is
more difficult to maintain.

Steps (1) to (4) are repeated either a predefined number of times or until a
satisfactory solution has been found. The algorithm works by reinforcing
portions of solutions that belong to good solutions and by applying a
dissipation mechanism, pheromone evaporation, which ensures that the system does
not converge early toward a poor solution. When a = 0, the algorithm implements
a probabilistic greedy search, whereby the next city is selected solely on the
basis of its distance from the current city. When b = 0, only the pheromone is
used to guide the search, which would react the way the ants do it. However, the
explicit use of distance as a criterion for path selection appears to improve
the algorithm's performance. In all other optimization applications also, an
improvement in the algorithm's performance is observed when a local measure of
greed, similar to the inverse of distance for the TSP, is included into the
local selection of portions of solution by the agents. Typical parameter values
are: m = n, a = 1, b = 5, p = 0.5, t(0) = 1e-6.

-- Inspiration for optimization from social insect behaviour
-- by E. Bonabeau, M. Dorigo & G. Theraulaz
-- NATURE, VOL 406, 6 JULY 2000, www.nature.com
]]

-- Note:
-- The functions in this file are written specifically for use with Routes
-- in mind and is not a general TSP library.

-- TSP:ClusterRoute(nodes, zoneID, radius)
-- Arguments
--   nodes    - The table containing a list of Routes node IDs to path
--              This list should only contain nodes on the same map. This
--              table should be indexed numerically from nodes[1] to nodes[n].
--   zoneID   - The map area ID the route is in
--   radius   - The radius in yards to cluster
-- Returns
--   path     - The result TSP path is a table indexed numerically from path[1]
--              to path[n], a list of Routes node IDs. n is usually smaller than
--              the original input
--   metadata - The metadata table for path[] containing the original nodes
--              clustered
--   length   - The length of the new route in yards
-- Notes: The original table sent in is unmodified. New tables are returned.
--[[
Hierarchical Agglomerative Clustering

Data clustering algorithms can be hierarchical or partitional. Hierarchical
algorithms find successive clusters using previously established clusters,
whereas partitional algorithms determine all clusters at once. Hierarchical
algorithms can be agglomerative ("bottom-up") or divisive ("top-down").
Agglomerative algorithms begin with each element as a separate cluster and
merge them into successively larger clusters. Divisive algorithms begin with
the whole set and proceed to divide it into successively smaller clusters.

This method (Agglomerative) builds the hierarchy from the individual elements
by progressively merging clusters. The first step is to determine which
elements to merge in a cluster. Usually, we want to take the two closest
elements, according to the chosen distance.

Optionally, one can also construct a distance matrix at this stage, where the
number in the i-th row j-th column is the distance between the i-th and j-th
elements. Then, as clustering progresses, rows and columns are merged as the
clusters are merged and the distances updated. This is a common way to
implement this type of clustering, and has the benefit of catching distances
between clusters.

-- From Wikipedia, Cluster analysis
-- http://en.wikipedia.org/wiki/Cluster_analysis
-- 25 January 2008
]]
	local nodes = {} -- copy and paste the contents of the ["route"] to cluster from your Routes.lua SavedVariables here
	local weight = {} -- weight matrix
	local metadata = {} -- metadata after clustering

	local numNodes = #nodes
	local zoneW, zoneH = 0.0, 0.0 -- get these from inside WoW by running /script print(Routes.Dragons:GetZoneSize(<insert zoneID here>))
	local radius = 60 -- replace this with the cluster distance of your preference
	local diameter = radius * 2
	--local taboo = 0

	-- some helpers
	local inf = math.huge
	local tinsert = table.insert
	local tremove = table.remove
	local floor = math.floor

	-- Create a copy of the nodes[] table and use this instead of the original because we want to modify this table
	local nodes2 = {}
	for i = 1, numNodes do
		nodes2[i] = nodes[i]
		weight[i] = {} -- make weight[] a 2-dimensional table
	end
	local nodes = nodes2

	-- Step 1: Generate the weight table
	for i = 1, numNodes do
		local coord = nodes[i]
		local x, y = floor(coord / 10000) / 10000, (coord % 10000) / 10000
		local w = weight[i]
		w[i] = 0
		for j = i+1, numNodes do
			local coord = nodes[j]
			local x2, y2 = floor(coord / 10000) / 10000, (coord % 10000) / 10000
			w[j] = (((x2 - x)*zoneW)^2 + ((y2 - y)*zoneH)^2)^0.5 -- Calc distance between each node pair
			weight[j][i] = true -- dummy value just to fill the lower half of the table so that tremove() will work on it
		end
	end

	-- Step 2: Generate the initial metadata tables
	for i = 1, numNodes do
		metadata[i] = {}
		metadata[i][1] = nodes[i]
	end

	-- Step 5: ...and loop until there is no such pair of nodes
	while true do
		-- Step 3: Find the closest pair of nodes within the merge radius
		local smallestDist = inf
		local node1, node2
		for i = 1, numNodes-1 do
			local w = weight[i]
			for j = i+1, numNodes do
				local w2 = w[j]
				if w2 <= diameter and w2 < smallestDist then
					smallestDist = w2
					node1 = i
					node2 = j
				end
			end
		end
		-- Step 4: Merge node2 into node1...
		if node1 then
			local m1, m2 = metadata[node1], metadata[node2]
			local node1num, node2num = #m1, #m2
			local totalnum = node1num + node2num
			-- Calculate the new centroid of node1
			local n1, n2 = nodes[node1], nodes[node2]
			local node1x = ( floor(n1 / 10000) / 10000 * node1num + floor(n2 / 10000) / 10000 * node2num ) / totalnum
			local node1y = ( (n1 % 10000) / 10000 * node1num + (n2 % 10000) / 10000 * node2num ) / totalnum
			-- Calculate the new coord from the new (x,y)
			local coord = floor(node1x * 10000 + 0.5) * 10000 + floor(node1y * 10000 + 0.5)
			node1x, node1y = floor(coord / 10000) / 10000, (coord % 10000) / 10000 -- to round off the coordinate
			-- Check that the merged point is valid
			for i = 1, node1num do
				local coord = m1[i]
				local x, y = floor(coord / 10000) / 10000, (coord % 10000) / 10000
				local t = (((node1x - x)*zoneW)^2 + ((node1y - y)*zoneH)^2)^0.5
				if t > radius then
					-- Merging this node will cause the merged point to be too far away
					-- from an original point, so taboo it by making the weight infinity
					-- And store a backup in the lower half of the table
					weight[node2][node1] = weight[node1][node2]
					weight[node1][node2] = inf
					--taboo = taboo + 1
					break
				end
			end
			if weight[node1][node2] ~= inf then
				for i = 1, node2num do
					local coord = m2[i]
					local x, y = floor(coord / 10000) / 10000, (coord % 10000) / 10000
					local t = (((node1x - x)*zoneW)^2 + ((node1y - y)*zoneH)^2)^0.5
					if t > radius then
						weight[node2][node1] = weight[node1][node2]
						weight[node1][node2] = inf
						--taboo = taboo + 1
						break
					end
				end
			end
			if weight[node1][node2] ~= inf then
				-- Merge the metadata of node2 into node1
				for i = 1, node2num do
					tinsert(m1, m2[i])
				end
				-- Set the new coord of node1
				nodes[node1] = coord
				-- Delete node2 from metadata[]
				tremove(metadata, node2)
				-- Delete node2 from nodes[]
				tremove(nodes, node2)
				-- Remove node2 from the weight table
				for i = 1, numNodes do
					tremove(weight[i], node2) -- remove column
				end
				tremove(weight, node2) -- remove row
				-- Update number of nodes
				numNodes = numNodes - 1
				-- Update the weight table for all nodes relating to node1, this can untaboo nodes
				for i = 1, node1-1 do
					local coord = nodes[i]
					local x, y = floor(coord / 10000) / 10000, (coord % 10000) / 10000
					weight[i][node1] = (((node1x - x)*zoneW)^2 + ((node1y - y)*zoneH)^2)^0.5
				end
				for i = node1+1, numNodes do
					local coord = nodes[i]
					local x, y = floor(coord / 10000) / 10000, (coord % 10000) / 10000
					weight[node1][i] = (((node1x - x)*zoneW)^2 + ((node1y - y)*zoneH)^2)^0.5
				end
			end
		else
			break -- loop termination
		end
	end

	-- Get the new pathLength
	local pathLength = weight[1][numNodes]
	pathLength = pathLength == inf and weight[numNodes][1] or pathLength
	for i = 1, numNodes-1 do
		local w = weight[i][i+1]
		pathLength = pathLength + (w == inf and weight[i+1][i] or w) -- use the backup in the lower half of the triangle if it was tabooed
	end

	--ChatFrame1:AddMessage(taboo.." tabooed")

-- vim: ts=4 noexpandtab
