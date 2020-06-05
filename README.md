# routes-lua

Port of the clustering algorithm from the excellent WoW addon Routes (https://www.curseforge.com/wow/addons/routes) to a "regular" Lua program that can be executed in an interpreter outside WoW. Can be used to work around the execution time limits imposed by WoW's engine, which will stop an execution when it takes too long to complete. (This often happens when trying to cluster a route with too many nodes.)

Requires manual user intervention for input and output, see the source code comments for details.

For licensing see the header in TSP.lua, which was left intact from the original source code.
