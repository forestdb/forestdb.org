---
layout: model
title: Layout of Tables and Plates (Terra)
model-status: code
model-category: Miscellaneous
model-tags: layout, constraints
model-language: terra
---

    terralib.require("prob")
    local mem = terralib.require("mem")
    local Vec = terralib.require("linalg").Vec
    local Vector = terralib.require("vector")
    local inheritance = terralib.require("inheritance")
    local rand = terralib.require("prob.random")
    local cmath = terralib.includec("math.h")
    
    local C = terralib.includecstring [[
    #include <stdio.h>
    ]]
    
    local Vec2 = Vec(double, 2)
    
    local struct Circle { pos: Vec2, rad: double }
    terra Circle:area() return [math.pi]*self.rad*self.rad end
    terra Circle:intersectArea(other: &Circle)
    	var r = self.rad
    	var R = other.rad
    	var d = self.pos:dist(other.pos)
    	-- No intersection
    	if d > r+R then
    		return 0.0
    	end
    	if R < r then
    		r = other.rad
    		R = self.rad
    	end
    	-- Complete containment
    	if d < R-r then
    		return [math.pi]*r*r
    	end
    	var d2 = d*d
    	var r2 = r*r
    	var R2 = R*R
    	var x1 = r2*cmath.acos((d2 + r2 - R2)/(2*d*r))
    	var x2 = R2*cmath.acos((d2 + R2 - r2)/(2*d*R))
    	var x3 = 0.5*cmath.sqrt((-d+r+R)*(d+r-R)*(d-r+R)*(d+r+R))
    	return x1 + x2 - x3
    end
    
    local struct Table { plates: Vector(Circle) }
    inheritance.staticExtend(Circle, Table)
    terra Table:__construct() mem.init(self.plates) end
    terra Table:__destruct() mem.destruct(self.plates) end
    terra Table:__copy(t: &Table) self.plates = mem.copy(t.plates) end
    mem.addConstructors(Table)
    
    local function layoutModel()
    
    	-- Soft equality factor function
    	local softEq = macro(function(x, target, softness)
    		return `[rand.gaussian_logprob(real)](x, target, softness)
    	end)
    
    	-- Generate random tables (with plates)
    	local plateNums = Vector.fromNums(0, 1, 2, 3, 4)
    	local makeTables = pfn()
    	makeTables:define(terra(numTables: int, tables: &Vector(Table)) : {}
    		if numTables > 0 then
    			-- Generate table with random posiiton and size
    			tables:resize(tables.size + 1)
    			var newTable = tables:backPointer()
    			newTable.pos = Vec2.stackAlloc(uniform(0.0, 50.0), uniform(0.0, 50.0))
    			newTable.rad = uniform(5.0, 15.0)
    
    			-- Make random number of plates with random 
    			--    position and size
    			var numPlates = int(uniformDraw(&plateNums))
    			newTable.plates:resize(numPlates)
    			for i=0,numPlates do
    				var posvariance = newTable.rad / 3.0
    				var perturb = Vec2.stackAlloc(gaussian(0.0, posvariance), gaussian(0.0, posvariance))
    				newTable.plates(i).pos = newTable.pos + perturb
    				newTable.plates(i).rad = uniform(1.0, 2.0)
    
    				-- Encourage plate to be fully on the table
    				var area = newTable.plates(i):area()
    				var isectArea = newTable.plates(i):intersectArea(newTable)
    				factor(softEq(isectArea, area, 2.0))
    			end
    
    			-- Encourage plates not to overlap
    			if numPlates > 0 then
    				for i=0,numPlates-1 do
    					for j=i+1, numPlates do
    						factor(softEq(newTable.plates(i):intersectArea(newTable.plates:getPointer(j)), 0.0, 0.1))
    					end
    				end
    			end
    
    			-- Recursively generate remaining tables
    			makeTables(numTables-1, tables)
    		end
    	end)
    
    	-- Program to perform inference on
    	return terra()
    		-- Generate random number of tables
    		var tables = [Vector(Table)].stackAlloc()
    		var numTables = poisson(6)
    		makeTables(numTables, &tables)
    
    		-- Encourage tables not to overlap
    		for i=0,tables.size-1 do
    			for j=i+1, tables.size do
    				factor(softEq(tables(i):intersectArea(tables:getPointer(j)), 0.0, 0.1))
    			end
    		end
    
    		return tables
    	end
    end

