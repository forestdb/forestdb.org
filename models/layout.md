---
layout: model
title: Layout of Tables and Plates
model-status: code
model-category: Scientific and Physical Models
model-tags: layout, constraints
model-language: church
---

This model places a random number of plates on a table by sampling each plate's position and size, then uses Metropolis-Hastings with soft equality constraints to favor arrangements where plates cover about seventy percent of the table's area, stay within its bounds, and do not overlap. The Church version above implements this search; a Terra version below extends it to multiple tables.

Church version:

    (define my-pi 3.141592653589793)
    
    ;; soft equality score: Gaussian log-density of (y - x), softness 0.1
    ;; (as in the Quicksand/Terra softeq this Church port is based on)
    (define (softeq x y)
      (let ((d (- y x))
            (s 0.1))
        (- 0 (+ (* 0.5 (log (* 2 my-pi (* s s))))
                (/ (* d d) (* 2 (* s s)))))))
    
    (define (make-plate pos size table)
      (list pos size table))
    (define plate-position first)
    (define plate-size second)
    (define plate-table third)
    
    (define (make-table size num-plates)
      (list size num-plates))
    (define table-size first)
    (define table-num-plates second)
    
    (define (plate-area plate)
      (* my-pi (* (plate-size plate) (plate-size plate))))
    
    (define (total-area plates)
      (apply + (map plate-area plates)))
    
    (define (table-area table)
      (* my-pi (* (table-size table) (table-size table))))
    
    (define (vec-dist xy1 xy2)
      (let* ([x (list-ref xy1 0)]
             [y (list-ref xy1 1)]
             [z (list-ref xy2 0)]
             [w (list-ref xy2 1)])
        (expt (+ (expt (- x z) 2) (expt (- y w) 2)) 0.5)))
    
    (define (dist-from-origin xy)
      (vec-dist xy (list 0 0)))
    
    (define (circular-area r)
      (* my-pi (expt r 2)))
    
    (define (area-outside-table plate)
      (let* ([d (dist-from-origin (plate-position plate))]
             [table-limit (table-size (plate-table plate))]
             [difference (max 0 (- d table-limit))])
        (circular-area difference)))
    
    (define (overlap-area p1 p2)
      (let* ([dist-between (vec-dist (plate-position p1) (plate-position p2))]
             [overlap-amt (max 0 (- (+ (plate-size p1) (plate-size p2)) dist-between))])
        (circular-area overlap-amt)))
    
    ;; webchurch has no letrec; the recursive helpers live at the top level
    (define (sks-fold-drop-one l seen accum)
      (if (null? l) accum
          (sks-fold-drop-one (cdr l) (cons (car l) seen)
                             (cons (append (cdr l) seen) accum))))
    
    (define (sks-fold-singletons l prev-els accum)
      (if (null? l) accum
          (sks-fold-singletons (cdr l) prev-els
                               (cons (cons (car l) prev-els) accum))))
    
    (define (sks-loop l ln n prev-els accum)
      (cond
        ((<= n 0) (cons prev-els accum))
        ((< ln n) accum)
        ((= ln n) (cons (append l prev-els) accum))
        ((= ln (+ 1 n)) (sks-fold-drop-one l prev-els accum))
        ((= n 1) (sks-fold-singletons l prev-els accum))
        (else
          (sks-loop (cdr l) (- ln 1) n prev-els
                    (sks-loop (cdr l) (- ln 1) (- n 1) (cons (car l) prev-els) accum)))))
    
    (define (select-k-subsets n l)
      (sks-loop l (length l) n '() '()))
    
    (define (pairs xs) (select-k-subsets 2 xs))
    
    (define (randint low high)
      (+ low (sample-integer (- high low))))
    
    (define (sample-table)
      (let* ([num-plates (randint 1 10)]
             [table-size (uniform 15 25)])
        (make-table table-size num-plates)))
    
    (define (sample-plate table)
      (let* ([posx (uniform -10 10)]
             [posy (uniform -10 10)]
             [plate-size (uniform 0.1 2.0)])
        (make-plate (list posx posy) plate-size table)))
    
    (define (tables-and-plates)
      (let* ([table (sample-table)]
             [plates (map (lambda (i) (sample-plate table)) (iota (table-num-plates table)))]
             [occupy-area (softeq 0.7 (/ (total-area plates) (table-area table)))]
             [inside (map (lambda (p) (softeq 0.0 (area-outside-table p))) plates)]
             [non-overlap (map (lambda (pq) (softeq 0.0 (overlap-area (first pq) (second pq)))) (pairs plates))])
        (list (apply + (list occupy-area (apply + (pair 0.0 inside)) (apply + (pair 0.0 non-overlap)))) table plates)))
    
    (for-each display
              (mh-query 100 100
                        (define asn (tables-and-plates))
                        (list 'score (first asn) 'num-plates (table-num-plates (second asn)))
                        true))
    


Terra version:

~~~~ norun
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
~~~~

References:

- Cite:yeh2012synthesizing
- [Daniel Ritchie](https://dritchie.github.io/) (2014)
