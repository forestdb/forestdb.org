---
layout: model
title: Inverse statics (Terra)
model-status: code-fail
model-status-verbose: The code is in Terra, not Church.
model-category: Inverse Dynamics
model-tags: statics
model-language: terra
---

This is a simple model of a beam connected to the ground at its bottom end by a hinge and at its top end to a cable. 
Inference is over beam rotation angles that (approximately) satisfy static equilibrium.
    
    terralib.require("prob")
    local Vec = terralib.require("linalg").Vec
    local m = terralib.require("mem")
    local ad = terralib.require("ad")
    local rand = terralib.require("prob.random")
    
    local beamBottomX = `25.0
    local beamBottomY = `2.0
    local beamLength = `20.0
    local beamWidth = `3.0
    local cableRootX = `10.0
    local cableRootY = `2.0
    local function staticsModel()
    
    	local Vec2 = Vec(real, 2)
    
    	local terra torque(force: Vec2, pos: Vec2, centerOfRotation: Vec2)
    		var d = pos - centerOfRotation
    		return d(0)*force(1) - d(1)*force(0)
    	end
    
    	local terra polarToRect(r: real, theta: real)
    		return Vec2.stackAlloc(r*ad.math.cos(theta), r*ad.math.sin(theta))
    	end
    
    	-- Soft equality factor function
    	local softEq = macro(function(x, target, softness)
    		return `[rand.gaussian_logprob(real)](x, target, softness)
    	end)
    
    	local forceVariance = `100.0
    	return terra()
    		-- Set up geometry
    		var beamBottom = Vec2.stackAlloc(beamBottomX, beamBottomY)
    		var beamAngle = uniform(0.0, [math.pi], {structural=false, lowerBound=0.0, upperBound=[math.pi]})
    		var beamTop = beamBottom + polarToRect(beamLength, beamAngle)
    		var cableRoot = Vec2.stackAlloc(cableRootX, cableRootY)
    
    		-- Generate cable pulling (tensile) force
    		var cablePullDir = cableRoot - beamTop; cablePullDir:normalize()
    		var cableForce = gaussian(0.0, forceVariance, {structural=false, lowerBound=0.0}) * cablePullDir
    
    		-- Generate hinge force
    		var hingeForce = Vec2.stackAlloc(gaussian(0.0, forceVariance, {structural=false}),
    										 gaussian(0.0, forceVariance, {structural=false}))
    
    		-- Encourage net zero force on the beam
    		var totalForce = cableForce + hingeForce
    		factor(softEq(totalForce(0), 0.0, 1.0))
    		factor(softEq(totalForce(1), 0.0, 1.0))
    
    		-- Encourage net zero torque on the beam
    		var hingeTorque = torque(cableForce, beamTop, beamBottom)
    		factor(softEq(hingeTorque, 0.0, 1.0))
    
    		return beamAngle
    	end
    end

References:

- [Daniel Ritchie](http://stanford.edu/~dritchie/) (2014)
