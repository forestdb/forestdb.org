---
layout: model
title: LiquidFun Example
model-language: webppl
---

<script src="http://web.mit.edu/cjbates/www/liquidfun/liquidfun/Box2D/lfjs/liquidfun.js"></script>

<!-- testbed code !-->
<script src="http://web.mit.edu/cjbates/www/liquidfun/liquidfun/Box2D/lfjs/testbed/renderer.js"></script>
<script src="http://web.mit.edu/cjbates/www/liquidfun/liquidfun/Box2D/lfjs/testbed/three.js"></script>
<script src="http://web.mit.edu/cjbates/www/liquidfun/liquidfun/Box2D/lfjs/utils.js"></script>


<script src="liquidfun.js"></script>

<!-- testbed code !-->
<script src="http://web.mit.edu/cjbates/www/liquidfun/liquidfun/Box2D/lfjs/testbed/renderer.js"></script>
<script src="http://web.mit.edu/cjbates/www/liquidfun/liquidfun/Box2D/lfjs/testbed/three.js"></script>
<script src="http://web.mit.edu/cjbates/www/liquidfun/liquidfun/Box2D/lfjs/testbed/utils.js"></script>

<!-- *****************  modified testbed.js  *********************** !-->
<script>
	// shouldnt be a global :(
	var particleColors = [
	  new b2ParticleColor(0xff, 0x00, 0x00, 0xff), // red
	  new b2ParticleColor(0x00, 0xff, 0x00, 0xff), // green
	  new b2ParticleColor(0x00, 0x00, 0xff, 0xff), // blue
	  new b2ParticleColor(0xff, 0x8c, 0x00, 0xff), // orange
	  new b2ParticleColor(0x00, 0xce, 0xd1, 0xff), // turquoise
	  new b2ParticleColor(0xff, 0x00, 0xff, 0xff), // magenta
	  new b2ParticleColor(0xff, 0xd7, 0x00, 0xff), // gold
	  new b2ParticleColor(0x00, 0xff, 0xff, 0xff) // cyan
	];
	var container;
	var world = null;
	var threeRenderer;
	var renderer;
	var camera;
	var scene;
	var objects = [];
	var timeStep = 1.0 / 60.0;
	var velocityIterations = 8;
	var positionIterations = 3;
	var test = {};
	var projector = new THREE.Projector();
	var planeZ = new THREE.Plane(new THREE.Vector3(0, 0, 1), 0);
	var g_groundBody = null;

	var windowWidth = 500;//window.innerWidth;
	var windowHeight = 300;//window.innerHeight;

	function printErrorMsg(msg) {
	  var domElement = document.createElement('div');
	  domElement.style.textAlign = 'center';
	  domElement.innerHTML = msg;
	  document.body.appendChild(domElement);
	}

	function initTestbed(selector) {
	  camera = new THREE.PerspectiveCamera(70
	    , windowWidth / windowHeight
	    , 1, 1000);

	  try {
	    threeRenderer = new THREE.WebGLRenderer();
	  } catch( error ) {
	    printErrorMsg('<p>Sorry, your browser does not support WebGL.</p>'
	                + '<p>This testbed application uses WebGL to quickly draw'
	                + ' LiquidFun particles.</p>'
	                + '<p>LiquidFun can be used without WebGL, but unfortunately'
	                + ' this testbed cannot.</p>'
	                + '<p>Have a great day!</p>');
	    return;
	  }

	  threeRenderer.setClearColor(0xEEEEEE);
	  threeRenderer.setSize(windowWidth, windowHeight);

	  camera.position.x = 0;
	  camera.position.y = 0;
	  camera.position.z = 100;
	  scene = new THREE.Scene();
	  camera.lookAt(scene.position);

	  $(selector).append( this.threeRenderer.domElement);

	  this.mouseJoint = null;

	  // hack
	  renderer = new Renderer();
	  var gravity = new b2Vec2(0, -10);
	  world = new b2World(gravity);
	  Testbed();
	}
	function Testbed(is_rendered) {

	  ResetWorld();
	  world.SetGravity(new b2Vec2(0, -10));
	  var bd = new b2BodyDef;
	  g_groundBody = world.CreateBody(bd);
	  test = new window["cogfluid"];

	  if (is_rendered) {
	  	render();
	  }
	}

	var render = function() {
	  // bring objects into world
	  renderer.currentVertex = 0;
	  if (test.Step !== undefined) {
	    test.Step();
	  } else {
	    Step();
	  }
	  renderer.draw();

	  threeRenderer.render(scene, camera);
	  requestAnimationFrame(render);
	};

	var ResetWorld = function() {
	  if (world !== null) {
	    while (world.joints.length > 0) {
	      world.DestroyJoint(world.joints[0]);
	    }

	    while (world.bodies.length > 0) {
	      world.DestroyBody(world.bodies[0]);
	    }

	    while (world.particleSystems.length > 0) {
	      world.DestroyParticleSystem(world.particleSystems[0]);
	    }
	  }
	  camera.position.x = 0;
	  camera.position.y = 0;
	  camera.position.z = 100;
	};

	var Step = function() {
	  world.Step(timeStep, velocityIterations, positionIterations);
	};

	/**@constructor*/
	function QueryCallback(point) {
	  this.point = point;
	  this.fixture = null;
	}

	/**@return bool*/
	QueryCallback.prototype.ReportFixture = function(fixture) {
	  var body = fixture.body;
	  if (body.GetType() === b2_dynamicBody) {
	    var inside = fixture.TestPoint(this.point);
	    if (inside) {
	      this.fixture = fixture;
	      return true;
	    }
	  }
	  return false;
	};

	$(document).ready(function() {
		initTestbed("#omg");
	});
</script>
<!-- *************************************************************** !-->

<!-- *****************  modified cogfluid.js  ********************** !-->
<script>
	function cogfluid() {
	  function getRandomArbitrary(min, max) {
	    return Math.random() * (max - min) + min;
	  }

	  camera.position.y = 2;
	  camera.position.z = 3;
	  var bodyDef = new b2BodyDef();
	  var ground = world.CreateBody(bodyDef);

	  // Obstacle geometry
	  var xbound_min = -1.5;
	  var xbound_max = 1.5;
	  var ybound_min = 0.0;
	  var ybound_max = 4.0;
	  // var fluid_x = 0.0;
	  var fluid_y = 3.45;
	  var fluid_radius = 0.4
	  var fluid_x = getRandomArbitrary(xbound_min + fluid_radius * 1.01, 
	    xbound_max - fluid_radius * 1.01)
	  // var cup_offset = 0.5;
	  var wall_thickness = 0.1;
	  var cup_inner_radius = 0.5;
	  var cup_height = 0.75;
	  var cup_offset = getRandomArbitrary(xbound_min + cup_inner_radius + wall_thickness,
	    xbound_max - cup_inner_radius - wall_thickness)
	  var obs_radius = 0.25;
	  // var offset_x = 0.0;
	  var offset_x = getRandomArbitrary(xbound_min + obs_radius, 
	    xbound_max - obs_radius) 
	  // var offset_y = 1.5;
	  var offset_y = getRandomArbitrary(ybound_min + obs_radius + cup_height, 
	    ybound_max - obs_radius - fluid_radius * 1.05 - (ybound_max - fluid_y))

	  // Boundary 
	  var chainShape = new b2ChainShape();
	  chainShape.vertices.push(new b2Vec2(xbound_min, ybound_min));
	  chainShape.vertices.push(new b2Vec2(xbound_max, ybound_min));
	  chainShape.vertices.push(new b2Vec2(xbound_max, ybound_max));
	  chainShape.vertices.push(new b2Vec2(xbound_min, ybound_max));

	  chainShape.CreateLoop();
	  ground.CreateFixtureFromShape(chainShape, 0);

	  // Fluid blob
	  var psd = new b2ParticleSystemDef();
	  psd.radius = 0.025;
	  psd.dampingStrength = 0.2;
	  var particleSystem = world.CreateParticleSystem(psd);

	  var circle = new b2CircleShape();
	  circle.position.Set(fluid_x, fluid_y);
	  circle.radius = fluid_radius;
	  var pgd = new b2ParticleGroupDef();
	  pgd.shape = circle;
	  pgd.color.Set(0, 0, 255, 255);
	  particleSystem.CreateParticleGroup(pgd);

	  // Obstacle  
	  var shape1 = new b2PolygonShape();
	  var vertices = shape1.vertices;
	  vertices.push(new b2Vec2(offset_x - obs_radius, offset_y - obs_radius));
	  vertices.push(new b2Vec2(offset_x + obs_radius, offset_y - obs_radius));
	  vertices.push(new b2Vec2(offset_x + obs_radius, offset_y + obs_radius));
	  vertices.push(new b2Vec2(offset_x - obs_radius, offset_y + obs_radius));
	  ground.CreateFixtureFromShape(shape1, 0);

	  // Left cup wall  
	  var shape2 = new b2PolygonShape();
	  var vertices = shape2.vertices;
	  vertices.push(new b2Vec2(cup_offset - cup_inner_radius, 0));
	  vertices.push(new b2Vec2(cup_offset - cup_inner_radius - wall_thickness, 0));
	  vertices.push(new b2Vec2(cup_offset - cup_inner_radius - wall_thickness, cup_height));
	  vertices.push(new b2Vec2(cup_offset - cup_inner_radius, cup_height));
	  ground.CreateFixtureFromShape(shape2, 0);

	  // Right cup wall
	  var shape3 = new b2PolygonShape();
	  var vertices = shape3.vertices;
	  vertices.push(new b2Vec2(cup_offset + cup_inner_radius, 0));
	  vertices.push(new b2Vec2(cup_offset + cup_inner_radius + wall_thickness, 0));
	  vertices.push(new b2Vec2(cup_offset + cup_inner_radius + wall_thickness, cup_height));
	  vertices.push(new b2Vec2(cup_offset + cup_inner_radius, cup_height));
	  ground.CreateFixtureFromShape(shape3, 0);
	}
</script>
<!-- *************************************************************** !-->

* toc
{:toc}

# hello, world :)

~~~~
Testbed(true);
~~~~

<div id="omg" width="500px" height="300px">
</div>
