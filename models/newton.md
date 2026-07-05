---
layout: model
title: Newtonian Dynamics
model-status: code
model-category: Scientific and Physical Models
model-tags: dynamics, physics
model-language: webppl
---

This model infers what forces govern a simple physical scene from the scene's observed motion. Two particles move in the plane; their motion is computed by integrating Newton's equations of motion with a fourth-order Runge-Kutta (RK4) integrator. Forces are functions from particle positions, velocities, and properties (mass, size, elasticity) to force vectors, and the model defines a small library of them: damped springs, gravitational attraction, "squigglers" and "spiralers", containment boxes, and damped-spring collisions.

In the observed scene, one particle moves at high speed towards another particle at rest. Inference is over the dynamics that produced the observed trajectory: did collision forces act between the particles, and if so, with what elasticity? The initial velocities are also uncertain. Each hypothesis is evaluated by simulating the scene forward with RK4 and softly conditioning the simulated trajectory to match the observed one at a subsampled set of points, so every MH step integrates the equations of motion anew.

The program runs this inference for two scenes: one where the particles do not interact, so the moving particle passes through the other, and one where collision forces produce an almost inelastic collision. The first posterior favors no collision force; the second favors collision and recovers elasticity near its true value of 0.1. The short paths and modest MCMC settings keep both examples practical in the browser and in headless checks.

~~~~
// ----- 2-D vector helpers -----

var vAdd = function(a, b) { return [a[0] + b[0], a[1] + b[1]]; };

// Operations on per-particle lists of [x,y] vectors:
var addVL = function(A, B) { return map2(vAdd, A, B); };
var scaleVL = function(s, A) {
  return map(function(a) { return [s * a[0], s * a[1]]; }, A);
};
var zeroVL = function(n) { return repeat(n, function() { return [0, 0]; }); };

// Euclidean distance between two 2-D points
var dist = function(a, b) {
  return Math.sqrt(Math.pow(a[0] - b[0], 2) + Math.pow(a[1] - b[1], 2));
};

// Angle of the vector pointing from a to b
var angleBetween = function(a, b) {
  return Math.atan2(b[1] - a[1], b[0] - a[0]);
};

// Go from elastic property to spring damping constant c in F = -c*v
var elasticToDamping = function(elastic, k, mass) {
  var logE = Math.log(elastic);
  return 2 * Math.sqrt(k * mass) * (-logE) /
    Math.sqrt(Math.pow(Math.PI, 2) + Math.pow(logE, 2));
};

// ----- List of forces -----

// A force maps positions q, velocities v, and particle properties to a
// list of force vectors ((Fx1,Fy1), (Fx2,Fy2), ...).

// Force #0: nothing
var nullF = function(q, v, props) { return zeroVL(q.length); };

// Force #1: damped harmonic oscillator, one-dimensional
var dampedHarmonicF = function(q, v, props) {
  var centerpoint = -50;
  return map2(function(qi, vi) {
    return [-0.1 * vi[0] - (centerpoint + qi[0]), 0];
  }, q, v);
};

// Pairwise interactions: takes a force describing the interaction between
// any pair of particles and sums the contributions on each particle.
var multiInteractionF = function(pairF) {
  return function(q, v, props) {
    return mapN(function(i) {
      var fs = mapN(function(j) {
        return i === j ? [0, 0] : pairF(q[i], q[j], v[i], v[j], props[i], props[j]);
      }, q.length);
      return reduce(vAdd, [0, 0], fs);
    }, q.length);
  };
};

// Forces #2-3: gravitational attraction between particles
var twoParticleGravF = function(q1, q2, v1, v2, props1, props2) {
  var r = dist(q1, q2);
  var theta = angleBetween(q1, q2);
  var F = 100 * props1.mass * props2.mass / Math.pow(r, 2);
  return [F * Math.cos(theta), F * Math.sin(theta)];
};
var gravitationalF = multiInteractionF(twoParticleGravF);

// Forces #4-5: squigglers; the force direction oscillates with distance
var twoParticleSquigglerF = function(q1, q2, v1, v2, props1, props2) {
  var r = dist(q1, q2);
  var F = 10000 / Math.pow(r, 2);
  return (props1.squiggler && props2.squiggler) ?
    [F * Math.cos(r), -F * Math.sin(r)] : [0, 0];
};
var squigglerF = multiInteractionF(twoParticleSquigglerF);

// Forces #6-7: spiralers; the force direction is perpendicular
var twoParticleSpiralerF = function(q1, q2, v1, v2, props1, props2) {
  var r = dist(q1, q2);
  var theta = angleBetween(q1, q2);
  var F = 10000 / Math.pow(r, 2);
  return (props1.spiraler && !props2.spiraler) ?
    [-F * Math.sin(theta), F * Math.cos(theta)] : [0, 0];
};
var spiralerF = multiInteractionF(twoParticleSpiralerF);

// Force #8: global force that contains all particles in a box
var boxF = function(q, v, props) {
  var k = 100000;  // strength of box: powerful tiny springs
  var wall = function(pos, lim0, lim1) {
    return pos < lim0 ? -k * (pos - lim0) :
           pos > lim1 ? -k * (pos - lim1) : 0;
  };
  return map(function(qi) {
    return [wall(qi[0], 100, 400), wall(qi[1], 100, 400)];
  }, q);
};

// Forces #9-10: collisions; each particle gets a tiny strong damped spring
// attached to it, with damping derived from its elasticity
var twoParticleCollisionF = function(q1, q2, v1, v2, props1, props2) {
  var k = 200000;  // spring strength
  var r = dist(q1, q2);
  var theta = angleBetween(q1, q2);
  var c = elasticToDamping(props1.elastic, k, props1.mass);
  var vr = (v1[0] - v2[0]) * Math.cos(theta) +
           (v1[1] - v2[1]) * Math.sin(theta);
  var impinge = r - (props1.size + props2.size);
  if (impinge < 0) {
    var F = -c * vr + k * impinge;
    return [F * Math.cos(theta), F * Math.sin(theta)];
  } else {
    return [0, 0];
  }
};
var collisionF = multiInteractionF(twoParticleCollisionF);

// Sum the contributions of a list of forces
var applyForceList = function(Fl, q, v, props) {
  return reduce(function(F, acc) { return addVL(F(q, v, props), acc); },
                zeroVL(q.length), Fl);
};

// Turn forces into accelerations: a = F/m
var FtoA = function(Fs, masses) {
  return map2(function(f, m) { return [f[0] / m, f[1] / m]; }, Fs, masses);
};

// ----- Runge-Kutta (RK4) integrator -----

var rk4Step = function(q0, v0, Fl, props, masses, dt) {
  var aQ = v0;
  var aV = FtoA(applyForceList(Fl, q0, v0, props), masses);
  var bQ = addVL(v0, scaleVL(dt / 2, aV));
  var bV = FtoA(applyForceList(Fl,
                               addVL(q0, scaleVL(dt / 2, aQ)),
                               addVL(v0, scaleVL(dt / 2, aV)),
                               props), masses);
  var cQ = addVL(v0, scaleVL(dt / 2, bV));
  var cV = FtoA(applyForceList(Fl,
                               addVL(q0, scaleVL(dt / 2, bQ)),
                               addVL(v0, scaleVL(dt / 2, bV)),
                               props), masses);
  var dQ = addVL(v0, scaleVL(dt, cV));
  var dV = FtoA(applyForceList(Fl,
                               addVL(q0, scaleVL(dt, cQ)),
                               addVL(v0, scaleVL(dt, cV)),
                               props), masses);
  var q1 = addVL(q0, scaleVL(dt / 6, addVL(addVL(aQ, scaleVL(2, bQ)),
                                           addVL(scaleVL(2, cQ), dQ))));
  var v1 = addVL(v0, scaleVL(dt / 6, addVL(addVL(aV, scaleVL(2, bV)),
                                           addVL(scaleVL(2, cV), dV))));
  return { q: q1, v: v1 };
};

// Simulate Newtonian dynamics, returning the list of positions over time
var rkNewtonian = function(q0, v0, Fl, props, steps, dt) {
  var masses = map(function(p) { return p.mass; }, props);
  var go = function(q, v, n) {
    if (n === 0) { return []; }
    var next = rk4Step(q, v, Fl, props, masses, dt);
    return [next.q].concat(go(next.q, next.v, n - 1));
  };
  return go(q0, v0, steps);
};

// The full path is too long for efficient inference, so we pick
// points along the path to condition on.
var pathSplitter = function(path, points) {
  var n = Math.floor(path.length / points);
  return mapN(function(i) { return path[n * (i + 1) - 1]; },
              Math.floor(path.length / n));
};

// ----- Initialization of positions, velocities, and properties -----

var q0 = [[200, 200], [100, 200]];
var v0 = [[0, 0], [1000, 0]];
var sceneProperties = [{ mass: 2, size: 12, elastic: 0.1 },
                       { mass: 1, size: 12, elastic: 0.1 }];
var dt = 0.001;
var pathLength = 250;
var numInferencePoints = 50;

// ----- Observation model -----

var gaussianLnpdf = function(val, mu, variance) {
  return -0.5 * (1.8378770664093453 + Math.log(variance) +
                 (val - mu) * (val - mu) / variance);
};

// Log score of a soft (noisy) equality between two paths
var noisyPathScore = function(obs, inf, noiseVar) {
  return sum(map2(function(frameO, frameI) {
    return sum(map2(function(pO, pI) {
      return (gaussianLnpdf(pO[0] - pI[0], 0, noiseVar) -
              gaussianLnpdf(0, 0, noiseVar)) +
             (gaussianLnpdf(pO[1] - pI[1], 0, noiseVar) -
              gaussianLnpdf(0, 0, noiseVar));
    }, frameO, frameI));
  }, obs, inf));
};

var gaussianFromVariance = function(mu, variance) {
  return gaussian(mu, Math.sqrt(variance));
};

// Add Gaussian noise to a list of vectors
var addNoiseVL = function(A, noiseVar) {
  return map(function(a) {
    return [gaussianFromVariance(a[0], noiseVar),
            gaussianFromVariance(a[1], noiseVar)];
  }, A);
};

// ----- Inference -----

// Given an observed path, infer whether collision forces govern the
// scene, the particles' elasticity, and the initial velocities.
var inferDynamics = function(observedPath) {
  var reducedObserved = pathSplitter(observedPath, numInferencePoints);
  return Infer({ method: 'MCMC', samples: 100, lag: 1, burn: 200 }, function() {
    var collision = flip(0.5);
    var inferredForces = collision ? [collisionF] : [nullF];
    var mass1 = Math.exp(0.2);  // deterministic, as in the original code
    var mass2 = Math.exp(0.2);
    var elastic = uniform(0.01, 0.99);
    var inferredProps = [{ mass: mass1, size: 12, elastic: elastic },
                         { mass: mass2, size: 12, elastic: elastic }];
    var inferredPath = rkNewtonian(q0, addNoiseVL(v0, 30), inferredForces,
                                   inferredProps, pathLength, dt);
    var reducedInferred = pathSplitter(inferredPath, numInferencePoints);
    var score = noisyPathScore(reducedObserved, reducedInferred, 0.1);
    // guard against non-finite scores from diverging simulations
    factor((score === score && score > -Infinity) ? score : -1e10);
    return { collision: collision, elastic: elastic };
  });
};

var roundPath = function(path) {
  return map(function(frame) {
    return map(function(p) {
      return [Math.round(p[0] * 10) / 10, Math.round(p[1] * 10) / 10];
    }, frame);
  }, path);
};

var summarize = function(name, observedPath) {
  display(name);
  display('  positions, every 30th step: ' +
          JSON.stringify(roundPath(pathSplitter(observedPath, 10))));
  var posterior = inferDynamics(observedPath);
  display('  P(collision forces govern the scene) = ' +
          expectation(posterior, function(s) { return s.collision ? 1 : 0; }));
  display('  E[elasticity] = ' +
          expectation(posterior, function(s) { return s.elastic; }));
  return true;
};

// Scene 1 (the run in the original program): no interaction, so the
// moving particle passes straight through the resting one.
var passThroughScene = rkNewtonian(q0, v0, [nullF], sceneProperties,
                                   pathLength, dt);
summarize('Pass-through scene (true dynamics: no interaction):',
          passThroughScene);

// Scene 2: the same particles governed by collision forces.
var collisionScene = rkNewtonian(q0, v0, [collisionF], sceneProperties,
                                 pathLength, dt);
summarize('Collision scene (true dynamics: collisions, elasticity 0.1):',
          collisionScene);
~~~~

### Note on this port

This page was originally written for desktop Church (Bher/Ikarus) and could not run in the browser engine. The version above is a WebPPL port that keeps the physical model—the force library, the elasticity-derived damping, the RK4 integrator, and the soft trajectory-matching condition—while making inference light enough to run in the browser. Differences from the original:

- Particle properties are records rather than association lists, and angles are computed with `Math.atan2`, which is equivalent to the original's case analysis.
- The original parameterized Gaussians by variance (see its `gaussian-lnpdf`); the port keeps the variance parameterization through a helper.
- The original's `noisy=` soft-equality conditions are folded into a single equivalent `factor`. The browser model conditions on 50 subsampled path points rather than the full path.
- The original's `mh-query` returned `(list mass1 mass2)`, but both masses were the deterministic constant `(exp 0.2)`; the port returns the inferred force law and elasticity instead.
- The original only ran the pass-through scene (its `true-Fl` was the null force); the port also runs the collision scene, which the original's output filenames suggest was the intended experiment.
- MH settings: 200 burn-in iterations plus 100 samples per scene (the original requested 10 samples).

The original Church program:

~~~~ norun
(define pi 3.141592653589793)

;; select every nth element from a list
(define (element-n l n)
  (if (< (length l) n)
      '()
      (pair (list-ref l (- n 1)) 
            (element-n (drop l n) n))))

;; assuming path is a combination of q's and p's
(define (path-splitter path points)
  (let* ((n (floor (/ (length path) points))))
    (element-n (map first path) n)))

;; Euclidian distance between 2 2D particles, assuming coords come in as (x1, y1)
(define (compute-euc-dist particle1 particle2)
  (let ((x1 (first particle1))
        (x2 (first particle2))
        (y1 (second particle1))
        (y2 (second particle2)))
    (expt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)) 0.5))) 

;; get angle theta between 2 2D particles
(define (compute-angle particle1 particle2)
  (let* ((x1 (first particle1))
         (x2 (first particle2))
         (y1 (second particle1))
         (y2 (second particle2))
         (rx (- x2 x1))
         (ry (- y2 y1))
         (default-val (atan (/ ry rx))))
    (if (> rx 0)
        (atan (/ ry rx))
        (if (and (< rx 0) (>= ry 0))
            (+ (atan (/ ry rx)) pi)
            (if (and (< rx 0) (< ry 0))
                (- (atan (/ ry rx)) pi)
                (if (and (= rx 0) (> ry 0))
                    (/ pi 2)
                    (if (and (= rx 0) (< ry 0))
                        (/ pi -2)
                        0.0)))))))

;; go from x y to radial reprsentation

(define (xy->radial xy)
  (let ((r (compute-euc-dist xy '(0.0 0.0)))
        (theta (compute-angle xy '(0.0 0.0))))
    (list r theta)))

;; This probably exists already, but just to be sure
;; (define (gaussian-lnpdf val mu var)
;;   (* -0.5 (+ (+ 1.8378770664093453 (log var) )
;;              (/ (* (- val mu) (- val mu))
;;                 var) )) )

;; Go from elastic property to spring damping constant c in F = -c*v 
(define (elastic->damping elastic k mass)
  (* (* 2 (sqrt (* k mass))) (/ (- (log elastic)) (sqrt (+ (expt pi 2.0) (expt (log elastic) 2.0))))))


;; multiply number through a list
(define (mtimes n l)
  (map (lambda (x) (* n x)) l))

;; multiply number through a list of lists (GOD)
(define (mtimes2 n ll)
  (map (lambda (l) (mtimes n l)) ll))

;; add number through a list
(define (mplus n l)
  (map (lambda (x) (+ n x)) l))

;; add number through a list of lists
(define (mplus2 n ll)
  (map (lambda (l) (mplus n l)) ll))

;; add the lists of two lists together

(define addl 
  (lambda xs
    (let ([lla (first xs)]
          [llb (second xs)]
          [args (rest (rest xs))])
      (if (equal? args '())
          (map (lambda (la lb) (map + la lb)) lla llb)
          (fold (lambda (x y) (addl x y)) (addl lla llb) args)))))

(define (strip l)
  (if (equal? l '())
      '()
      (append (first l) (strip (rest l))) ))

;; remove the element indexed by ref from the list l, kind of
;; the opposite of list-ref. Assume zero-indexing. 
(define (remove-element l ref)
  (append (take l ref) (drop l (+ ref 1))))

;; turn momenta into velocity
(define (p->v pl ml)
  ;; assuming p and m are both vectors, however
  ;; p is a Nx2 vector-list and m is a N vector
  (map (lambda (pxy m) (list (/ (first pxy) m) (/ (second pxy) m)) ) pl ml))

;; turn forces into accelerations
(define (F->a Fl ml)
  ;; assuming F and m are both vectors, however
  ;; F is a Nx2 vector-list and m is a N vector
  (map (lambda (Fxy m) (list (/ (first Fxy) m) (/ (second Fxy) m)) ) Fl ml))

;; turn velocty into momenta
(define (v->p vl ml)
  ;; assuming v and m are both vectors, however
  ;; v is a Nx2 vector-list and m is an N vector
  (map (lambda (vxy m) (list (* (first vxy) m) (* (second vxy) m))) vl ml))

;; We don't want to have to remember property-lists by heart.
;; Since they will be in the form (("mass" 30) ("squiggler" #t)) and so on,
;; this function allows us to define general property-grabbing functions
(define (get-property property)
  (lambda (property-list) 
    (second (first (filter (lambda (p) (equal? property (first p))) property-list)))))

;; will grab the (float) value of mass from a property list of a particle
(define get-mass (get-property "mass"))

(define get-elastic (get-property "elastic"))

;; will grab the (truth) value of squiggler-hood from a property list
(define get-squiggler (get-property "squiggler"))

;; will grab the (truth) value of spiraler-hood from a property list
(define get-spiraler (get-property "spiraler"))

;; will grab the (float) value of object size from a property list of a particle
(define get-size (get-property "size"))

(define (get-x q) (map first q))

(define (get-y q) (map second q))

(define (basic-collision pos mom lim)
  (if (< pos (first lim))
      (abs mom)
      (if (> pos (second lim))
          (- (abs mom))
          mom)))

(define (collision-box xlim ylim)
  (lambda (p q) 
    (let ((x (first q))
          (y (second q))
          (px (first p))
          (py (second p)))
      (list (basic-collision x px xlim) (basic-collision y py ylim)))))

;; Usually we're going to want several forces acting at once on the
;; different particles. We need a way to apply these individually
;; and then sum up the results. This function will take in the list of
;; forces, apply them to q and v in turn, and then return the summation
;; of Fx and Fy for each particle.

(define (apply-force-list F-list q v properties)
  ; we still want this to handle regular forces, so if it's not a list, just
  ; apply the force
  (if (not (list? F-list))
      (F-list q v properties)
      (if (equal? F-list '())
          (make-list (length q) '(0.0 0.0))
          (addl ((first F-list) '() '() '() q v properties) 
                (apply-force-list (rest F-list) q v properties)))))

;; convenient function for handling multiple-interaction between pairs of particles.
;; takes in a force F which describes the interaction between any pair, and computes
;; the necessary N^2 interactions, spitting them out as ((Fx1, Fy1) (Fx2, Fy2), ...)
(define (multi-interaction-F F)
  (lambda (q v properties) 
    (let* ((masses (map get-mass properties)))
      (let ((n 0) (Fl '()))
        (define (loop n Fl)
          (if (< n (length q))
              ((lambda ()
                 (define main-q (list-ref q n))
                 (define main-v (list-ref v n))
                 (define main-properties (list-ref properties n))
                 (define rest-q (remove-element q n))
                 (define rest-v (remove-element v n))
                 (define rest-properties (remove-element properties n))
                 (define f-main-rest (map  (lambda (q2 v2 properties2)
                                             (F main-q q2 main-v v2 main-properties properties2))
                                           rest-q rest-v rest-properties))
                 (define result (list (sum (map first f-main-rest)) (sum (map second f-main-rest))))
                 
                 (loop (+ n 1) (pair result Fl))))
              (reverse Fl)))
        (loop n Fl)))))

;;integrator step, using Runge-Kutta (RK4)

;; No gradient methods necessary for Newton.
;; Notice this is written as p and q rather than x and v so
;;  it will be more easily integrated into a single framework with
;;  Hamiltonian dynamics.

;; Note that F is a function of q, v, properties and possibly t
;;  F is a procedure that takes in vector-lists q, v and properties
;;  and spits out a vector-list of forces in the x and
;;  y directions, i.e. ((Fx1, Fy1), (Fx2, Fy2), ...)  *sigh*.
;;  Possibly it would be better for F to take in particles.
;;  but that would require using "set!" and such. 

;; a = F/m
;; v = integrate a
;; x = integrate v

;; The noise model adds the appropriate noise at the end of
;;   this deterministic procedure.
;; NOTETOSELF: There must be a simpler way to do this using
;;             some abstraction of a state evaluation.
(define rk-newtonian
  (lambda (q0 v0 Fl properties steps dt noise)
    (if (= steps 0)
        '()  
        (let* ((masses (map get-mass properties))
               ;(v0 (p->v p0 masses))
               (a0 (F->a (apply-force-list Fl q0 v0 properties) masses))
               
               (a_q v0)
               (a_v a0)
               
               (b_q (addl v0 (mtimes2 (/ dt 2) a_v)))
               (b_v (F->a (apply-force-list Fl (addl q0 (mtimes2 (/ dt 2) a_q))
                                            (addl v0 (mtimes2 (/ dt 2) a_v))
                                            properties)
                          masses))
               
               (c_q (addl v0 (mtimes2 (/ dt 2) b_v)))
               (c_v (F->a (apply-force-list Fl (addl q0 (mtimes2 (/ dt 2) b_q))
                                            (addl v0 (mtimes2 (/ dt 2) b_v))
                                            properties) 
                          masses))
               
               (d_q (addl v0 (mtimes2 dt c_v)))
               (d_v (F->a (apply-force-list Fl (addl q0 (mtimes2 dt c_q))
                                            (addl v0 (mtimes2 dt c_v))
                                            properties)
                          masses))
               
               (q1 (addl q0 (mtimes2 (/ dt 6)
                                     (addl a_q
                                           (mtimes2 2 b_q)
                                           (mtimes2 2 c_q)
                                           d_q))))
               (v1 (addl v0 (mtimes2 (/ dt 6)
                                     (addl a_v
                                           (mtimes2 2 b_v)
                                           (mtimes2 2 c_v)
                                           d_v))))
               ;(v1 (add-noise v1 noise))
               
               ;(p1 (v->p v1 masses))
               
               )
          (pair (list q1 v1) 
                (rk-newtonian q1 v1 Fl properties (- steps 1) dt noise))))))

;; ======================== List of Forces ===================================

;; Force #0: nothing

(define (null-F q v properties)
  (make-list (length q) '(0.0 0.0)))

;; Force #1: damped harmonic oscillator, one-dimensional
(define (damped-harmonic-F q v)
  (let* ((x-vec (map first q))
         (vx-vec (map first v))
         (centerpoint -50)
         (Fx (map + (map - (mtimes 0.1 vx-vec)) (map - (mplus centerpoint x-vec))))
         (Fy (make-list (length x-vec) 0.0))
         )
    (zip Fx Fy)))

;; Force #2: gravitational force between two particles
(define (two-particle-grav-F q1 q2 v1 v2 properties1 properties2)
  (let* ((r (compute-euc-dist q1 q2))
         (theta (compute-angle q1 q2))
         (mass1 (get-mass properties1))
         (mass2 (get-mass properties2))
         (F (* 100 (/ (* mass1 mass2) (expt r 2.0))))
         (Fx (* F (cos theta)))
         (Fy (* F (sin theta))))
    (list Fx Fy)))

;; Force #3: gravitational force between multiple particles
(define gravitational-F (multi-interaction-F two-particle-grav-F))

;; Force #4: Squigglers. The force between them and non-squigglers is
;;           proportional to the inverse of the distance, and the direction
;;           oscillates as a function of the distance
(define (two-particle-squiggler-F q1 q2 v1 v2 properties1 properties2)
  (let* ((r (compute-euc-dist q1 q2))
         (theta (compute-angle q1 q2))
         (is-squiggler1 (get-squiggler properties1))
         (is-squiggler2 (get-squiggler properties2))
         (F (/ 10000 (expt r 2.0)))
         (Fy (* -1.0 (* F (sin r))))
         (Fx (* F (cos r))))
    (if (and is-squiggler1  is-squiggler2)
        (list Fx Fy)    
        '(0.0 0.0))))

;; Force #5: multi-particle squiggle interaction
(define squiggler-F (multi-interaction-F two-particle-squiggler-F))

;; Force #6: Spiralers. The force between them and non-spirals is proportional
;;           to the inverse of the distance, and the direction is perpendicular.
(define (two-particle-spiraler-F q1 q2 v1 v2 properties1 properties2)
  (let* ((r (compute-euc-dist q1 q2))
         (theta (compute-angle q1 q2))
         (is-spiraler1 (get-spiraler properties1))
         (is-spiraler2 (get-spiraler properties2))
         (F (/ 10000 (expt r 2.0)))
         (Fx (* -1.0 (* F (sin theta))))
         (Fy (* F (cos theta))))
    (if (and is-spiraler1 (not is-spiraler2))
        (list Fx Fy)    
        '(0.0 0.0))))

;; Force #7: multi-particle spiral interaction
(define spiraler-F (multi-interaction-F two-particle-spiraler-F))

;; Force #8: global force. Contains all particles in a box
(define (box-F q v properties)
  ;; create a 'collision box' global perimeter
  ;; It may be the case that collisions are better handled
  ;; as 'events' in which the velocities are switched, as
  ;; opposed to some sort of Force-law treatment.
  
  (let* ((k 100000.0) ;; strength of box, basically treat it like powerful tiny springs
         (xlim0 100.0)
         (xlim1 400.0)
         (ylim0 100.0)
         (ylim1 400.0)
         (xvec (get-x q))
         (yvec (get-y q))
         (Fx (map (lambda (pos) (if (< pos xlim0) (* (- k) (- pos xlim0)) (if (> pos xlim1) (* (- k) (- pos xlim1)) 0.0))) xvec))
         (Fy (map (lambda (pos) (if (< pos ylim0) (* (- k) (- pos ylim0)) (if (> pos ylim1) (* (- k) (- pos ylim1)) 0.0))) yvec)))
    
    (zip Fx Fy)))

;; Force #9: collision forces. Each particle gets a tiny strong spring attached
;;           to it. Requires a notion of object size.

(define (two-particle-collision-F q1 q2 v1 v2 properties1 properties2)
  (let* ((k 200000.0) ; spring strength
         (elastic 0.1)
         (r (compute-euc-dist q1 q2))
         (theta (compute-angle q1 q2))
         (mass1 (get-mass properties1))
         (e (get-elastic properties1))
         (c (elastic->damping e k mass1))
         (vr (+ (* (- (first v1) (first v2)) (cos theta)) (* (- (second v1) (second v2)) (sin theta))))
         (size1 (get-size properties1))
         (size2 (get-size properties2))
         (impinge (- r (+ size1 size2))))
    (if (< impinge 0)
        ;(let* ((F (* k impinge))
        (let* ((F (+ (* (- c) vr) (* k impinge)))
               (Fx (* F (cos theta)))
               (Fy (* F (sin theta))))
          (list Fx Fy))
        '(0.0 0.0))))

;; Force #10: multi-collision formulation.
(define collision-F (multi-interaction-F two-particle-collision-F))
;; ====================== END of list of Forces =============================


(define F0 null-F)
(define F1 squiggler-F)
(define F2 gravitational-F)
(define F3 spiraler-F)
(define F4 box-F)
(define F5 collision-F)


;; observation noise function

(define (add-noise l observation-noise)
  (if (not (list? (first l)))
      (map (lambda (x) (gaussian x observation-noise)) l)
      (map (lambda (x) (add-noise x observation-noise)) l)))       
;
;; ====================== Forces in inference form ==========================

;; (define (guessed-damped-harmonic-F q v)
;;   (let* ((x-vec (map first q))
;;          (vx-vec (map first v))
;;          (centerpoint (gaussian -47 2))
;;          (Fx (map + (map - (mtimes 0.1 vx-vec)) (map - (mplus centerpoint x-vec))))
;;          (Fy (make-list (length x-vec) 0.0))
;;          )
;;     (zip Fx Fy)))

;; ===================== END of Forces in inference form ===================

;; ======= INITIALIZATION of position, momemtum and other parameters =======

;; starting positions and momentum
(define q0 '((200.0 200.0)
             (100.0 200.0)))


(define v0 '((0.0 0.0)
             (1000.0 0.0)))

;; particle properties ((p1_1, p1_2, p1_3,...), (p2_1, p2_2, p2_3,...),...)
;; properties can be things like charge, binary features, etc.
;; NOTE: Mass the is first property of a body!

(define properties '((("mass" 2.0) ("size" 12.0) ("elastic" 0.1)) 
                     (("mass" 1.0) ("size" 12.0) ("elastic" 0.1)))) 

(define dt 0.001)
(define observation-noise 0.0)

(define path-length 300 )

; the actual path is too long for efficient inference
; so we pick points along the path to condition on.
(define num-inference-points 100) 

;; ========================= END of INITIALIZATION =========================
;; ========================= BEGIN ACTUAL RUN ==============================

(define (my-church-force q v properties)
  (make-list (length q) '(0.0 0.0)))

(define true-Fl (list my-church-force))

;; --- run the actual actual ------

(define observed-path (rk-newtonian q0 v0 true-Fl properties path-length dt 0.0))


(define output-filename "./physics/collision-ratio2_0-e0_1")
(define samples-filename "./physics/output-collision-ratio2_0-e0_1")

;; ======================= END ACTUAL RUN ==================================
;; ======================= BEGIN INFERENCE =================================


(define samples
  (mh-query 
   
   10 1
   
   (define inferred-Fl
     (if (flip)
         (list F5)
         (list my-church-force)))
   
   (define (noisy= x y noise)
     (log-flip (- (gaussian-lnpdf (- x y) 0.0 noise)
                  (gaussian-lnpdf 0.0 0.0 noise))))
   
   (define (noisy=* a b noise)
     (if (and (list? a) (list? b))
         (all (map (lambda (i j) (noisy=* i j noise)) a b))
         (noisy= a b noise)))
   
   (define mass1 (exp 0.2))
   (define mass2 (exp 0.2))
   (define elastic (uniform 0.01 0.99))
   (define inferred-properties (list (list (list "mass" mass1) (list "size" 12) (list "elastic" elastic))
                                     (list (list "mass" mass2) (list "size" 12) (list "elastic" elastic))))
   
   
   
   (define inferred-path (rk-newtonian q0  (add-noise v0 30)  inferred-Fl inferred-properties path-length dt observation-noise))
   
   ;(define inferred-path (rk-newtonian q0  (add-noise v0 30)  Fl inferred-properties path-length dt observation-noise))
   
   (define reduced-observation (path-splitter observed-path num-inference-points))
   (define reduced-inferred (path-splitter inferred-path num-inference-points))
   
   
   ;; query over
   (list mass1 mass2)
   ;; (begin
   ;;   (display (list mass1 mass2 elastic))
   ;;   (display "\n")
   ;;   ;(display (time-difference stime (current-time)))
   ;;   ;(display "\n")
   ;;   (list mass1 mass2))
   ;inferred-path
   ;; conditioned on
   
   (noisy=* (map first observed-path) (map first inferred-path) .1)
   (noisy=* reduced-observation reduced-inferred .1)
   ;(equal? reduced-observation reduced-inferred)
   ;(noisy=* 2.0 (gaussian 1.0 1.0) .1)
   ;true
   
   )
  )

samples
~~~~

References:

- Cite:ullman2014learning
