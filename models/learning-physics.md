---
layout: model
title: Learning Physics from Dynamical Scenes
model-status: link
model-category: Inverse Dynamics
model-tags: dynamics
---

From Ref:ullman2014learning:

> Humans acquire their most basic physical concepts early in development, but continue to enrich and expand their intuitive physics throughout life as they are exposed to more and varied dynamical environments. We introduce a hierarchical Bayesian framework to explain how people can learn physical theories across multiple timescales and levels of abstraction.

The implementation of this computational model requires [Ikarus scheme](https://code.launchpad.net/~derick-eddington/ikarus/ikarus.dev-derick). Follow these instructions to install Ikarus and run the model:

1. Log into a compatible computer. We recommend [launching](https://us-west-1.console.aws.amazon.com/ec2/v2/home?region=us-west-1#LaunchInstanceWizard:) a 32-bit Ubuntu version such as `ubuntu-trusty-14.04-i386-server-20140724` on [EC2](https://aws.amazon.com/ec2/). To find this machine image, search for `ami-07b4b742` under Community AMIs. After launch, log in:

        ssh -i my-keypair.pem ubuntu@ec2-...compute.amazonaws.com

2. Install required packages:

        sudo apt-get update
        sudo apt-get install build-essential autoconf m4 automake perl
        sudo apt-get install libgsl0-dev libffi-dev libgmp3-dev
        sudo apt-get install git unzip bzr

3. Install Ikarus Scheme:

        wget http://stuhlmueller.org/data/ikarus.zip
        unzip ikarus.zip
        cd ikarus.dev-derick
        ./configure --enable-libffi
        make
        sudo make install
        cd ..

4. Install scheme-tools:

        git clone https://github.com/stuhlmueller/scheme-tools.git
        export IKARUS_LIBRARY_PATH=~/scheme-tools/:$IKARUS_LIBRARY_PATH

5. Download the ["Learning Physics" Scheme code](http://stuhlmueller.org/data/ullman-physics2015.zip):

        wget http://stuhlmueller.org/data/ullman-physics2015.zip
        unzip ullman-physics2015.zip
        export IKARUS_LIBRARY_PATH=~/ullman-physics2015/:$IKARUS_LIBRARY_PATH

6. Run the Scheme code (in this example: world 1, scenario 2):

        cd ullman-physics2015
        mkdir inference
        mkdir inference/world1/
        ikarus --script run-inference.ss 1 2

The inference results are written to `inference/world1/world1_1`.

References:

- Cite:ullman2014learning
