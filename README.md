Multiparametric combinatorial samplers
--------------------------------------

The current repository contains several examples of combinatorial structures
amenable to *multiparametric frequency tuning* techniques of Maciej Bendkowski, Olivier Bodini 
and Sergey Dovgal, implemented in the combinatorial system compiler [Boltzmann Brain](https://github.com/maciej-bendkowski/boltzmann-brain).

Each example consists of several helper scripts facilitating the generation
and the corresponding visualisation process. The main ```runner.py``` scripts,
 working on top of ```Boltzmann Brain``` utilities,
 are meant as an interface for compilation and generation of some illustrative examples.
For instance:

```
python2 runner -m compile    # compiles the auxiliary generators
python2 runner -m generate   # run the generation process
python2 runner -m clean      # remove residue files
```

In the case of each example, please consult the appropriate ```runner.py``` file for details.
To obtain additional help for each particular `python` script `X.py`, type
```
python2 X.py -h
```

For ```Boltzmann Brain``` installation guidelines, please see
the [Boltzmann Brain webpage](https://github.com/maciej-bendkowski/boltzmann-brain).

#### Polyomino tilings
The ```tilings``` folder contains a set of scripts for the random generation of
polyomino tilings. The ```large-tiling-generator.py``` script generates a specification
file defining the target tiling width and the corresponding set of admissible
tiles (see, e.g. ```input.tile```). Once the ```input.tile``` specification is
set, the compilation and generation processes can be launched using the
corresponding runner script.

#### Weighted partitions
The ``partitions`` folder contains a set of scripts for the random generation
of weighted partitions, corresponding to a model of ideal gas in d-dimensional quantum harmonic oscillator trap (also
known as a Bose-Einstein condensate). 
Their generation follows the default scheme
of using the corresponding runner script.

#### Simple varieties of trees
The ``trees`` folder ships with a set of scripts for the random generation
of simple varieties of trees. The ```uniform-dist.py``` script helps to
generate an input specification for ```trees.py``` with almost uniform
distribution of node degrees (see, e.g.```input.deg```).
Once the ```input.deg```
is set, both the compilation of corresponding generators and the generation
process itself can be launched using the enclosed runner script.

#### Binary lambda terms
The ```binary-lambda-terms``` folder contains scripts and utilities for the random generation
of plain lambda terms, in the so-called de Bruijn notation, under Tromp's binary size notion.
The ```specification.in``` defines the ```Boltzmann Brain``` input specification.
Note that the runner script does not support visualisation capabilities.

#### Variable distribution in plain lambda terms
The ```variable-distribution``` folder contains scripts for the random generation
of plain lambda terms, in the so-called de Bruijn notation, with an explicit
control over the distribution of indices.
Their generation follows the default scheme
of using the corresponding runner script.

#### Redexes in plain lambda terms
The ```redexes``` folder contains auxiliary helper scripts for the random generation
of plain lambda terms, in the so-called de Bruijn notation, with an explicit control
over the number of redexes. The ```specification.in``` defines directly the 
```Boltzmann Brain``` input specification. Note that the runner script does not support 
visualisation capabilities.


#### Citing Boltzmann Brain
If you use `Boltzmann Brain` or `Paganini` for published work, 
we encourage you to cite the accompanying paper: 

Maciej Bendkowski, Olivier Bodini, Sergey Dovgal

[Polynomial tuning of multiparametric combinatorial
samplers](https://arxiv.org/abs/1708.01212).
