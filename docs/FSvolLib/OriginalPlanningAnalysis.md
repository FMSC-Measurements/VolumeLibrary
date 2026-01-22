## Problem

The Volume Library is a large codebase where code has been written over several decades by a handful of developers. Some code was originaly from seperate projects and not designed holesticly. Not all of the code follow the same codeing conventions. Many variables have short non discriptive variables. It may not be intuitive what all the code is doing. Use of pass by value with no enforcment of in/out variables makes it hard to tell when are where values are being changed. 

Although Fortran still remains in use by many people these days. Its uses is somewhat limited to niche uses, mainly mathmatical, and liniar algebra. 
The Volume Library hasn't kept up with modern Fortran and in its current state it may become hard to maintian in the longer term.

Converting over to a new language not only offers the advantage of picking a language that may be easier to find programmers for but it also forces is to take a deeper look at the code and see if there is a better way to work it. 


## language options
One key determinizers on leguage is the interoperability with existing projects that depend on the volume library.
FVS which is also written in Fortran uses the volume librry staticly, Cruise Processing, Orical procs used by ?, VolLib excel functions,
R bindings?,...
With that consideration leans us in the direction of highly portable native languages. C, C++, Rust, ...

 ### C
C has the unique binifit of being one the most commonly use languages asside Fortran. This is probably somewhat due to modern Fortran implementing robust interoperability with C and is a commonly listed feature of modern fortran.
 #### Pros
 - broad portability. guarinteed to work with any other language that is needed. can be compiled for any platfom needed (x86, x64, arm64, wasm, ...)
 - widely used language that will not likely case any resistance from picking it. 

 #### Cons
 - memory managment risks
 - language is a bit dated when it comes to quality of life enhancemnts of modern languages. try catch, struct/class methods, robust collection types

 ### C++
 C++ offers many of the same binifits and risk as C. However it can be a little harder to work with from other languages. In return it does offer the ability to use Object Oriented programming which can help with providing structure to the code base and reducing code duplication. Also OOP provides the ability to more easily work with data, via Vectors, Maps, and libraries that assist with working with external files.

 ## Recomended Language Selection
I am recomending using C++, with the option of using C as an interface/wrapper for easier calling code from legacy Fortran code. 


## Phased Plan

 ### Phase 0
 - identify code files that are not being used
 - identify and fill in missing documentation 

 ### Phase I
 This phase will focus on setting up the core API of the new Volume Library, the main entry point, and core utility classes 
 #### Sprint 1
  - create new `VolumeLibrary` class as the main API entry point 
    - define volume library input types (TreeHeights, TreeDiamaters, VolumeCalulationOptions, ...)
    - define volume library output types (VolumeCalulationOutput, LogInfo, BiomassComponents, ...)
  - create `VolumeCalculator` interface, this will serve as a base class for all volume calculation logic. 
  - create `BiomassCalculator` interface, this will serve as a base class for all biomass calculation logic. 
  - create a unit test project for testing new code. 

 #### Sprint 2 
  - create `EquationSelector` which defines logic for selecting which `VolumeCalculator` or `BiomassCalculator` to use based on the volume equation number, and/or other inputs to the volume library. This can also be use for looking up available volue equations
  - create `ConfigurationManager` class for loading coefficent data in from JSON files. The configuration manager will also handle storing configuration data in mememory so that JSON files only need to be read once in the case of calculating many tree volumes at once. 

  ### Phase II
  This phase will tackle the biomass calculation side of things first. Since biomass calculation is less extensive this will help warm up to working in a new language while not bighting off a big chunk. 

 - design out `BiomassCalculator` base class and base logic. 
 - create `JenkinsBiomassCalculator` sub class
 - create `NBELBiomassCalculator` sub class
 - create `biomass_equations.json` file
 - create `species_mappings.json` file

 ### Phase III
 This phase will tackle the region specific volume calculation logic. 

 - create `RegionalVolmeCalculator` base class
 - create region and method specific volume calculators e.g. `R1VolumeCalculator`, `R2TaperCalculator`, etc.
 - create `EquationCoefficient` data type
 - create files for storing coefficient data e.g. `r1coefficients.json`, `r2_taper.json`


## Time estimates
So to get a scale of the conversion effort, metrics of the origianal code base are:
 - 149 individual code files
 - 415 subroutines and functions arcross the code files
 - most files are around 200-500 lines of code with some larger files up to 2000loc
 - rough ballpark estimated lines of code 60k - 90k 

To get a rough estimate of how long it might take to convert the code I'll use the number of subrutines and functions, since thats a more consistant metric, and make an estimate number of functions to convert per day. 
I think it might be reasonable to esitmate converting about 1-2 methods a day. Some methods can be on the longer side and my take a bit of thinking to understand what they are doing and may take a day. Other methods just do simple math or logic and could be easy to convert. With that we are looking at around a year. 

Although I did consider some options that could help with speeding up the process. Using the `f2c` project which offers the ability to automaticly convert Fortran 77 code to C or C++ could lift a lot of the load from the convertion process. Then I tried it, converting as much code I could automaticly. Some files couldn't convert since they used more modern fortran features which are not supported by f2c. 
What I got was even less readable than the original Fortran code. Any code automaticly converted to C would need to be reworked by hand and would take as much time to rework as a compleat rewrite. Additionaly one advantave to a manual rewrite is that it allows us to better clear out the "cobwebs" of the old code. With a automated conversion the resulting code would much more a mirror of the old code and risk bringing old flaws with it. One place where it could be benifitial would be migrating over old hardcoded data, since automation will help reduce risk of human error. 

Another option that I think would be worth considering is enlisting the help of AI. AI works best when you narrow down context and contraints. Having it convert one function at a time may give us reasonably good results. 
With the help of AI it may be feasable to hit closer to 3-4 methods converted a day which could bring the overall effor down to 3-5 months. 