Problem

large codebase with 149 code files. Most files have around 200 - 500 lines of code, some larger have 2000, 800 lines of code. 
so I estimate rough ball park of 90-60k lines of code. code files have been written by lots of different contributers over the years. 
some attempt to modernize over the years but not in a consistant way, probably due to the number of contributers. 

on a reasonable persons estimate, optimisticly converting on average about one file a day it would take over 7 months of dedicated work to convert all the code. 

# language options
One key determinizers on leguage is the interoperability with existing projects that depend on the volume library.
FVS which is also written in Fortran uses the volume librry staticly, Cruise Processing, Orical procs used by ?, VolLib excel functions,
R bindings?,...
With that consideration leans us in the direction of highly portable native languages. C, C++, Rust, ...

## C
C has a unique binifit as one of the most commonly use languages asside Fortran. This is probably somewhat do to modern versions of Fortran implementing robust C interoperabilites and is one of the commonly listed features of moden fortran. 

### ways to cut it down with C
In addition to C having good interoperability with C, there are several tools that offer the ability to convert Fortran code automaticly to C.
It probably wound't result in a great conversion to just convert all the code automaticly and then call it a day. The resulting code would likely be hard to read, coding styles that would make sence in Fortran may not carry over well into C, the fact that C methods usualy pass by value and most vollib methods are subrutines that use pass by reference and don't return a value. 

### Cons
 - no garbage collection, memory management risk goes up if we do anything that needs managment of resources, i.e. working with data colections, Files, or Databases 
 - language is a bit dated when it comes to quality of life enhancemnts of modern languages. try catch, unit testing, struct/class methods

## C++
C++ is also another widely used languages that is also very portable. 
### Pros
 - ability to use OO, when working to simplify down the code base. 
 - has built-in complex collection types.  
### Cons
 - also no garbage collection.




 ## Phase I

### Sprint 1
  - Convert VolInit(and maybe some other public interfaces) to modern fotran ~ 1000 lines of code excluding comments : about a week
    - also try to identify other files that can be easily converted to modern fortran  
  - create a C language project 
  - design a C interface that acts as the entry point for volume caculation similar to how VolInit or NVB did 
  - identify a set of core functionality that can be converted independantly 
### Sprint 2 
  - migrate that core functionality over to C using automated C conversion
  - bit by bit pollish down the automaticly converted C code into the form we want for the new code base. 


  ## Phase II
### Sprint 3 - ~7

   - continue to identify additional sections that can be itterativly converted over to the new code base.
   - as more functionality be comes duplicated by the new code base, new code can be back ported into the old volume library by using C interoperability provided by converting volinit to modern fortran. 

 ## Phase III
  - as easy to mirgate code becomes less and we start getting into the lage coeffeciant lookups that are uses extensivly in the volume library we can start looking twards other more modern languages that offer better tools such as object types for storing and working on data, better ability to work with external data, exception handleing, better unit testing tools and maybe even memory managment.


