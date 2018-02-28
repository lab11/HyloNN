#!/usr/bin/env stack

import Distribution.Simple

{- this is the main build system file, that we will use not only for cabal but
   for other things as well. -} 

{-
 The default options for cabal build we ought to support. 
./Setup.lhs configure [flags]	
Prepare to build the package. Typically, this step checks that the target platform is capable of building the package, and discovers platform-specific features that are needed during the build.

./Setup.lhs build	
Make this package ready for installation. For a true compiler, this step involves compiling the Haskell source code. Even for an interpreter, however, it may involve running a pre-processor.

./Setup.lhs install [install-prefix]	
Copy the files into the install locations, and register the package with the compiler.

./Setup.lhs register 

./Setup.lhs unregister

Register (or un-register) this package with the compiler. (NB: registration is also done automatically by install.)

./Setup.lhs clean	
Clean out the files created during the configure, build, or register steps.

./Setup.lhs test	
Run the package's test suite.
 -}

main = defaultMain
