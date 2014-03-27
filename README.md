### Implementation of Algorithm W in Scala 

This repository is used to store the result of a small WBSO project. The
aim of the project was to learn about monad transformers in Scala. In the 
end, I wish to look into dependency injection using the reader monad. 
Understanding of monad transformers is necessary to achieve this. 

To run this application:

cd /path/to/application
sbt
compile
run

The application will infer the types of lambda expressions. The state and 
the either monad are composed and the monad transformer stack is called TI.
The inference algorithm runs in the TI monad.  

