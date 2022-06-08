# CYLANG
CYTech's pseudo-code based programming language
## Introduction
CYLang is a project aiming to standardize and make executable pseudo-code in order to facilitate the introduction to computer science for students in the first year of the cycle.
<br>
<br>
The project focuses on three fundamental points:
* Allow students to test their algorithm written in pseudo-code
* Allow teachers to test their examples and make them interactive
* Standardize the syntax of the pseudo-code between the different TDs/Teachers
### Utilization
Note: This section is subject to change as the first release is still being developed.
### Java executable (universal)
You can download the Java executable (JAR) and launch it using the command `java -jar cylang-x.x.x.jar <file name>`
<br>
The Java executable requires having Java 8 or higher installed.
### Native executable (platform dependent)
You can download the native executable (without extension or .exe for Windows) and launch it via the command `cylang <file name>`
<br>
The native executable is completely self-contained and does not require any prerequisites.
<br>
<br>
[Releases/Downloads](https://github.com/Iltotore/cylang/releases)
## Internal functions
CYLang is an interpreter written in Scala. The general operation of an interpreter is to transform the source code (step called "parsing") into a tree called AST which can be traversed for various purposes:
<br>
<br>
* Assessment: "execute" the Program.
* Typechecking: Check the consistency of the types of the different operations (example: do not subtract a text from a number)
* Scope resolution: Check that each variable/function/etc used exists.
* etc.
<br>
The project is divided into three main categories of modules:
<br>
* Main module: The module containing the main code of the interpreter (from parsing to evaluation), used as a library by the other modules   
* Test modules: Generally called `test`, contain the tests associated with their parent module (example: main/test contains the `tests` of the main module (main))   
* Other modules: Mainly front-ends (interfaces) like command line interface   
