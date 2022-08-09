<!-- Completed by Colack :) -->

<p align="center">
  <img src="logo.png" alt="Logo" width="353" height="170.5"/>
</p>

*This document is derived from CY Cergy Paris University's logo: www.cyu.fr. Reproduction rights reserved.*

---

CYTech's pseudocode based programming language.

## Introduction

CYLang is a project aiming to standardize and make executable pseudocode in order to facilitate the introduction to
computer science for students in the first year of the cycle.

The project focuses on three fundamental points:

- Allow students to test their algorithm written in pseudocode
- Allow teachers to test their examples and make them interactive
- Standardize the syntax of the pseudocode between the different TDs/Teachers

### Usage

Note: This section is subject to change as the first release is still being developed.

[**Releases/Downloads**](https://github.com/Iltotore/cylang/releases)

### Native executable (platform dependent)

You can download the native executable (without extension or .exe for Windows) and launch it using the following command:

| Operating System | Launch a program                | Show current version     |
|------------------|---------------------------------|--------------------------|
| Windows          | `.\cylang.exe <nom du fichier>` | `.\cylang.exe --version` |
| Linux            | `./cylang <nom du fichier>`     | `./cylang --version`     |

The native executable is completely self-contained and does not require any prerequisites.

### Java executable (universal)

You can download the Java executable (JAR) and launch it using the command `java -jar cylang.jar <file name>`.

Use `java -jar cylang.jar --version` to check the used version of CYLang.

The Java executable requires having Java 8 or higher installed.

## Internal functions

CYLang is an interpreter written in Scala. The general operation of an interpreter is to transform the source code (step
called "parsing") into a tree called AST which can be traversed for various purposes:
- Assessment: "execute" the AST.
- Typechecking: Check the consistency of the types of the different operations (example: do not subtract a text from a
  number)
- Scope resolution: Check that each variable/function/etc. used exists.
- etc.

The project is divided into three main categories of modules:
- Main module: The module containing the main code of the interpreter (from parsing to evaluation), used as a library by
  the other modules
- Test modules: Generally called `test`, contain the tests associated with their parent module (example: main/test
  contains the `tests` of the main module (main))
- Other modules: Mainly front-ends (interfaces) like command line interface

## Contributing

CYLang is open to any contribution regardless of the status or level of the contributor (internal or external to the
school).

### Issues

You can report a bug or suggest an addition by opening an [Issue](https://github.com/Iltotore/cylang/issues). Please
just follow the outline given when writing its content.

### Code Changes

#### Preparing your work environment

CYLang uses a tool called [Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html) which makes it easy to
prepare your workspace.

If you are using an IDE or a text editor like Intellij IDEA (with the Scala plugin) or Visual Studio Code (with the
Metals plugin), you can connect Mill to it using the command `mill mill.bsp.BSP/install` and importing the project via
BSP (detected automatically on most editors)     
You can then compile your code using the command `mill <module name>.compile` and test it
using `mill <module name>.test`

#### Notes:

- The `test` task automatically executes the `compile` task if it has not been launched before. So you don't need to
  compile then `test`. You can directly use `module.test.`
- On Windows, if you consistently get an error at the end of a task started via `mill <task>`, use `mill -i <task>`
  instead.
- Most IDE's "build" button works. You can use it instead of `mill module.compile`.
- You can run a task (like test) on all modules that have it use `__.` Example: `mill __.test`

#### Pull Requests

[Pull Requests](https://github.com/Iltotore/cylang/pulls) (PR) are a simple and stable way to propose
modifications/additions in the code of CYLang.

The process is the same as for other projects hosted on GitHub or similar platforms:

- Create a fork of the project (an alternative version automatically hosted on your account)

- Make the changes to propose on your fork (find out about the use of the Git tool)

- Open a Pull Request (GitHub will automatically suggest it to you on your fork page) describing the changes made and
  using the keyword `Closes #XX` if this PR closes/responds to one or more issues.

Note that only Pull Requests that pass all the tests will be accepted. If this is not the case and the error comes from
your modifications, you will simply be invited to correct the bug introduced.

## Questions

If you have any questions about CYLang, here are the different ways to ask:

- Open an Issue (See "Issues" section)
- Email `fromentinr@cy-tech.fr` (university email of the main author)
- Send a private message to `Il_totore#9133` (main author account) on Discord
