# Animator of Lince 2.0

Rebooting the Lince program using CAOS, using simpler data structures and functions. Initially this will only have the approximated (non-symbolic) execution.


# Caos

This project uses and the Caos's framework as a submodule. More information on it can be found online:

 - Caos' GitHub page: https://github.com/arcalab/CAOS
 - Caos' tutorial: https://arxiv.org/abs/2304.14901
 - Caos' demo video: https://youtu.be/Xcfn3zqpubw 


## Requirements

- JVM (>=1.8)
- sbt

Before compiling import the CAOS submodule, e.g., using the command:

> git submodule update --init

## Compilation

You need to compile this project using the ScalaJS plug-in, following the steps below.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `sbt fastLinkJS`
2. open the file `lib/tool/index.html`
