# Animator of Lince 2.0

Rebooting the Lince program using CAOS, using simpler data structures and functions. Initially this will only have the approximated (non-symbolic) execution.

A snapshot of Lince 2.0 can be executed at https://lmf.di.uminho.pt/lince-2.0/

The previous version of Lince (not maintained) can be found at http://http://arcatools.org/lince


## Caos

This project uses and the Caos's framework as a submodule. More information on it can be found online:

 - Caos' GitHub page: https://github.com/arcalab/CAOS
 - Caos' tutorial: https://arxiv.org/abs/2304.14901
 - Caos' demo video: https://youtu.be/Xcfn3zqpubw 


## Videos

- [Hands-on tutorial on Lince 2.0](https://youtu.be/JFVPHVxvMjo?si=WmGAc7NDQzX5uofz), presented at the Shif2SDV European project consortium, April 2026 (11min)
- [Talk at GAG seminars over Lince](https://youtu.be/DHOXR2AmTp8?si=FAjMzhLIjtAjSwT5), University of Aveiro, Portugal, June 2021 (46min)

## Publications

 - [An Adequate While-Language for Stochastic Hybrid Computation](https://jose.proenca.org/publication/ppdp-stochastic-lince-2025/), _Renato Neves, José Proença, Juliana Souza_, PPDP 2025, September 2025
 - [Analyzing Many Simulations of Hybrid Programs in Lince](https://jose.proenca.org/publication/fmas-lince-2025/), _Reydel Arrieta and, José Proença, Patrick Meumeu Yomsi_, FMAS@iFM 2025, November 2025
 - [Formal Simulation and Visualisation of Hybrid Programs](https://jose.proenca.org/publication/fmas-lince-2024/), _Pedro Mendes, Ricardo Correia, Renato Neves, José Proença_, FMAS@iFM 2024, November 2024
 - [Implementing Hybrid Semantics: From Functional to Imperative](https://jose.proenca.org/publication/goncharov-implementing-ictac-20/), _Sergey Goncharov, Renato Neves, José Proença_, ICTAC 2020, October 2020
 - [An Adequate While-Language for Hybrid Computation](https://repositorio.inesctec.pt/items/6377f0aa-bea0-4d5a-b996-26ffab9e4703), _Sergey Goncharov, Renato Neves_, PPDP 2019, October 2019


## Requirements

- JVM (>=1.8)
- sbt

Before compiling import the CAOS submodule, e.g., using the command:

> git submodule update --init

You also need to add the following line to the file `lib/caos/tool/index.html`, at line 57 (since CAOS does not load Plotly by default):

> <script type="text/javascript" src="js/static/plotly.min.js"></script>

## Compilation

You need to compile this project using the ScalaJS plug-in, following the steps below.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `sbt fastLinkJS`
2. open the file `lib/caos/tool/index.html`
