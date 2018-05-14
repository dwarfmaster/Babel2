# Babel2

Babel2 is a general framework for implementing and running your agent-based experiments, both in a simulated environment or embodied in grounded robots. It connects our core technologies such as [Fluid Construction Grammar](www.fcg-net.org) and Incremental Recruitment Language (IRL) with mechanisms for multi-agent interactions, robotic embodiment, cognitive processing and learning. An extensive monitoring system opens up every detail of Babel2’s intermediate representations and underlying dynamics. A modular design ensures that the system can be used in a wide variety of scenarios. It is therefore possible to use each component individually, according to your needs.

Babel2 is written in Common Lisp and runs in most major Lisp implementations ([CCL](https://ccl.clozure.com), [SBCL](http://www.sbcl.org) and [LispWorks (excluding Personal Edition)](http://www.lispworks.com)) on all major platforms (Linux, Mac OS X, Windows). Its source code is frequently released to the public under the Apache 2.0 License. We recommend using the [Emacs editor with SLIME](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki/Emacs-with-Lisp) or [Sublime Text with additional packages](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki/Sublime-Text-with-Lisp) for Babel development.

The best place for Babel2 and FCG discussion is [the mailing list](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki/Mailing-List). 

**Please note** that this repository contains only the public release of the Babel2 package. Our team works on another, private repository. We aim to release new functionality on a regular basis.

## Installation

### Mac OS X or Linux

Installation scripts are provided for both [Linux](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki/Installation-Script) and [Mac OS](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki/Installation-Script). These scripts will install all necessary tools for you to get started with Babel2 development.

If you don't want to install a Lisp environment by using the provided scripts, you can follow these instructions for [Linux](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki/Installing-Babel2-on-Linux) or [Mac OS](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki/Installing-Babel2-on-Mac-OS-X).

### Windows

Currently, we do not have an installation script for Windows. This is work in progress. For now, you will have to set up a Lisp environment yourself. You can install everything manually in [Windows](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki/Windows-Installation).

## Getting Started

The [FCG cookbook](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/wiki) is a dynamic document hosting practical recipes for learning about common problems and solutions in the Babel2 framework. This ranges from installation and Lisp editing tools, over writing constructions to more advanced topics such as setting up your own FCG server, creating a web demonstration, etc.

A good starting point for learning Fluid Construction Grammar is the article "Basics of Fluid Construction Grammar" by Luc Steels, published in _Constructions and Frames 9(2)_. You can download a draft of the article [here](https://www.fcg-net.org/wp-content/uploads/papers/basics-of-fcg.pdf). The web demonstration that supports the article is available [here](https://www.fcg-net.org/demos/basics-of-fcg/).

We regularly organise tutorials on Babel2. Please check [our events page](http://fcg-net.org/events).

## Contributing to Babel2

Feel free to contribute to the Babel2 package! There are 2 ways to do so:

 1. Reporting problems by creating an issue
 2. Extending Babel2 using a pull request

You can find more information in the [CONTRIBUTING](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/blob/master/CONTRIBUTING.md) file.
