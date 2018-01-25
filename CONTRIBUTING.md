# Contributing to Babel2

There are 2 ways in which you can contribute to the Babel2 package. 

## Reporting Bugs

When you find a bug in the Babel2 package, you can report this using the [GitHub Issue Tracker](https://github.com/EvolutionaryLinguisticsAssociation/Babel2/issues). Before doing so, make sure this bug has not already been reported or even fixed (also check the closed issues).

When submitting a bug, please provide us with as much information as possible. Here is a checklist to guide you through the process:

 - Give a clear and descriptive title for the issue to identify the problem.
 - Describe the exact steps to reproduce the issue. Start by describing your system setup. Which OS are you using? Which Lisp implementation are you using? Which editor are you using?
 - Next, describe the error you encountered and in what system/package/function the error occurred.
 - Ideally, provide a MWE (Minimum Working Example) that reproduces the error.
 - If possible, check if the error persists when using another OS, another Lisp implementation or another editor. 

## Adding Functionality

It is possible to extend Babel2 in various ways. You can [write your own grammar in FCG](), you can [set up an evolutionary experiment]() or [create an NLP experiment using our nlp-tools](). On a more technical level, you can help improve or extend the implementation of some of our systems.

Whatever you want to do, the first thing you should do is create a fork of this repository. A fork is a copy of a repository that you manage. Forks let you make changes to a project without affecting the original repository. Do make sure to set up the original repository as a remote to this fork, so you can fetch changes from upstream. A step-by-step guide on how to do this can be found [here](https://help.github.com/articles/configuring-a-remote-for-a-fork/) and [here](https://help.github.com/articles/syncing-a-fork/).

When you are done developing whatever you want to add to Babel2, send a pull request with your proposed changes. Detailed instructions can be found [here](https://help.github.com/articles/creating-a-pull-request-from-a-fork/). The Babel2 development team will be notified of your pull request. This allows us to start a discussion about your proposed changes and for you to make additional changes when and where necessary. Once accepted, the pull request can be merged into the Babel2 package.  