---
layout: page
title:  "Getting started"
section: "start"
position: 1
---

# Getting started with the Faacets tools

This page describes how to use faacets beyond the main website. In particular, it explains how to:
 - [Run the Faacets library console](#how-to-run-the-faacets-console)
 - [Run Faacets in matlab](#how-to-use-faacets-in-matlab)
 - [Run a local copy of the website](#how-to-run-a-local-copy-of-the-website)
 
Check [this page](contributing.html) for ways to contribute to this project.


## How to run the Faacets console
Faacets being based on scala, it comes with a standard console interface. Here we describe how to install this through SBT.

### Install git
Since the Faacets project is hosted on a git server, it requires a [git client](https://git-scm.com/downloads). On ubuntu, you can install git with the following command:

    sudo apt-get install git

### Install SBT
Install the Scala Build Tool from [http://www.scala-sbt.org/download.html](http://www.scala-sbt.org/download.html)

### Download the Faacets code
The latest code of Faacets can be downloaded directly from github.

    git clone https://github.com/denisrosset/faacets-beta
    cd faacets-beta
    git submodule update --init

### Launch the console
In order to launch the Faacets console, open a terminal in the faacets-beta folder and type

    sbt

The first time the `sbt` command is called, several softare modules will be downloaded. This may take several minutes. When this is finished, type

    console

This launches the Faacets console.

### Using the Faacets console
See [here](console/example.html) for an example of Faacets console usage.


## How to use Faacets in matlab

[TODO]

## How to run a local copy of the website

[TODO]








