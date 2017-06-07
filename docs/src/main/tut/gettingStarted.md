---
layout: page
title:  "Getting started"
section: "start"
position: 1
---

# Getting started with the Faacets tools

This page describes how to use faacets beyond the main website. In particular, it explains how to:
 - [Run the Faacets console](#how-to-run-the-faacets-console)
 - [Run a local copy of the website](#how-to-run-a-local-copy-of-the-website)
 - [Run Faacets in matlab](#how-to-use-faacets-in-matlab)
 
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

    git clone https://github.com/denisrosset/faacets-gamma
    cd faacets-gamma

### Launch the console
In order to launch the Faacets console, open a terminal in the faacets-gamma folder and type

    sbt

The first time the `sbt` command is called, several software modules will be downloaded. This may take several minutes. When this is finished, type

    console

This launches the Faacets console.

### Using the Faacets console
See [here](console/example.html) for an example of Faacets console usage.


## How to run a local copy of the website

There are two ways to run the Faacets website. 
The first one uses the SBT Play plugin, and is appropriate for local development and use, the second is based on Docker containers and is robust for production use.
For both methods, first install git / SBT  as detailed in [Getting started with the Faacets tools](#getting-started-with-the-faacets-tools) 

### Development mode

1) Clone the faacets *website* repository.

    git clone https://github.com/denisrosset/faacets-playnew
    cd faacets-playnew


2) The first time the `sbt` command is called, several software modules will be downloaded and cached in the local Ivy/Maven repository on your computer. This download takes several minutes. When this is finished, type

    run

When all downloads are finished, this launches the Faacets server on your local computer.

3) Open a web browser at the address [http://localhost:9000](http://localhost:9000).


### Production mode (Docker)

The website is packaged using Docker containers.

Install Docker: follow the instructions on the [Docker website](https://www.docker.com), including
"Manage Docker as a non-root user" in the post-installation steps.

1) Clone the Git repository with `git clone https://github.com/denisrosset/faacets-playnew`

2) Run `sbt docker:publishLocal` to create a local Docker container with the version

3) Run `docker run -e APPLICATION_SECRET=1234 --net="host" faacets-playnew:0.14.1.0-SNAPSHOT`
   where `1234` is the application secret (does not matter much, as all the website is public)
   and where `0.14.1.0-SNAPSHOT` is the version you want to run. By default, we run the networking
   on the host (i.e. the port 9000 will be opened on the host).

4) Connect to [http://localhost:9000](http://localhost:9000)

## How to use Faacets in Matlab

As of March 2017, there are no bindings for Matlab. 
The previous solution (Java adapters) has too high maintenance burden, we will consider lighter alternatives in the future. 
