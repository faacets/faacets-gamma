---
layout: page
title:  "Contributing"
section: "contributing"
position: 3
---

# Contributing to the Faacets project
Faacets is a project that is open to the community. This page provides a short guide for various ways in which you can get involved in this project.

Before proceeding with any of the following, make sure you are able to run the Faacets console (see instructions on the [gettingStarted page](gettingStarted.html#how-to-run-the-faacets-console)).

You should also be familiar with the [git workflow](https://guides.github.com/activities/contributing-to-open-source/). In particular, please use local *fork*, make any changes into a new branch (`git checkout -b [name_of_your_new_branch]`) and provide changes through a *pull request*.

The following typical contribution paths are detailed below:
 - [Updating the documentation](#updating-the-documentation)
 - [Contributing to the database](#contributing-to-the-database)
 - [Improving the source code](#contributing-to-the-code)

Note also that it is possible to raise 'issues' on the [github page](https://github.com/denisrosset/faacets-gamma/issues) in order to signal a desired feature or a bug.


## Updating the documentation
The website documentation is stored in the folder `docs/src/main`. It consists in a collection of markdown files. These files are automatically processed by various tools, including [tut](https://github.com/tpolecat/tut), [sbt-microsites](https://47deg.github.io/sbt-microsites/), [kramdown](https://kramdown.gettalong.org/) and [jekyll](https://jekyllrb.com/) to produce the documentation website.

Before performing any change on the source files, please make sure that you are located in your own git branch. If you just downloaded the folder `faacets-gamma`, you may achieve this by calling `git checkout -b [name_of_your_new_branch]`.

### Testing a modified documentation
To see the effect of some changes in the documentation source files, use the following commands to reconstruct the documentation and serve it on a local computer.

#### Install Jekyll
If you did not do it already, install ruby's version of Jekyll (do not install it from ubuntu repository: run `sudo apt-get autoremove jekyll` if already installed earlier).

    sudo apt-get install gem ruby-dev
    sudo gem install jekyll

#### Compile and serve the documentation
To compile the documentation from the source markdown files with the following command

    sbt docs/makeMicrosite

You may then serve a local copy of this . For this, run the following command from the `docs/target/jekyll` folder:

    jekyll serve --baseurl /faacets-gamma

After launching this command, the updated documentation website should be available at [http://127.0.0.1:4000/faacets-gamma/]([http://127.0.0.1:4000/faacets-gamma/) in your browser. Don't forget the trailing slash.

### Submit a pull request
To submit changes in the documentation to the github repository, you should first commit and push your branch to the github (this will require a github account). This may be achieved by running the following commands:

    git add .
    git commit
    git push

The first command adds any modified file to the staging area. The second command commits the changes to your local repository. This command requires a text explanation of the changes performed. The last command sends these changes to github.

After these steps, you may find you branch on the [github page](https://github.com/denisrosset/faacets-gamma) and submit a pull request. Please provide clear details about the reasons for the changes, and link the request to any open issues if applicable.


## Contributing to the database
The database of Bell inequalities managed by Faacets contains a lot of information on known Bell inequalities, but is not complete. If you wish to *add more Bell inequalities* to the database, or add *important missing information* (e.g. ) on the , you may do so by following the steps below. Note that contributions of this kind may be checked for quality before being merged into the main database. Also, such contributions should be reasonably sized to be considered ().

 -
 -
 -

## Contributing to the code
The Faacets source code is hosted on the [github page](https://github.com/denisrosset/faacets-gamma). To work on it and propose improvements to it, you may follow the next steps.


