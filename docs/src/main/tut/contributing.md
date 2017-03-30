---
layout: page
title:  "Contributing"
section: "contributing"
position: 4
---

# Contributing to the Faacets project
Faacets is an open source project. It thus welcomes contributions from the community. This page provides a short guide for various ways in which you can get involved in this project.

Note also that it is possible to raise 'issues' on the [github page](https://github.com/denisrosset/faacets-gamma/issues) in order to signal a desired feature or a bug.

Before proceeding with any of the instructions below, make sure you are able to run the Faacets console (see how to do that on the [gettingStarted page](gettingStarted.html#how-to-run-the-faacets-console)).

### Creating a development branch
Please also make sure that you are familiar with the [git workflow](https://guides.github.com/activities/contributing-to-open-source/). In particular, changes should be performed on a *new branch* of a local *fork*, and pushed to the main repository through a *pull request*.

Here is how to fork the main faacets-gamma repository and create a personal development branch on it: on the [github page](https://github.com/denisrosset/faacets-gamma), click on the `Fork` button. This will make a copy of the repository to your personal github page. You may log into your github account or create one at this stage. Then, open a terminal on your computer typ the following commands:

``` bash
git clone https://github.com/[your_github_username]/faacets-gamma
cd faacets-gamma
git checkout -b [name_of_your_new_branch]
```

The first command downloads a local copy of your faacets-gamma repository, and the second one creates a new working branch. Once this branch is ready, you can proceed with performing changes to the source code.

### How to contribute
The following typical contribution paths are detailed below:
 - [Writing documentation](#updating-the-documentation)
 - [Contributing to the database](#contributing-to-the-database)
 - [Improving the source code](#contributing-to-the-code)

### Submit a pull request
After implementing changes on the source code, you may submit these changes to the main github repository. For this, first make sure that the files you modified were updated on your fork of the repository. This may be achieved by running the following commands within your local copy of the repository:

    git add .
    git commit
    git push

The first command adds any modified file to the staging area. The second command commits the changes to your local repository. This command requires a text explanation of the changes performed. The last command sends these changes to github. Note that the first two commands may be used more often than when performing a pull request: it is considered a good practice to perform regular commits when performing a large amount of changes to the code.

After these steps, you may go to the main [github page](https://github.com/denisrosset/faacets-gamma) and select `Pull requests`, `New pull request`. Then, click on `compare across forks` and select the destination and source of the pull request. Typically, this would be "base fork: denisrosset/faacets-gamma" with "base: master" and "head fork: [your_github_username]/faacets-gamma" with "compare: [name_of_your_new_branch]" to send your changes to the main branch. If the branches are "Able to merge", you can proceed further and `Create pull request`. At this stage, please provide clear details about the reasons for the changes, and link the request to any open issues if applicable.


## Updating the documentation
The website documentation is stored in the folder `docs/src/main`. It consists in a collection of markdown files (specifically, the GitHub variant). These files are automatically processed by various tools, including [tut](https://github.com/tpolecat/tut), [sbt-microsites](https://47deg.github.io/sbt-microsites/), [kramdown](https://kramdown.gettalong.org/) and [jekyll](https://jekyllrb.com/) to produce the documentation website.

Right now, we use GitHub Pages to publish our website, and the Markdown processor is [kramdown](kramdown.gettalong.org).

Mathematical equations are displayed using [MathJax](www.mathjax.org). It [seems](http://tobanwiebe.com/blog/2016/02/mathjax-kramdown) that `kramdown` only recognizes expressions in double dollar signs `$$ ... $$`, altough the [kramdown documentation](https://kramdown.gettalong.org/math_engine/mathjax.html) does not document the exact operation.


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

After launching this command, the updated documentation website should be available at [http://127.0.0.1:4000/faacets-gamma/](http://127.0.0.1:4000/faacets-gamma/) in your browser. Don't forget the trailing slash.


## Contributing to the database
The database of Bell inequalities managed by Faacets contains a lot of information on known Bell inequalities, but is not complete. If you wish to *add more Bell inequalities* to the database, or add *important missing information* (e.g. ) on the , you may do so by following the steps below. Note that contributions of this kind may be checked for quality before being merged into the main database. Also, such contributions should be reasonably sized to be considered.

[TODO]

## Contributing to the code
The Faacets source code is hosted on the [github page](https://github.com/denisrosset/faacets-gamma). To work on it and propose improvements to it, you may follow the next steps.

[TODO]
