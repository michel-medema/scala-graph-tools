# Scala Graph Tools

This repository contains a small collection of graph algorithms and data structures.

## `nl.rug.ds.common`

Several simple helper methods to deal with common tasks such as removing extensions from filenames and writing to files.

## `nl.rug.ds.graph.common`

Data structures for graphs and common graph functions, such as for computing the components of a graph and finding cliques.

## `nl.rug.ds.graph.communities`

An implementation of the Louvain algorithm that computes the communities of a graph. It is described in the paper

>V. D. Blondel, J.-L. Guillaume, R. Lambiotte, and E. Lefebvre, ‘Fast unfolding of communities in large networks’, J. Stat. Mech., vol. 2008, no. 10, p. P10008, Oct. 2008, doi: 10.1088/1742-5468/2008/10/P10008.

## `nl.rug.ds.graph.format.parser`

The object in `Pace.scala` reads a graph from a file or writes it to a file following the [PACE format](https://github.com/PACE-challenge/Treewidth?tab=readme-ov-file#input-format). The methods in `SeparatorFile.scala` can be used to read and write the vertex separators of a graph. This is a convenience class used in combination with the class `nl.rug.ds.graph.generator.SafeSeparatorGraph`.

## `nl.rug.ds.graph.generator`

A generator to create random graphs and a generator to create graphs with vertex separators that are known to be safe. Safe separators were first introduced in the paper

> H. L. Bodlaender and A. M. C. A. Koster, ‘Safe separators for treewidth’, Discrete Mathematics, vol. 306, no. 3, pp. 337–350, Feb. 2006, doi: 10.1016/j.disc.2005.12.017.

## `nl.rug.ds.graph.tree.decomposition`

`PaceTD.scala` provides a parser to read a tree decomposition from a file that is encoded using the [PACE format](https://github.com/PACE-challenge/Treewidth?tab=readme-ov-file#output-format). `Tamaki.scala` is a wrapper around [the treewidth algorithm of Hisao Tamaki](https://github.com/twalgor/tw). This algorithm has been implemented in Java. One of the GitHub workflows of this repository packages the algorithm and publishes it to the GitHub package registry, allowing it to be used directly in this project.

## `nl.rug.ds.graph.triangulation`

Data structures to store triangulated graphs and algorithms to produce triangulated graphs. Currently, two common heuristic triangulation algorithms are provided: min-fill and min-degree. This package also contains an implementation of the Heuristic Decomposition with Community Structure algorithm, which decomposes a graph using the boundaries of the communities as likely safe separators. This algorithm is described in the paper 

> M. Medema and A. Lazovik, ‘A safeness condition for minimal separators based on vertex connectivity’, Discrete Mathematics, vol. 348, no. 9, p. 114524, Sep. 2025, doi: 10.1016/j.disc.2025.114524.
