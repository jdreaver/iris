# Iris: OpenGL Visualization in Haskell

Iris is an OpenGL-based visualization library for the Haskell programming
language. Iris aims to help users create fast interactive visualizations by
combining the expressiveness of Haskell and the power of OpenGL.

Iris takes inspiration from the [vispy](http://vispy.org/) and
[pyqtgraph](http://www.pyqtgraph.org/) Python plotting libraries.

# Motivation and Goals

Data visualization is a fundamental necessity for a few common tasks:

* Interactive and exploratory data analysis.
* GUI applications in scientific and engineering domains.
* Live visualization of real-time data.

Iris aims to help Haskell programmers with these tasks by:

* Leveraging OpenGL for fast visualizations with large data sets (using some
  current low-level Haskell OpenGL libraries as a substrate).
* Creating a high-level interface so users not familiar with OpenGL can quickly
  and easily create visualizations.
* Creating a common low-level interface to visualization components. Then,
  users with more more advanced OpenGL knowledge can easily add custom
  visualizations without having to reimplement common functionality (scene
  graphs, cameras, GUI event handlers, etc).
* Adding many GUI frameworks as backends, so iris visualizations can be
  integrated into native user interfaces.

# Design

All visualizations in iris are centered around the abstraction called a
[scene graph](https://en.wikipedia.org/wiki/Scene_graph). Our scene graph is
implemented partially with functional reactive programming (in particular, the
[reactive-banana](https://hackage.haskell.org/package/reactive-banana)
library), so users can efficiently modify scene graph components on-the-fly. We
try to implement as much as we can using pure functions or functions in the IO
monad, and then "connect the dots" using FRP so scenes can be dynamic and
interactive.

We want to have a solid library of common visuals for users to work with, along
with an easy and flexible way to create custom visuals.

# Project Status

Right now, the project is in an **experimental** stage. Hopefully we can have
an initial release and some cool examples to play with in the near future.

There is currently have a simple (but working!) scene graph system, along with
a few simple visuals, a GLFW backend, an event system, and some cameras. We
don't want to add too many features until the core design for each component is
solidified.

Some short-term goals are:

* Create a few representative examples so we can get feedback on the API.
* Create some high-quality, complex visuals so we can see if some of the scene
  graph design decisions are flexible enough for many use cases.
* Implement axes so users can create interactive 2D plots, instead of just
  plotting lines and shapes.
* Start creating an high-level API, so users don't have to even look at an FRP
  monad. That is, they should be able to declaratively create their canvas and
  their scene (with some optional callbacks into dynamic parts of the scene),
  and have it "just work."


# Contributing

Feel free to make a pull request or file an issue if you have suggestions or
find a bug. There won't be any contribution rules or guidelines until we
actually have some users :)
