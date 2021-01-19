# fun with diagrams

Various bits of visualization code based on the [diagrams](http://projects.haskell.org/diagrams/) library.

## RemyVis

[RemyVis.hs](RemyVis.hs): generates an animation of [Remy's algorithm](https://fr.wikipedia.org/wiki/Algorithme_de_R%C3%A9my) for uniformly generating random binary trees:

![Remy's algorithm animation](diagrams/tree50.gif)

## Luka

[Luka.hs](Luka.hs): some routines for visualizing generalized Łukasiewicz paths:

```haskell
*Luka> walkSVG' 1 "U2DU1DDU1DD"
```
![U2DU1DDU1DD](diagrams/U2DU1DDU1DD.svg)

```haskell
*Luka> walkSVG' 1 "U1DU2DU1DDD"
```
![U1DU2DU1DDD](diagrams/U1DU2DU1DDD.svg)

```haskell
*Luka> walkSVG' 0 "UUUD2UUD2UUUD2D2"
```
![UUUD2UUD2UUUD2D2](diagrams/UUUD2UUD2UUUD2D2.svg)

## LukaTree

[LukaTree.hs](LukaTree.hs): visualizing trees as paths.  (This uses routines for visualizing operadic trees from the [LinLam](https://github.com/noamz/linlam) library.)

Each tree is encoded as a Łukasiewicz path in four different ways, respectively via its preorder left-to-right, preorder right-to-left, postorder left-to-right, and postorder right-to-left traversals.

```haskell
> t = bin (bin (bin zer (bin zer zer)) zer) (bin (bin (bin zer zer) zer) (bin zer zer))
> renderPretty "binwalk.svg" (mkWidth 2048) (treeWalksDiagram t)
```
![binwalk](diagrams/binwalk.svg)

```haskell
> t = ter (bin zer zer) (ter zer zer (bin zer zer)) zer
> renderPretty "terwalk.svg" (mkWidth 1024) (treeWalksDiagram t)
```
![terwalk](diagrams/terwalk.svg)

```haskell
> renderPretty "allbin3walk.svg" (mkWidth 1024) (vsep 1 [treeWalksDiagram t | t <- kTree 2 3])
```
![allbin3walk](diagrams/allbin3walk.svg)

We can also consider the reverse translation from walks to trees.
Here we interpret a walk not touching the x-axis as a tree with free leaves, respectively via its preorder left-to-right and preorder right-to-left traversals:
```haskell
> w = readWalk "U1U1U1D1U1D1D1U1U1D1"
> renderPretty "walk2tree.svg" (mkWidth 1024) (hsep 1 [gridWalk 1 w # centerXY, treeDiagram (unpreLR w) # centerXY, treeDiagram (unpreRL w) # centerXY] # pad 1.1)
```
![allbin3walk](diagrams/walk2tree.svg)
