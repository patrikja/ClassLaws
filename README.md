#ClassLaws: Testing Type Class Laws

http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/ClassLaws

This repository has the library source code ([also available from hackage](http://hackage.haskell.org/package/ClassLaws))
```Shell
  cabal install ClassLaws
```

It has not been maintained since 2012 - patches are welcome!

Other links
* [Paper (by Jeuring, Jansson, Amaral)](http://dl.acm.org/citation.cfm?id=2364514&CFID=114228077&CFTOKEN=91363922) published at the [Haskell Symposium 2012](http://www.haskell.org/haskell-symposium/2012/)
* [Chalmers publication library entry](http://publications.lib.chalmers.se/publication/160720-testing-type-class-laws)
* Also available as [Tech. report UU-CS-2012-008](http://www.cs.uu.nl/research/techreps/UU-CS-2012-008.html)
* [Haskell Symposium talk video](http://www.youtube.com/watch?v=-IZjuQNC_uk&feature=plcp) (Johan Jeuring presenting)

##Paper abstract

The specification of a class in Haskell often starts with stating, in comments, the laws that should be satisfied by methods defined in instances of the class, followed by the type of the methods of the class. This paper develops a framework that supports testing such class laws using QuickCheck. Our framework is a light-weight class law testing framework, which requires a limited amount of work per class law, and per datatype for which the class law is tested. We also show how to test class laws with partially-defined values. Using partially-defined values, we show that the standard lazy and strict implementations of the state monad do not satisfy the expected laws.

##Bibtex

```TeX
@InProceedings{jeuringHaskell12ClassLaws,
  author =	 {Johan Jeuring and Patrik Jansson and Cl\'audio Amaral},
  title =	 {Testing type class laws},
  longbooktitle ={Proceedings of the 2012 symposium on Haskell},
  booktitle =	 {Haskell'12},
  year =	 2012,
  pages =	 {49--60},
  numpages =	 12,
  doi =		 {10.1145/2364506.2364514},
  COMMENTisbn =	 {978-1-4503-1574-6},
  COMMENTlocation ={Copenhagen, Denmark},
  publisher =	 {ACM},
  keywords =	 {classes, laws, state monad, testing}
}
```
