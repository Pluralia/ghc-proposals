Local Warning Pragmas
=====================

.. proposal-number:: 
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/234>`_.
.. sectnum::
.. contents::

We propose to add functionality for switching GHC-options locally. For instance, in this code
::
 {-# OPTIONS_GHC -Wno-name-shadowing #-}

 f a =
   {-# OPTIONS_LOCAL -Wname-shadowing #-}
   \a -> a + a

 g a = \a -> a + a

there is ``-Wname-shadowing`` warning in funcion ``f`` but not in function ``g``

This proposal rises some ticket problems:
 1. https://gitlab.haskell.org/ghc/ghc/issues/602
 2. https://gitlab.haskell.org/ghc/ghc/issues/10150

Motivation
------------

Warnings and errors by compiler help to write nice code. Using ``OPTIONS_LOCAL`` pragma you can more flexible configure compiler options.

Local control of warnings
~~~~~~~~~~~~~~~~~~~~~~~~~
 
1. Consider `suppress orphan instance warning per instance <https://gitlab.haskell.org/ghc/ghc/issues/10150>`_. We disable ``-Worphans`` warning for ``instance ApplyFunc Box`` but warning for `instance ApplyFunc Bottle` works.
   ::
    module Foo (
      ApplyFunc(..)
    ) where

    class ApplyFunc f where
      func :: (a -> b) -> f a -> f b

   
    module Bar (
      Box(..)
    , Bottle(..)
    ) where

    data Box a = Empty
               | Content a 

    data Bottle a = Water
                  | Milk a 

   
    {-# OPTIONS -Worphans #-}
    module Baz where

    import Foo
    import Bar

    instance {-# OPTIONS_LOCAL -fno-warn-orphans #-} ApplyFunc Box where
      func f Empty       = Empty
      func f (Content a) = Content $ f a

    instance ApplyFunc Bottle where
      func f Water    = Water
      func f (Milk a) = Milk $ f a

2. Consider `suppress particular kinds of warnings for parts of a source file <https://gitlab.haskell.org/ghc/ghc/issues/602>`_. In this example we don't get ``-Wunused-do-bind`` warning for ``f`` but get it for ``g``.
   ::
    {-# OPTIONS_GHC -Wunused-do-bind #-}

    f :: IO ()
    f = {-# OPTIONS_LOCAL -Wno-unused-do-bind #-} do
      getLine
      return ()

    g :: IO ()
    g = do
      getLine
      return ()
      
3. You get warning ``-Wmissing-signatures`` for ``x`` but not for ``y``.
   ::
    {-# OPTIONS_GHC -Wmissing-signatures #-}

    x2 :: Int -> Int
    x2 = (* 2)

    x3 :: Int -> Int
    x3 = (* 3)

    x4 :: Int -> Int
    x4 = (* 4)

    x = 12
    
    {-# OPTIONS_LOCAL -Wno-missing-signatures #-}    
    x' = 13

Using language extensions locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Comment from the ticket 602 <https://gitlab.haskell.org/ghc/ghc/issues/602#note_108677>`_: "It might be reasonable to consider adding arbitrary option-changes locally. (For example, I'd love to be able to turn on LANGUAGE pragmas only for part of a file"

1. Let's enable ``-XPartialTypeSignatures`` in ``f``. Such code doesn't compile because partial type signature wasn't allow in ``g``.
   ::
    f = {-# OPTIONS_LOCAL -XPartialTypeSignatures #-}
      let a :: _
          a = ()
       in ()
       
    g =
      let a :: _
          a = ()
       in ()
       
2. Function ``g`` fails because ``-XRecordWildCards`` was enabled only for ``f``.
   ::
    data Info = Info {x :: Bool, y :: Char, z :: Char, w :: String}

    {-# OPTIONS_GHC -XRecordWildCards #-}
    f :: Info -> String
    f (Info {x = False, ..}) = y : z : ' ' : w
    f (Info {x = True, ..})  = w ++ [' ', y, z]

    g :: Info -> String
    g (Info {x = False, ..})         = y : z : ' ' : w
    g (Info {x = True, y = '+', ..}) = w ++ [' ', z]

Proposed Change Specification
-----------------------------

GHC already support the ``OPTIONS_GHC`` pragma for configuring options for the file as a whole. We propose to create a similar pragma ``OPTIONS_LOCAL`` which will do the same things but locally. You can see what it looks like in the *Motivation section*.

Places for ``OPTIONS_LOCAL`` pragma:
 - expression
 - declaration
 - types

The pragma uses `meaning-preserving parsing rules <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0046-scc-parsing.rst>`_ for expressions and types. As for declarations - it applies to the following declaration.

If ``OPTIONS_LOCAL`` doesn't make sense compiler gives a warning.

Costs and Drawbacks
-------------------

1) **Estimate on development and maintenance costs**

Every compiler flag can require individual way to collaborate with ``OPTIONS_LOCAL`` pragma.

2) **Influence to learnability of the language**

*Learnability of the pragma*: ``OPTIONS_LOCAL`` is optional pragma and is non-essential for basic users of the language. The area of using intersects with ``OPTIONS_GHC`` pragma and as a result it does not require any more learning after the ``OPTIONS_GHC`` pragma. There is only one distinction - you need to learn where and how to place it inside the file (somewhat like the ``SCC`` pragma).

*Learnability of the language*: ``OPTIONS_LOCAL`` used close to the declaration. That means a basic user will search faster the name of the language extension  to which the new syntax corresponds.

3) **Remaining drawbacks**

None.


Alternatives
------------

None.

Unresolved Questions
--------------------

1) There is an idea to refuse the implementation of ``OPTIONS_LOCAL`` for language extensions. It can be too expensive (personal work for every extension). Instead of it ``OPTIONS_LOCAL`` for extensions can be splitted on some pragmas with the same local action. Here "the list of sense" for every extension https://gist.github.com/Pluralia/d1d0466ced9d211a9ebc2b944139de78.

2) There is `a question on Stackoverflow <https://stackoverflow.com/questions/12717909/stop-ghc-from-warning-me-about-one-particular-missing-pattern/>`_. It says to rid of the warning in case of incomplete patterns in a code you can use only ``error``. Whether is fine to add such feature using pragma ``OPTIONS_LOCAL``? It will be new flag.


Implementation Plan
-------------------

There is `the proof of concept implementation <https://gitlab.haskell.org/ghc/ghc/merge_requests/1029>`_.
