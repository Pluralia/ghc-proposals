Local Warning Pragmas
=====================

.. proposal-number:: 
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/234>`_.
.. sectnum::
.. contents::

We propose to add functionality for control warnings locally, in particular, add pragmas ``WARN``, ``IGNORE`` and ``ERROR``. For instance, in this code
::
 {-# OPTIONS_GHC -Wno-name-shadowing #-}

 f a =
   {-# WARN name-shadowing #-}
   \a -> a + a

 g a = \a -> a + a

there is ``-Wname-shadowing`` warning in funcion ``f`` but not in function ``g``.

This proposal rises some ticket problems:
 1. https://gitlab.haskell.org/ghc/ghc/issues/602
 2. https://gitlab.haskell.org/ghc/ghc/issues/10150

Motivation
------------

Warnings and errors by compiler help to write nice code. Using ``WARN``, ``IGNORE`` and ``ERROR`` pragmas you can more flexible configure warnings which are indicate bugs.

Consider some examples.

1. `Suppress orphan instance warning per instance <https://gitlab.haskell.org/ghc/ghc/issues/10150>`_. We disable ``-Worphans`` warning for ``instance ApplyFunc Box`` but warning for ``instance ApplyFunc Bottle`` works.
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

   
    {-# OPTIONS_GHC -Worphans #-}
    module Baz where

    import Foo
    import Bar

    instance {-# IGNORE orphans #-} ApplyFunc Box where
      func f Empty       = Empty
      func f (Content a) = Content $ f a

    instance ApplyFunc Bottle where
      func f Water    = Water
      func f (Milk a) = Milk $ f a

2. `Suppress particular kinds of warnings for parts of a source file <https://gitlab.haskell.org/ghc/ghc/issues/602>`_. In this example we don't get ``-Wunused-do-bind`` warning for ``f`` but get it for ``g``.
   ::
    {-# OPTIONS_GHC -Wunused-do-bind #-}

    f :: IO ()
    f = {-# IGNORE unused-do-bind #-} do
      getLine
      return ()

    g :: IO ()
    g = do
      getLine
      return ()
      
3. `Suppress the warning in case of incomplete patterns <https://stackoverflow.com/questions/12717909/stop-ghc-from-warning-me-about-one-particular-missing-pattern/>`_. Pragma ``IGNORE`` fixes it:
   ::
    {-# OPTIONS_GHC -Wincomplete-patterns #-}

    {-# INGNORE incomplete-patterns #-}
    f :: (Show a) => Maybe a -> String
    f (Just a) = show a
      
4. In this example you get warning ``-Wmissing-signatures`` for ``x`` but not for ``y``.
   ::
    {-# OPTIONS_GHC -Wmissing-signatures #-}

    x2 :: Int -> Int
    x2 = (* 2)

    x3 :: Int -> Int
    x3 = (* 3)

    x4 :: Int -> Int
    x4 = (* 4)

    x = 12
    
    {-# IGNORE missing-signatures #-}    
    y = 13

Proposed Change Specification
-----------------------------

GHC already support the ``OPTIONS_GHC`` pragma for configuring options for the file as a whole (in particular, configure warnings). **We propose to create new pragmas**:

1. ``WARN`` - enables warning locally
2. ``IGNORE`` - disables warning locally
3. ``ERROR`` - makes a specific warning into a fatal error localy

This pragmas use idea of (``-W``, ``-Wno-``, ``-Werror-``) batch switching of flags.

**Places for pragmas**:
 - expression
 - declaration
 - types

This pragmas use `meaning-preserving parsing rules <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0046-scc-parsing.rst>`_ for expressions and types. As for declarations - they apply to the following declaration.

Local work makes sense not for every warning. In case of misuse compiler gives some error.

Costs and Drawbacks
-------------------

1) **Estimate on development and maintenance costs**

Every warning can require individual way to collaborate with local using.

2) **Influence to learnability of the language**

This pragmas are optional pragmas and is non-essential for basic users of the language. The area of using intersects with ``OPTIONS_GHC`` pragma and as a result it does not require any more learning after the ``OPTIONS_GHC`` pragma. There is only one distinction - you need to learn where and how to place it inside the file (somewhat like the ``SCC`` pragma).

3) **Remaining drawbacks**

None.


Alternatives
------------

We proposed to create one pragma ``OPTIONS_LOCAL`` which works like ``OPTIONS_GHC`` and provides a local control warnings and language extensions. This idea was reject becase:

- every local language extension require individual way to implementation and can sense which is different from the global sence
- using one name ``OPTIONS_LOCAL`` for warning is not so comfortable

Unresolved Questions
--------------------

New flags
~~~~~~~~~

Local switching of warnings makes harder keeping track of using one specific warning. To "profile" local warnings avoid mistakes we propose to create following GHC warnings:

1. ``-Wlocal-warn`` - enable warning for every using of proposed pragmas
2. ``-Wunused-local-warn`` - enable warning for unused proposed pragmas

Local language extensions
~~~~~~~~~~~~~~~~~~~~~~~~~

There are three ways to local work with language extensions:

1. Create a new pragma ``LANGUAGE_LOCAL``
2. Tweak the LANGUAGE pragma to be acceptable in other places, not only at the top.
2. Create individual local pragmas for every extension when it makes sense
3. Forget this idea

Implementation Plan
-------------------

There is `the proof of concept implementation <https://gitlab.haskell.org/ghc/ghc/merge_requests/1029>`_.
