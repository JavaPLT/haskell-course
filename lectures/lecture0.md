# Installing Stack

Stack is the build system that we want to use with GHC in this course.

You can find installation instructions for Haskell Stack [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

On Linux and Mac (even the M1 Mac), this seems to be running fine. 

If you are on Windows, it probably still runs fine. You can either use the link above, or you can try installing Stack using chocolatey, as described [here](https://www.haskell.org/platform/windows.html)

# After Installing Stack

1. After you have installed `stack`, clone [this repository](https://github.com/JavaPLT/haskell-course) to your local machine.
2. `cd` over to `haskell-code/` and run `stack build`. This will take a while.
3. Run `stack test`.

If the tests above pass, `stack` is working on your machine. Please go to `haskell-code/src/Lecture0.hs` and check the code there. Also, check the tests over at `haskell-code/test/Spec.hs`.

# GHCi (The REPL)

Run `stack ghci` or `stack repl` within `haskell-code/`. This is the REPL (Read-Eval-Print-Loop) environment, which should be an important part of your development cycle.

Try running these commands in GHCi. What do they do?

* `:l src/Lecture0.hs`
* `import Data.List`
* `take 10 [1..]`
* `:t take`
* `:set +s`
