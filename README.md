# Haskript

A tool for compiling and running Haskell scripts with minimal overhead.

### The Stack way

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-2.9 --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
import Turtle
main = echo "Hello World!"
```

### The Haskript way

Simply change the executable from `stack` to `Haskript`

```haskell
#!/usr/bin/env haskript
-- stack --resolver lts-2.9 --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
import Turtle
main = echo "Hello World!"
```

### What's the point?

With Haskript, you don't need to wait for the overhead of stack to run
nor run your scripts in an interpreter. Haskript manages your scripts for
you by compiling them at first and simply running the compiled version if
it is available and up-to-date.

### Configuring your editor

We don't really _need_ to use `.hs` as an extension for our Haskripts
since we're using the shebang line. Instead, we can tell our editor
about this hint.

For Vim support, add the following to `~/.vim/ftdetect/haskript.vim`

```vim
fun s:DetectCustomLang()
  if getline(1) =~# '^#!.*\<haskript\>'
    setfiletype haskell
  endif
endfun

autocmd BufNewFile,BufRead * call s:DetectCustomLang()
```

Now Vim will automatically recognize your Haskripts as Haskell source files.
