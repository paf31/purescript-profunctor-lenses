## Examples

- [Using Prisms with Sum Types](src/PrismsForSumTypes.purs)

## Conventions and the "why" behind them

The examples are meant to be read from top to bottom.

Every example is self-contained and can be imported into the repl. The
top of an example module will have a comment like this:

```purescript
{-   If you want to try out examples, paste the following into the repl.

import PrismsForSumTypes
import Data.Lens
...
-}
```

(Tip: because each line is self-contained, you don't need to use `:paste`.)

The modules contain both definitions of optics and sample uses. The
sample uses are executable code that look like this:

```purescript
s1 :: Maybe Color
s1 = preview solidFocus (Solid Color.white)
-- (Just rgba 255 255 255 1.0)
```

Why?

1. The second line can be pasted into the repl without needing to use
   `:paste`. (I usually don't copy the `s1 =` because I prefer seeing
   the results immediately. All types in the examples implement `show`,
   making that painless.)

2. The sample uses are executable code so that the compiler checks
   them and -- more importantly -- so that they stand out from the
   surrounding commentary. That makes the files more easily scannable
   when you're refreshing your memory about a particular function
   (assuming you use a syntax highlighter).

3. The name-value bindings (like `s1`) are clutter, but required by the
   compiler. Most of the type annotations are also clutter, but are required
   to prevent compiler warnings in, for example, `pulp build`.

4. The expected value is on a new line, rather than appended to the
   end of the second line, because it's visually easier to scan down a line
   than scan right for a `--` pattern.

   "Comfort is the key." -- Prof. Dawn Marick, DVM, MS, DACVIM (LAIM)
