This is the (toy subset of a) Scheme interpreter which I wrote while reading [*Write Yourself a Scheme in 48 Hours*] [1].

I've since used the code as a playground for experimenting with different parsing libraries. The original implementation used [Parsec] [2] (as does the book); I then refactored the parser using [attoparsec] [3], and finally using [trifecta] [4].

Both attoparsec and trifecta support incremental parsing, which enabled me to easily write a REPL supporting multi-line input, for example:

    >>> (+
    ...   3
    ...   2)
    5

However, attoparsec is designed for fast parsing of data formats, rather than parsing programming languages, and the error reporting is not great. Trifecta, on the other hand, is designed for parsing languages.

[1]: http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
[2]: http://hackage.haskell.org/package/parsec
[3]: http://hackage.haskell.org/package/attoparsec
[4]: http://hackage.haskell.org/package/trifecta
