# language-powerquery
PowerQuery (M Language) Parser
- Lexer ([alex])
- Parser [happy])
- Pretty Printer

## Specification
- [Power Query M Formula Language Specification PDF]
- Code follows specification closely  

![alt text][spec_vs_code]


## TODO

- [ ] Add line numbers (etc.) to `Annotation`
- [ ] Package for Hackage
- [ ] More documentation
- [ ] Missing Lexer/Parser special keywords ([Link])

[Power Query M Formula Language Specification PDF]: https://docs.microsoft.com/en-us/powerquery-m/power-query-m-language-specification
[spec_vs_code]: raw/spec_vs_code.png
[alex]: https://github.com/simonmar/alex
[happy]: https://github.com/simonmar/happy
[Link]: https://github.com/Atidot/language-powerquery/blob/8d40aa07d9a37dd2b87b4476850448291f656241/language-powerquery/src/Language/PowerQuery/Parser.y#L183
