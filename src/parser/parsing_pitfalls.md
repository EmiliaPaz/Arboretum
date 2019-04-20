# Parsing Pitfalls

While overall Chevrotain has been a joy to work with, sometimes it requires
very non-obvious or unintuitive bits of infrastructure, and gives poor error
messages if these requirements aren't fulfilled.  If you're new to working
with the parser it would be a good idea to give this a scan, and it's a good
first resource for if you hit a dead end.  Feel free to add to it any time
something trips you up.

* Propogating Tokens from Lexing to Parsing: Since the tokens created in
  `lexing.js` are needed during parsing, they have to be imported there.
  First, make sure that any new tokens are added to `allTokens` in
  `lexing.js`.  Then, you'll want to bind the token to a name in `parsing.js`
  with something like `const NewToken = tokenVocabulary.NewToken`

* Multiple Occurences of the Same Token/Subrule:
  [see here](https://sap.github.io/chevrotain/documentation/3_3_0/classes/parser.html#consume)