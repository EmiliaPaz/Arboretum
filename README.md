# Arboretum

Arboretum is a project to explore the potential of displaying expressions and
their types as visual trees in a browsers.  In the course of this project, we
have begun building a new functional language, TreeScript (working name).

# Technologies

All of the language type checking and evaluation, as well as the rendering, is
performed in Elm.  Lexing and Parsing is handled by Chevrotain, a parsing
library written in Javascript.

# Building

After cloning the git repo, make sure you download
[Chevrotain](https://github.com/SAP/chevrotain), our parsing library:

```bash
npm install chevrotain
```

Also make sure you download browserify, which we use to bundle all of the
projects dependencies.

```bash
npm install -g browserify
```

To build the webpage, two javascript files need to be built: main.js
containing the compiled Elm code, and bundle.js containing the parser and its
dependencies.  The bash script `makearb.sh` automates the process, so run it
now, and after any changes to Elm or JavaScript code to propagate the changes
into the webpage.