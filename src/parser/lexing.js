"use strict"

const chevrotain = require("chevrotain")
const Lexer = chevrotain.Lexer
const createToken = chevrotain.createToken

// the vocabulary will be exported and used in the Parser definition.
const tokenVocabulary = {}

// createToken is used to create a TokenType
// The Lexer's output will contain an array of token Objects created by metadata
const Identifier = createToken({ name: "Identifier", pattern: /[a-zA-Z]\w*/ })

const Integer = createToken({ name: "Integer", pattern: /0|[1-9]\d*/ })

const Boolean = createToken({
    name: "Boolean",
    pattern: /True|False/,
    longer_alt: Identifier
    })

const Lambda = createToken({name: "Lambda", pattern: /\\/ })
const Arrow = createToken({name: "Arrow", pattern: /->/ })
const TypeAssignment = createToken({name: "TypeAssignment", pattern: /::/ })

const BasicType = createToken({
    name: "BasicType",
    pattern: /Int|Bool/,
    longer_alt: Identifier
    })

const Equivalence = createToken({name: "Equivalence", pattern: /==/ })
const Assignment = createToken({ name: "Assignment", pattern: /=/ })

const Addition = createToken({name: "Addition", pattern: /\+/ })
const Subtraction = createToken({name: "Subtraction", pattern: /-/ })
const Multiplication = createToken({name: "Multiplication", pattern: /\*/ })
const Division = createToken({name: "Division", pattern: /\//})
const Modulus = createToken({name: "Modulus", pattern: /%/})

const LogicalOR = createToken({name: "LogicalOR", pattern: /\|\|/ })
const LogicalAND = createToken({name: "LogicalAND", pattern: /&&/ })

const LParen = createToken({ name: "LParen", pattern: /\(/ })
const RParen = createToken({ name: "RParen", pattern: /\)/ })
const WhiteSpace = createToken({
    name: "WhiteSpace",
    pattern: /\s+/,
    group: Lexer.SKIPPED
})

const Comma = createToken({ name: "Comma", pattern: /,/ })

// The order of tokens is important
const allTokens = [
    WhiteSpace,
    Boolean,
    BasicType,
    Identifier,
    Lambda,
    Arrow,
    TypeAssignment,
    Comma,
    Integer,
    Equivalence,
    Assignment,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    LogicalOR,
    LogicalAND,
    LParen,
    RParen
]

const TreeLexer = new Lexer(allTokens)

allTokens.forEach(tokenType => {
    tokenVocabulary[tokenType.name] = tokenType
})

module.exports = {
    tokenVocabulary: tokenVocabulary,

    lex: function(inputText) {
        const lexingResult = TreeLexer.tokenize(inputText)

        if (lexingResult.errors.length > 0) {
            throw Error("Sad Sad Panda, lexing errors detected")
        }

        return lexingResult
    }
}
