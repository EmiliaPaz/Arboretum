"use strict"

// Adding a Parser (grammar only, only reads the input without any actions).
// Using the Token Vocabulary defined in the previous step.

const scriptLexer = require("./lexing")
const Parser = require("chevrotain").Parser
const tokenVocabulary = scriptLexer.tokenVocabulary

// individual imports, prefer ES6 imports if supported in your runtime/transpiler...
const Identifier = tokenVocabulary.Identifier
const Integer = tokenVocabulary.Integer
const Boolean = tokenVocabulary.Boolean
const Lambda = tokenVocabulary.Lambda
const Arrow = tokenVocabulary.Arrow
const TypeAssignment = tokenVocabulary.TypeAssignment
const BasicType = tokenVocabulary.BasicType
const Equivalence = tokenVocabulary.Equivalence
const Assignment = tokenVocabulary.Assignment
const Addition = tokenVocabulary.Addition
const Subtraction = tokenVocabulary.Subtraction
const Multiplication = tokenVocabulary.Multiplication
const Modulus = tokenVocabulary.Modulus
const LogicalOR = tokenVocabulary.LogicalOR
const LogicalAND = tokenVocabulary.LogicalAND
const LParen = tokenVocabulary.LParen
const RParen = tokenVocabulary.RParen

// ----------------- parser -----------------
class ScriptParser extends Parser {
    // A config object as a constructor argument is normally not needed.
    // Our tutorial scenario requires a dynamic configuration to support step3 without duplicating code.
    constructor(config) {
        super(tokenVocabulary, config)

        // for conciseness
        const $ = this

        $.RULE("statement", () => {
            $.OR([
                { ALT: () => { $.SUBRULE($.typeStatement) }},
                { ALT: () => { $.SUBRULE($.assignmentStatement) }},
            ])
        })

        $.RULE("typeStatement", () => {
            $.CONSUME(Identifier)
            $.CONSUME(TypeAssignment)
            $.SUBRULE($.type)
        })

        $.RULE("type", () => {
            $.SUBRULE($.atomicType)
            $.MANY( () => {
                $.CONSUME(Arrow)
                $.SUBRULE2($.atomicType)
            })
        })

        $.RULE("atomicType", () => {
            $.OR([
                { ALT: () => {
                    $.CONSUME(LParen)
                    $.SUBRULE($.type)
                    $.CONSUME(RParen)
                }},
                { ALT: () => { $.CONSUME(BasicType) }},
            ])

        })

        $.RULE("assignmentStatement", () => {
            $.CONSUME(Identifier)
            $.CONSUME(Assignment)
            $.SUBRULE($.expression)
        })

        $.RULE("expression", () => {
            $.SUBRULE($.fnExpression)
        })

        $.RULE("fnExpression", () => {
            $.OR([
                { ALT: () => {
                    $.CONSUME(Lambda)
                    $.CONSUME(Identifier)
                    $.CONSUME(Arrow)
                    $.SUBRULE($.eqExpression)
                }},
                { ALT: () => { $.SUBRULE2($.eqExpression) }},
            ])
        })

        $.RULE("eqExpression", () => {
            $.SUBRULE($.orExpression)
            $.OPTION( () => {
                $.CONSUME(Equivalence)
                $.SUBRULE2($.orExpression)
            })
        })

        $.RULE("orExpression", () => {
            $.SUBRULE($.andExpression)
            $.MANY( () => {
                $.CONSUME(LogicalOR)
                $.SUBRULE2($.andExpression)
            })
        })

        $.RULE("andExpression", () => {
            $.SUBRULE($.subtExpression)
            $.MANY( () => {
                $.CONSUME(LogicalAND)
                $.SUBRULE2($.subtExpression)
            })
        })

        $.RULE("subtExpression", () => {
            $.SUBRULE($.addExpression)
            $.MANY( () => {
                $.CONSUME(Subtraction)
                $.SUBRULE2($.addExpression)
            })
        })

        $.RULE("addExpression", () => {
            $.SUBRULE($.multExpression)
            $.MANY( () => {
                $.CONSUME(Addition)
                $.SUBRULE2($.multExpression)
            })
        })

        $.RULE("multExpression", () => {
            $.SUBRULE($.modExpression)
            $.MANY( () => {
                $.CONSUME(Multiplication)
                $.SUBRULE2($.modExpression)
            })
        })

        $.RULE("modExpression", () => {
            $.SUBRULE($.appExpression)
            $.MANY( () => {
                $.CONSUME(Modulus)
                $.SUBRULE2($.appExpression)
            })
        })

        $.RULE("appExpression", () => {
            $.SUBRULE($.atomicExpression)
            $.MANY( () => {
                $.SUBRULE2($.atomicExpression)
            })
        })

        $.RULE("atomicExpression", () => {
            $.OR([
                { ALT: () => {
                    $.CONSUME(LParen)
                    $.SUBRULE($.expression)
                    $.CONSUME(RParen)
                }},
                { ALT: () => $.CONSUME(Integer) },
                { ALT: () => $.CONSUME(Boolean) },
                { ALT: () => $.CONSUME(Identifier) }
            ])
        })


        // very important to call this after all the rules have been defined.
        // otherwise the parser may not work correctly as it will lack information
        // derived during the self analysis phase.
        this.performSelfAnalysis()
    }
}

// We only ever need one as the parser internal state is reset for each new input.
const parserInstance = new ScriptParser()

module.exports = {
    parserInstance: parserInstance,

    ScriptParser: ScriptParser,

    parse: function(inputText) {
        const lexResult = scriptLexer.lex(inputText)

        // ".input" is a setter which will reset the parser's internal's state.
        parserInstance.input = lexResult.tokens

        parserInstance.assignmentStatement()

        if (parserInstance.errors.length > 0) {
            throw Error(
                "Parsing Error:\n" +
                    parserInstance.errors[0].message
            )
        }
    }
}
