"use strict"


const scriptLexer = require("./lexing")
// re-using the parser implemented in step two.
const parser = require("./parsing")
const SelectParser = parser.ScriptParser

// A new parser instance with CST output (enabled by default).
const parserInstance = new SelectParser([])
// The base visitor class can be accessed via the a parser instance.
const BaseScriptVisitor = parserInstance.getBaseCstVisitorConstructor()


class ToAstVisitor extends BaseScriptVisitor {
    constructor() {
        super()
        this.validateVisitor()
    }

    assignmentStatement(ctx) {
        const id = ctx.Identifier[0].image

        const exp = this.visit(ctx.expression)

        return {
            type: "ASSIGN_STMT",
            identifier: id,
            expression: exp
        }
    }

    // expression rule is some sort of wrapper right now
    expression(ctx) {
        const expr = this.visit(ctx.eqExpression)

        return expr
    }

    /*
    This and many of the following visitor rules check to see if more than one
    subexpression exists to see if the rule was actually used.  If it was, a
    json node is returned.  Otherwise, the results of visiting further down
    the tree are returned.
    */
    eqExpression(ctx) {
        if(ctx.orExpression.length > 1) {
            const lhs = this.visit(ctx.orExpression[0])
            const rhs = this.visit(ctx.orExpression[1])
            return {
                type: "EQ_EXPR",
                lhs: lhs,
                rhs: rhs,
            }
        }
        else {
            return this.visit(ctx.orExpression)
        }
    }

    orExpression(ctx) {
        if(ctx.andExpression.length > 1) {
            const children = ctx.andExpression.map(node => this.visit(node))
            return {
                type: "OR_EXPR",
                children: children,
            }
        }
        else {
            return this.visit(ctx.andExpression)
        }
    }

    andExpression(ctx) {
        if(ctx.subtExpression.length > 1) {
            const children = ctx.subtExpression.map(node => this.visit(node))
            return {
                type: "AND_EXPR",
                children: children,
            }
        }
        else {
            return this.visit(ctx.subtExpression)
        }
    }

    subtExpression(ctx) {
        if(ctx.addExpression.length > 1) {
            const children = ctx.addExpression.map(node => this.visit(node))
            return {
                type: "SUBT_EXPR",
                children: children,
            }
        }
        else {
            return this.visit(ctx.addExpression)
        }
    }

    addExpression(ctx) {
        if(ctx.multExpression.length > 1) {
            const children = ctx.multExpression.map(node => this.visit(node))
            return {
                type: "ADD_EXPR",
                children: children,
            }
        }
        else {
            return this.visit(ctx.multExpression)
        }
    }
    
    multExpression(ctx) {
        if(ctx.atomicExpression.length > 1) {
            const children = ctx.atomicExpression.map(node => this.visit(node))
            return {
                type: "MULT_EXPR",
                children: children,
            }
        }
        else {
            return this.visit(ctx.atomicExpression)
        }
    }

    atomicExpression(ctx) {
        if(ctx.expression) {
            return this.visit(ctx.expression)
        }
        else if(ctx.Integer) {
            return {
                type: "INT",
                value: ctx.Integer[0].image,
            }
        }
        else if(ctx.Boolean) {
            return {
                type: "BOOL",
                value: ctx.Boolean[0].image,
            }
        }
        else if(ctx.Identifier) {
            return {
                type: "ID",
                value: ctx.Identifier[0].image,
            }
        }
    }

}


// Our visitor has no state, so a single instance is sufficient.
const toAstVisitorInstance = new ToAstVisitor()

module.exports = {
    toAst: function(inputText) {
        const lexResult = scriptLexer.lex(inputText)

        // ".input" is a setter which will reset the parser's internal's state.
        parserInstance.input = lexResult.tokens

        // Automatic CST created when parsing
        const cst = parserInstance.assignmentStatement()

        if (parserInstance.errors.length > 0) {
            throw Error(
                "Parse Error:\n" +
                    parserInstance.errors[0].message
            )
        }

        const ast = toAstVisitorInstance.visit(cst)

        return ast
    }
}