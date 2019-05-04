"use strict"


const scriptLexer = require("./lexing")
// re-using the parser implemented in step two.
const parser = require("./parsing")
const SelectParser = parser.ScriptParser

// A new parser instance with CST output (enabled by default).
const parserInstance = new SelectParser([])
// The base visitor class can be accessed via the a parser instance.
const BaseScriptVisitor = parserInstance.getBaseCstVisitorConstructor()

function parseBool(string) {
    if(string === 'True') {
        return true
    }
    else if(string === 'False') {
        return false
    }
    else {
        throw "Parsing boolean from string failed!"
    }
}


class ToAstVisitor extends BaseScriptVisitor {
    constructor() {
        super()
        this.validateVisitor()
    }

    statement(ctx) {
        if(ctx.assignmentStatement) {
            return this.visit(ctx.assignmentStatement)
        }
        else if(ctx.typeStatement) {
            return this.visit(ctx.typeStatement)
        }
        else {
            throw "Statement has no valid type"
        }
    }

    typeStatement(ctx) {
        const assignType = this.visit(ctx.type)
        const id = ctx.Identifier[0].image

        return {
            type: "TYPE_STMT",
            identifier: id,
            assignType: assignType,
        }
    }

    type(ctx) {
        if(ctx.atomicType.length > 1) {
            const children = ctx.atomicType.map(node => this.visit(node))

            return {
                type: "FN_TYPE",
                children: children,
            }
        }
        else {
            return this.visit(ctx.atomicType)
        }
    }

    atomicType(ctx) {
        if(ctx.type) {
            return this.visit(ctx.type)
        }
        else {
            const value = ctx.BasicType[0].image
            return {
                type: "BASIC_TYPE",
                value: value,
            }
        }
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
        const expr = this.visit(ctx.fnExpression)

        return expr
    }

    fnExpression(ctx) {
        if(ctx.Lambda) {
            const variable = ctx.Identifier[0].image
            const body = this.visit(ctx.eqExpression)

            return {
                type: "FN_EXPR",
                variable: variable,
                body: body,
            }
        }
        else {
            return this.visit(ctx.eqExpression)
        }
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
        if(ctx.appExpression.length > 1) {
            const children = ctx.appExpression.map(node => this.visit(node))
            return {
                type: "MULT_EXPR",
                children: children,
            }
        }
        else {
            return this.visit(ctx.appExpression)
        }
    }

    appExpression(ctx) {
        if(ctx.atomicExpression.length > 1) {
            const children = ctx.atomicExpression.map(node => this.visit(node))
            return {
                type: "APP_EXPR",
                children: children,
            }
        }
        else {
            return this.visit(ctx.atomicExpression)
        }
    }


    atomicExpression(ctx) {
        if(ctx.tuple) {
            return this.visit(ctx.tuple)
        }
        else if(ctx.Integer) {
            return {
                type: "INT",
                value: parseInt(ctx.Integer[0].image),
            }
        }
        else if(ctx.Boolean) {
            return {
                type: "BOOL",
                value: parseBool(ctx.Boolean[0].image),
            }
        }
        else if(ctx.Identifier) {
            return {
                type: "ID",
                value: ctx.Identifier[0].image,
            }
        }
    }

    tuple(ctx) {
        const left = this.visit(ctx.expression)
        if(ctx.Comma) {    
            const right = this.visit(ctx.expression)
            return {
                type: "TUPLE",
                left: left,
                right: right, 
            }
        }
        else {
            return left
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
        const cst = parserInstance.statement()

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