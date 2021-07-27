package closure

import kotlinx.collections.immutable.persistentHashMapOf

sealed class Token {

    override fun toString(): String {
        return this.javaClass.simpleName
    }

    // Keywords
    object IF : Token()
    object THEN : Token()
    object ELSE : Token()
    object LET : Token()
    object IN : Token()


    // Symbols
    object LPAREN : Token()
    object RPAREN : Token()
    object BACKSLASH : Token()
    object ARROW : Token()
    object EQUALS : Token()
    object LBRACE : Token()
    object RBRACE : Token()
    object COMMA : Token()
    object POINT : Token()

    // Operatoren
    object PLUS : Token()
    object MINUS : Token()
    object MUL : Token()
    object DOUBLE_EQUALS : Token()

    data class IDENT(val ident: String) : Token()

    // Literals
    data class BOOLEAN_LIT(val b: Boolean) : Token()
    data class NUMBER_LIT(val n: Int) : Token()

    // Control Token
    object EOF : Token()
}

class PeekableIterator<A>(private val iter: Iterator<A>) {
    private var lookahead: A? = null
    fun next(): A {
        lookahead?.let { lookahead = null; return it }
        return iter.next()
    }

    fun peek(): A {
        val token = next()
        lookahead = token
        return token
    }

    fun hasNext(): Boolean {
        return lookahead != null || iter.hasNext()
    }
}

class Lexer(input: String) {

    private val iter: PeekableIterator<Char> = PeekableIterator(input.iterator())
    private var lookahead: Token? = null

    public fun next(): Token {
        lookahead?.let { lookahead = null; return it }
        consumeWhitespace()
        if (!iter.hasNext()) {
            return Token.EOF
        }
        return when (val c = iter.next()) {
            '(' -> Token.LPAREN
            ')' -> Token.RPAREN
            '{' -> Token.LBRACE
            '}' -> Token.RBRACE
            '+' -> Token.PLUS
            '-' -> Token.MINUS
            '*' -> Token.MUL
            '\\' -> Token.BACKSLASH
            '=' -> when (iter.peek()) {
                '>' -> {
                    iter.next()
                    Token.ARROW
                }
                '=' -> {
                    iter.next()
                    Token.DOUBLE_EQUALS
                }
                else -> Token.EQUALS
            }
            ',' -> Token.COMMA
            '.' -> Token.POINT
            else -> when {
                c.isJavaIdentifierStart() -> ident(c)
                c.isDigit() -> number(c)
                else -> throw Exception("Unexpected char: '$c'")
            }
        }
    }

    public fun peek(): Token {
        val token = next()
        lookahead = token
        return token
    }

    private fun number(c: Char): Token {
        var result = c.toString()
        while (iter.hasNext() && iter.peek().isDigit()) {
            result += iter.next()
        }
        return Token.NUMBER_LIT(result.toInt())
    }

    private fun ident(c: Char): Token {
        var result = c.toString()
        while (iter.hasNext() && iter.peek().isJavaIdentifierPart()) {
            result += iter.next()
        }
        return when (result) {
            "true" -> Token.BOOLEAN_LIT(true)
            "false" -> Token.BOOLEAN_LIT(false)
            "if" -> Token.IF
            "then" -> Token.THEN
            "else" -> Token.ELSE
            "let" -> Token.LET
            "in" -> Token.IN
            else -> Token.IDENT(result)
        }
    }

    private fun consumeWhitespace() {
        while (iter.hasNext()) {
            val c = iter.peek()
            if (!c.isWhitespace()) break
            iter.next()
        }
    }
}

class Parser(val tokens: Lexer) {

    fun parseExpr(): Expr {
        return parseBinary(0)
    }

    fun parseBinary(minBP: Int): Expr {
        var lhs: Expr = parseApplication()
        while (true) {
            val op = parseOperator() ?: break
            val (leftBP, rightBP) = bindingPower(op)
            if (leftBP < minBP) {
                break
            }
            tokens.next()
            val rhs = parseBinary(rightBP)
            lhs = Expr.Binary(op, lhs, rhs)
        }

        return lhs
    }

    private fun parseOperator(): Operator? {
        return when(tokens.peek()) {
            Token.PLUS -> Operator.Plus
            Token.MINUS -> Operator.Minus
            Token.MUL -> Operator.Multiply
            Token.DOUBLE_EQUALS -> Operator.Equals
            else -> null
        }
    }

    fun bindingPower(op: Operator): Pair<Int, Int> {
        return when(op) {
            Operator.Equals -> 1 to 2
            Operator.Plus, Operator.Minus -> 3 to 4
            Operator.Multiply -> 5 to 6
        }
    }

    fun parseApplication(): Expr {
        val func = parseAtom()
        val args: MutableList<Expr> = mutableListOf()
        while (true) {
            args += tryParseAtom() ?: break
        }
        return args.fold(func) { acc, arg -> Expr.Application(acc, arg) }
    }

    fun parseAtom(): Expr {
        return tryParseAtom() ?: throw Exception("Expected expression, but saw unexpected token: ${tokens.peek()}")
    }

    fun tryParseAtom(): Expr? {
        return when (val t = tokens.peek()) {
            is Token.BOOLEAN_LIT -> parseBoolean()
            is Token.NUMBER_LIT -> parseNumber()
            is Token.IDENT -> parseVar()
            is Token.IF -> parseIf()
            is Token.BACKSLASH -> parseLambda()
            is Token.LPAREN -> parseParenthesized()
            is Token.LET -> parseLet()
            is Token.LBRACE -> parseBracethesized()
            else -> null
        }
    }

    private fun parseLet(): Expr {
        expectNext<Token.LET>("let")
        val binder = expectNext<Token.IDENT>("binder").ident
        expectNext<Token.EQUALS>("equals")
        val expr = parseExpr()
        expectNext<Token.IN>("in")
        val body = parseExpr()
        return Expr.Let(binder, expr, body)
    }

    private fun parseBoolean(): Expr {
        val t = expectNext<Token.BOOLEAN_LIT>("boolean literal")
        return Expr.Boolean(t.b)
    }

    private fun parseNumber(): Expr {
        val t = expectNext<Token.NUMBER_LIT>("number literal")
        return Expr.Number(t.n)
    }

    private fun parseVar(): Expr {
        val t = expectNext<Token.IDENT>("identifier")
        if(tokens.peek() is Token.POINT){
            expectNext<Token.POINT>("point")
            return Expr.Projection(parseVar(), t.ident)
        }
        return Expr.Var(t.ident)
    }

    private fun parseParenthesized(): Expr {
        expectNext<Token.LPAREN>("(")
        val inner = parseExpr()
        expectNext<Token.RPAREN>(")")
        return inner
    }

    private fun parseBracethesized(): Expr {
        // { x = 7,  y = true}
        expectNext<Token.LBRACE>("{")
        // defintion
        var pairs = persistentHashMapOf<String, Expr>()
        var binder = Expr.Var(expectNext<Token.IDENT>("ident").ident)
        expectNext<Token.EQUALS>("=")
        var expr = parseExpr()
        pairs = pairs.put(binder.name,expr)

        while (tokens.peek().equals(Token.COMMA)) {
            expectNext<Token.COMMA>(",")
            binder = Expr.Var(expectNext<Token.IDENT>("ident").ident)
            expectNext<Token.EQUALS>("=")
            expr = parseExpr()
            pairs = pairs.put(binder.name,expr)
        }

        expectNext<Token.RBRACE>("}")
        return Expr.Record(pairs)
    }

    private fun parseLambda(): Expr {
        // \binder => body
        expectNext<Token.BACKSLASH>("\\")
        val binder = expectNext<Token.IDENT>("ident").ident
        expectNext<Token.ARROW>("=>")
        val body = parseExpr()
        return Expr.Lambda(binder, body)
    }

    // if true then 3 else 4
    private fun parseIf(): Expr.If {
        expectNext<Token.IF>("if")
        val condition = parseExpr()
        expectNext<Token.THEN>("then")
        val thenBranch = parseExpr()
        expectNext<Token.ELSE>("else")
        val elseBranch = parseExpr()
        return Expr.If(condition, thenBranch, elseBranch)
    }

    private inline fun <reified A>expectNext(msg: String): A {
        val next = tokens.next()
        if (next !is A) {
            throw Exception("Unexpected token: expected $msg, but saw $next")
        }
        return next
    }
}


fun test(input: String) {
    println("Lexing: $input")
    val lexer = Lexer(input)
    while (lexer.peek() != Token.EOF) {
        println(lexer.next())
    }
    println(lexer.next())
}

fun testParser(input: String) {
    println("Parsing: $input")
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    println(parser.parseExpr())
}

fun main() {
    /*test("""
        if (\x => equals 25 x) 20
        then true
        else add 3 (add 4 5)
    """.trimIndent())

    // Uebung:
    // 1. Zeilenkommentare (Tipp: Kommentare als Whitespace behandeln)
    // 2. Bonus(schwer): Blockkommentare
    // 3. BonusBonus(schwer!): Geschachtelte Blockkommentare
//    test("""
//        if (\x => equals 25 x) 20
//        // Huge if true
//        then true
//        // smol
//        /* ich
//        bin auch
//        ein Kommentar
//        */
//        /* Ich /* bin geschachtelt */ */
//        else add 3 (add 4 5)
//    """.trimIndent())


    testParser("42")
    testParser("true")
    testParser("imAVar")
    testParser("if true then 3 else 4")
    testParser("""\x => \y => 10""")
    testParser("5")


    val input = """(\x => if x then 3 else 4) false"""
    println(eval(persistentHashMapOf(), Parser(Lexer(input)).parseExpr()))

    testParser("1 + 2")*/

//    testEval("""
//        let myrecord = {x = {x = 5}, y = true} in
//        myrecord
//        """.trimIndent()
//    )
//
//    testEval("""
//        let myrecord = {x = {x = 5}, y = true} in
//        myrecord.x.x
//        """.trimIndent()
//    )
//
//    testEval("""
//        let projectx =
//            \\r => r.x in
//        let myrecord =
//            {x = \\r => 2 + r, y = 9} in
//        projectx myrecord 10
//        """.trimIndent()
//    )
//
//    testEval(
//        """
//        let myrecord = {x = 5, y = if 5 == 4 then true else false} in
//        let myrecord2 = {x = 10} in
//        let x = 2 in
//        if myrecord.y then
//            myrecord.x
//          else
//            myrecord2.x + x
//        """.trimIndent()
//    )
//
//    testEval(
//        """
//            let r = {x = 3, y = \f => if f == 3 then false else true} in
//            let r2 = {z = 2} in
//            if r.y r2.z then
//                r.x
//            else
//                r.x + r.x
//        """.trimIndent()
//    )


    testEval(
        """
        let gregor =    { age = 20, matrikelnumber = 87654321, semester = 2, 
                                        grades = { puc = 2, pp = 2, mathe = 2, mci = 1 } } in
        let max =       { age = 25, matrikelnumber = 12348765, semester = 5, 
                                        grades = { puc = 3, pp = 2, mathe = 3, mci = 1 } } in
        let beate =     { age = 24, matrikelnumber = 12341234, semester = 6, 
                                        grades = { puc = 2, pp = 2, mathe = 3, mci = 4 } } in
        let anna =      { age = 25, matrikelnumber = 12344321, semester = 4, 
                                        grades = { puc = 1, pp = 2, mathe = 3, mci = 1 } } in
        let peter =     { age = 22, matrikelnumber = 43215678, semester = 1, 
                                        grades = { puc = 0, pp = 0, mathe = 4, mci = 0 } } in
        let compareRecord =     { semester =    \comparedSemester => \person => if comparedSemester == person.semester then true else false,
                                  age =         \personA => \personB => if personA.age == personB.age then true else false
                                } in
        let gradesRecord =      { puc =         \person => if person.grades.puc == 0 then false else person.grades.puc,
                                  pp =          \person => if person.grades.pp == 0 then false else person.grades.pp,
                                  mathe =       \person => if person.grades.mathe == 0 then false else person.grades.mathe,
                                  mci =         \person => if person.grades.mci == 0 then false else person.grades.mci
                                } in
        gregor
    """.trimIndent()
    )

//    gregor
//    compareRecord.semester 3 gregor
//    compareRecord.age gregor max
//    gradesRecord.puc gregor
//    gradesRecord.pp gregor
//    gradesRecord.mathe gregor
//    gradesRecord.mci gregor


}

