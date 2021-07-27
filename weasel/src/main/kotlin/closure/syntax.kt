package closure

sealed class Expr {
    data class Var(val name: String) : Expr()
    data class Lambda(val binder: String, val body: Expr) : Expr()
    data class Application(val func: Expr, val arg: Expr) : Expr()
    data class Number(val n: Int) : Expr()
    data class Record(val lets : PersistentMap<String, Expr>) : Expr()
    data class Projection(val expr: Expr, val field: String) : Expr()
    data class Boolean(val b: kotlin.Boolean) : Expr()
    data class Binary(val operator: Operator, val x: Expr, val y: Expr) : Expr()
    data class If(val condition: Expr, val thenBranch: Expr, val elseBranch: Expr) : Expr()
    data class Let(val binder: String, val expr: Expr, val body: Expr) : Expr()
}

enum class Operator {
    Equals, Plus, Minus, Multiply
}