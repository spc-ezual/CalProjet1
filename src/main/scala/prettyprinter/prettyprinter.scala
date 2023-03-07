package pretty_printer

import scala.util.Try

/** définition d'une exception pour le cas des listes vides de commandes
  */
case object ExceptionListeVide extends Exception

/** UN PRETTY-PRINTER POUR LE LANGAGE WHILE
  */
object Prettyprinter {

  /** définition d'un type pour les spécifications d'indentation
    */
  type IndentSpec = List[(String, Int)]

  /** définition d'une valeur d'indentation par défaut
    */
  val indentDefault: Int = 1

  /** TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
    */

  /** @param expression
    *   : un AST décrivant une expression du langage WHILE
    * @return
    *   une chaîne représentant la syntaxe concrète de l'expression
    */
  // TODO TP2
  def prettyPrintExpr(expression: Expression): String = {
    
    expression match{
      case Cons(arg1, arg2) => "(cons " + prettyPrintExpr(arg1) + " " + prettyPrintExpr(arg2) + ")" 
      case Cst(name) => name 
      case Eq(arg1, arg2) => prettyPrintExpr(arg1) + " =? " + prettyPrintExpr(arg2) 
      case Hd(arg) => "(hd " + prettyPrintExpr(arg) + ")"
      case Nl => "nil"
      case Tl(arg) => "(tl " + prettyPrintExpr(arg) + ")"
      case VarExp(name) => name
    }

  }

  /** FONCTIONS AUXILIAIRES DE TRAITEMENT DE CHAINES POUR L'INDENTATION DES
    * COMMANDES OU LA PRESENTATION DU PROGRAMME
    */

  /** recherche d'une valeur d'indentation dans une liste de spécifications
    * d'indentation
    *
    * @param context
    *   une chaîne de caractères décrivant un contexte d'indentation
    * @param is
    *   une liste de spécifications d'indentation, chaque spécification étant un
    *   couple (un contexte,une indentation) les contextes possibles seront, en
    *   majuscules, "WHILE", "FOR", "IF", ou "PROGR".
    * @return
    *   l'indentation correspondant à context
    */

  // TODO TP2
  def indentSearch(context: String, is: IndentSpec): Int = {
    is match {
    case Nil => indentDefault
    case (ctx, indent) :: tail =>
      if (ctx == context) indent
      else indentSearch(context, tail)
  }
  }

  /** création d'une indentation
    *
    * @param n
    *   un nombre d'espaces
    * @return
    *   une chaîne de n espaces
    */

  // TODO TP2
  def makeIndent(n: Int): String = {
    var espaces = ""
    for (i <-1 to n){ espaces = espaces + " "}
    espaces
  }

  /** ajout d'une chaîne devant chaque élément d'une liste non vide de chaînes
    *
    * @param pref
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de pref devant chaque
    *   élément de strings
    */

  // TODO TP2
  def appendStringBeforeAll(pref: String, strings: List[String]): List[String] ={
    strings match{
      case Nil => throw(ExceptionListeVide)
      case head::Nil => pref+head::Nil
      case head::tail => pref+head::appendStringBeforeAll(pref,tail)
    }
  }
  /** ajout d'une chaîne après chaque élément d'une liste non vide de chaînes
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après chaque
    *   élément de strings
    */

  // TODO TP2
  def appendStringAfterAll(suff: String, strings: List[String]): List[String] =
    {
    strings match{
      case Nil => throw(ExceptionListeVide)
      case head::Nil => head+suff::Nil
      case head::tail => head+suff::appendStringAfterAll(suff,tail)
    }
  }

  /** ajout d'une chaîne après le dernier élément d'une liste non vide de
    * chaînes
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après le
    *   dernier élément de strings
    */

  // TODO TP2
  def appendStringAfterLast(suff: String, strings: List[String]): List[String] ={
    strings match{
      case Nil => throw(ExceptionListeVide)
      case head::Nil => (head+suff)::Nil
      case head::tail => head::appendStringAfterLast(suff,tail)
    }
  }

  /** ajout d'une chaîne après chaque élément d'une liste non vide de chaînes
    * sauf le dernier
    *
    * @param suff
    *   une chaîne
    * @param strings
    *   une liste non vide de chaînes
    * @return
    *   une liste de chaînes obtenue par la concaténation de suff après chaque
    *   élément de strings sauf le dernier
    */

  // TODO TP2
  def appendStringAfterAllButLast(
      suff: String,
      strings: List[String]
  ): List[String] = {
    strings match{
      case Nil => throw(ExceptionListeVide)
      case head::Nil => head::Nil
      case head::tail => head+suff::appendStringAfterAllButLast(suff,tail)
    }
  }

  /** TRAITEMENT DES COMMANDES DU LANGAGE WHILE
    */

    
    def getVariableName(v: Variable): String = v match {
      case Var(name) => name
      case _          => throw new IllegalArgumentException("Expected Var instance")
    }
  /** @param command
    *   : un AST décrivant une commande du langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète de la commande
    */
  // TODO TP2
  def prettyPrintCommand(command: Command, is: IndentSpec): List[String] = {
    command match{
      case Nop => List("nop")
      case Set(variable, expression) => List(getVariableName(variable) + " := " + prettyPrintExpr(expression))
      case For(count, body) => ("for " + prettyPrintExpr(count) + " do")::(appendStringBeforeAll(makeIndent(indentSearch("FOR",is)),prettyPrintCommands(body,is)):+"od")
      case While(condition, body) => ("while " + prettyPrintExpr(condition) + " do")::(appendStringBeforeAll(makeIndent(indentSearch("WHILE",is)),prettyPrintCommands(body,is)):+"od")
      case If(condition, then_commands, else_commands) =>
        val thenBlock = appendStringBeforeAll(makeIndent(indentSearch("IF", is)), prettyPrintCommands(then_commands, is))
        val elseBlock = appendStringBeforeAll(makeIndent(indentSearch("IF", is)), prettyPrintCommands(else_commands, is))
        List("if " + prettyPrintExpr(condition) + " then") ::: thenBlock.map(_.toString) ::: List("else") ::: elseBlock.map(_.toString) ::: List("fi")

    }
  }

  /** @param commands
    *   : une liste non vide d'AST décrivant une liste non vide de commandes du
    *   langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète de la liste de
    *   commandes
    */
  // TODO TP2
  def prettyPrintCommands(
      commands: List[Command],
      is: IndentSpec
  ): List[String] = {
    commands match{
      case Nil => throw(ExceptionListeVide)
      case head :: Nil =>  prettyPrintCommand(head, is)
      case head :: tail =>  {val headLines = prettyPrintCommand(head, is)
                            val tailLines = prettyPrintCommands(tail, is)
                            headLines ::: tailLines}
    }
    }


  
  /** TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
    */

  /** @param vars
    *   : une liste non vide décrivant les paramètres d'entrée d'un programme du
    *   langage WHILE
    * @return
    *   une liste de chaînes représentant la syntaxe concrète des paramètres
    *   d'entrée du programme
    */
  // TODO TP2
  def prettyPrintIn(vars: List[Variable]): String = vars match {
    case Nil          => throw ExceptionListeVide
    case x :: Nil     => getVariableName(x)
    case x :: xs      => s"${getVariableName(x)}, ${prettyPrintIn(xs)}"
}


  /** @param vars
    *   : une liste non vide décrivant les paramètres de sortie d'un programme
    *   du langage WHILE
    * @return
    *   une liste de chaînes représentant la syntaxe concrète des paramètres de
    *   sortie du programme
    */
  // TODO TP2
  

  def prettyPrintOut(vars: List[Variable]): String = vars match {
    case Nil          => throw ExceptionListeVide
    case x :: Nil     => getVariableName(x)
    case x :: xs      => s"${getVariableName(x)}, ${prettyPrintOut(xs)}"
  }

  /** @param program
    *   : un AST décrivant un programme du langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    * @return
    *   une liste de chaînes représentant la syntaxe concrète du programme
    */
  // TODO TP2
  def prettyPrintProgram(program: Program, is: IndentSpec): List[String] = {
    program match {
    case Progr(ins, body, outs) =>
      val inStrs = ins.map("read " + getVariableName(_))
      val bodyStr = prettyPrintCommands(body, is)
      val outStrs = outs.map("write " + getVariableName(_))
      inStrs ::: "%" :: bodyStr.flatMap(str => List("", str)) ::: "%" :: outStrs
  }
    
  }

  /** @param program
    *   : un AST décrivant un programme du langage WHILE
    * @param is
    *   : une liste de spécifications d'indentation
    * @return
    *   une chaîne représentant la syntaxe concrète du programme
    */
  // TODO TP2
  def prettyPrint(program: Program, is: IndentSpec): String = {
    prettyPrintProgram(program,is).map( _ +"\n").mkString.dropRight(1)
  }

  val program: Program =
    Progr(
      List(Var("X")),
      List(
        Set(Var("Y"), Nl),
        While(
          VarExp("X"),
          List(
            Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))
          )
        )
      ),
      List(Var("Y"))
    );
  val is: IndentSpec = List(("PROGR", 2), ("WHILE", 5));

  def main(args: Array[String]): Unit = {
    println(prettyPrint(program, is));
  }

  /** UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
    *
    * les 3 fonctions suivantes permettent de construire un arbre de syntaxe
    * abstraite respectivement pour une expression, une commande, un programme
    */

  /** @param s
    *   : une chaine de caractère représentant la syntaxe concrète d'une
    *   expression du langage WHILE
    * @return
    *   un arbre de syntaxe abstraite pour cette expression
    */
  def readWhileExpression(s: String): Expression =
    WhileParser.analyserexpression(s)

  /** @param s
    *   : une chaine de caractère représentant la syntaxe concrète d'une
    *   commande du langage WHILE
    * @return
    *   un arbre de syntaxe abstraite pour cette commande
    */
  def readWhileCommand(s: String): Command = WhileParser.analysercommand(s)

  /** @param s
    *   : une chaine de caractère représentant la syntaxe concrète d'un
    *   programme du langage WHILE
    * @return
    *   un arbre de syntaxe abstraite pour ce programme
    */
  def readWhileProgram(s: String): Program = WhileParser.analyserprogram(s)

}
