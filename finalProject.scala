/************* GUI *************/
import scala.swing._
import scala.swing.event._
import java.awt.{Color, Graphics2D};

/************************************************************

HOMEWORK 5

Team members: Kristen Behrakis, Minju Kang

Emails: kristen.behrakis@students.olin.edu, minju.kang@students.olin.edu

Remarks, if any: Instead of creating a new class Esubdictionary, we could've done Edictionary.env to extract an environment and evaluate it

************************************************************/


/*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions and methods below are stubs that you should
 * replace with your own implementation.
 *
 * PLEASE DO NOT CHANGE THE TYPES
 * Doing so risks making it impossible for me to test your code.
 *
 * Always make sure you can run this file as a whole with scala
 * by running
 *    scala <file>
 * before submitting it. It has to load without any errors.
 *
 */


//
//  Values
//


abstract class Value {

   // default behaviors for values

   def isInteger () : Boolean = false
   def isBoolean () : Boolean = false
   def isVector () : Boolean = false
   def isFunction () : Boolean = false
   def isDictionary () : Boolean = false
   def isString () : Boolean = false

   def error (msg : String) : Nothing = {
      throw new Exception("Value error: "+ msg + "\n   in value " + this)
   }

   def getString () : String = {
      throw new Exception("Value not of type STRING")
   }

   def getInt () : Int = {
      throw new Exception("Value not of type INTEGER")
   }

   def getBool () : Boolean = {
      throw new Exception("Value not of type BOOLEAN")
   }

   def getList () : List[Value] = {
      throw new Exception("Value not of type VECTOR")
   }

   def apply (args: List[Value]) : Value =  {
      throw new Exception("Value not of type FUNCTION")
   }

   def checkInteger () : Unit = {
     if (!isInteger()) {
        error("Value not of type INTEGER")
     }
   }

   def checkBoolean () : Unit = {
     if (!isBoolean()) {
        error("Value not of type BOOLEAN")
     }
   }

   def checkVector () : Unit = {
     if (!isVector()) {
        error("Value not of type VECTOR")
     }
   }

   def checkFunction () : Unit = {
     if (!isFunction()) {
        error("Value not of type FUNCTION")
     }
   }

   def access () : Unit = {
     if (!isDictionary()){
       throw new Exception("Value not of type DICTIONARY")
     }
   }

}


class VString (val i:String) extends Value {

  override def isString () : Boolean = true
  override def getString () : String = i

}


class VInteger (val i:Int) extends Value {

  override def toString () : String = i.toString()
  override def isInteger () : Boolean = true
  override def getInt () : Int = i

}


class VBoolean (val b:Boolean) extends Value {

  override def toString () : String = b.toString()
  override def isBoolean () : Boolean = true
  override def getBool () : Boolean = b
}


class VVector (val l:List[Value]) extends Value {

  override def toString () : String =
     return l.addString(new StringBuilder(), "[ ", " ", " ]").toString()

  override def isVector () : Boolean = true
  override def getList () : List[Value] = l
}


class VPrimOp (val oper : (List[Value]) => Value) extends Value {

  override def toString () : String = "primop(" + oper + ")"
  override def isFunction () : Boolean = true

  override def apply (args: List[Value]) : Value =
     oper(args)
}


class VRecClosure (val self: String, val params: List[String], val body:Exp, val env:Env) extends Value {

  override def toString () : String = params + " | " + self + " => " + body
  override def isFunction () : Boolean = true

  override def apply (args: List[Value]) : Value = {
     if (params.length != args.length) {
        throw new Exception("Runtime error : wrong number of arguments\n  Function "+this.toString())
     }
     var new_env = env
     for ((p,v) <- params.zip(args)) {
        new_env = new_env.push(p,v)
     }

     // push the current closure as the value bound to identifier self
     new_env = new_env.push(self,this)
     return body.eval(new_env)
  }
}


       /******************** QUESTION 3 *********************/
class VDictionary (val content: List[(Value, Value)], val env:Env) extends Value {

   override def toString () : String =
     "Dictionary(" + content + ")"

   override def isDictionary () : Boolean = true
}

class VSubDictionary(val content:List[(Value,Value)], val key:Value) extends Value {
   override def toString () : String =
     "Dictionary(" + content + ")"

   override def isDictionary () : Boolean = true
}




//
//  Primitive operations
//

object Ops {

   def runtimeError (msg: String) : Nothing = {

       throw new Exception("Runtime error: "+msg)
   }



   def checkArgsLength (vs:List[Value], min: Int, max : Int) : Unit = {

      //
      // check whether an argument list has size between min and max
      //

      if (vs.length < min) {
         runtimeError("Number of args < "+min)
      }
      if (vs.length > max) {
         runtimeError("Number of args > "+max)
      }
   }



   def operPlus (vs:List[Value]) : Value = {

      checkArgsLength(vs,2,2)

      val v1 = vs(0)
      val v2 = vs(1)

      var finalVector = new VVector(List())

      if (v1.isInteger() && v2.isInteger()) {
	 var result = new VInteger(v1.getInt() + v2.getInt())
	 println(v1 +" + " + v2 +" = " +result)
         return result
      } else if (v1.isVector() && v2.isVector()) {
          if (v1.getList().length == v2.getList().length) {
             var result : List[Value] = List()
             for ((entry1,entry2) <- v1.getList().zip(v2.getList())) {
                result = result :+ operPlus(List(entry1,entry2))
             }
	     finalVector = new VVector(result)
          } else {
             runtimeError("vectors of different length")
          }
      } else if (v1.isVector() && !(v2.isVector())) {
          finalVector = new VVector(v1.getList().map((v:Value) => operPlus(List(v,v2))))
      } else if (v2.isVector() && !(v1.isVector())) {
          finalVector = new VVector(v2.getList().map((v:Value) => operPlus(List(v1,v))))
      } else {
         runtimeError("cannot add values of different types\n  "+v1+"\n  "+v2)
      }

      println(v1 +" + " + v2 +" = " +finalVector)
      return finalVector
   }


   def operTimes (vs: List[Value]):Value = {

      checkArgsLength(vs,2,2)

      val v1 = vs(0)
      val v2 = vs(1)

      var finalVector = new VVector(List())

      if (v1.isInteger() && v2.isInteger()) {
         var result = new VInteger(v1.getInt() * v2.getInt())
	 println(v1 +" * " + v2 +" = " +result)
         return result
      } else if (v1.isVector() && v2.isVector()) {
        if (v1.getList().length == v2.getList().length) {
             var result : Value = new VInteger(0)
             for ((entry1,entry2) <- v1.getList().zip(v2.getList())) {
                result = operPlus(List(result, operTimes(List(entry1,entry2))))
             }
	     println(v1 +" * " + v2 +" = " +result)
             return result
         } else {
           runtimeError("vectors of different length")
         }
      } else if (v1.isVector() && !(v2.isVector())) {
          finalVector = new VVector(v1.getList().map((v:Value) => operTimes(List(v,v2))))
      } else if (v2.isVector() && !(v1.isVector())) {
          finalVector =  new VVector(v2.getList().map((v:Value) => operTimes(List(v1,v))))
      } else {
         runtimeError("cannot multiply values of different types")
      }

      return finalVector
   }


   def operMap (vs: List[Value]):Value = {

      checkArgsLength(vs,2,2)

      val vf = vs(0)
      val vv = vs(1)

      vf.checkFunction()
      vv.checkVector()

      val l = vv.getList()
      return new VVector(l.map((v:Value) => vf.apply(List(v))))
   }


   def operFilter (vs: List[Value]):Value = {

      checkArgsLength(vs,2,2)

      val vf = vs(0)
      val vv = vs(1)

      vf.checkFunction()
      vv.checkVector()

      def asBool (v:Value) : Boolean = {
        if (!v.isBoolean()) {
            runtimeError("filter predicate not returning a Boolean")
        }
        return v.getBool()
      }

      val l = vv.getList()
      return new VVector(l.filter((v:Value) => asBool(vf.apply(List(v)))))
   }


   def operEqual (vs: List[Value]) : Value = {

      checkArgsLength(vs,2,2)

      val v1 = vs(0)
      val v2 = vs(1)

      if (v1.isBoolean() && v2.isBoolean()) {
         return new VBoolean(v1.getBool() == v2.getBool())
      } else if (v1.isInteger() && v2.isInteger()) {
         return new VBoolean(v1.getInt() == v2.getInt())
      } else if (v1.isVector() && v2.isVector()) {
         if (v1.getList().length == v2.getList().length) {
            for ((vv1,vv2) <- v1.getList().zip(v2.getList())) {
   	    if (!operEqual(List(vv1,vv2)).getBool()) {
   	       return new VBoolean(false)
   	    }
   	 }
   	 return new VBoolean(true)
         } else {
            return new VBoolean(false)
         }
      } else if (v1.isFunction() && v2.isFunction()) {
         return new VBoolean(v1==v2)
      } else {
         return new VBoolean(false)
      }
   }


   def operLess (vs: List[Value]) : Value = {

       checkArgsLength(vs,2,2)

       val v1 = vs(0)
       val v2 = vs(1)
       v1.checkInteger()
       v2.checkInteger()

       return new VBoolean(v1.getBool() < v2.getBool())
   }


   def operVector (vs: List[Value]) : Value = {

      return new VVector(vs)
   }


   def operEmpty (vs : List[Value]) : Value = {

     checkArgsLength(vs,1,1)
     val v = vs(0)
     v.checkVector()
     return new VBoolean(v.getList().length == 0)
   }


   def operFirst (vs : List[Value]) : Value = {
     checkArgsLength(vs,1,1)
     val v = vs(0)
     v.checkVector()
     val l = v.getList()
     if (l.length == 0) {
       runtimeError("Taking first of an empty vector")
     }
     return l(0)
   }


   def operRest (vs : List[Value]) : Value = {
     checkArgsLength(vs,1,1)
     val v = vs(0)
     v.checkVector()
     val l = v.getList()
     if (l.length == 0) {
       runtimeError("Taking rest of an empty vector")
     }
     return new VVector(l.tail)
   }


   def operCons (vs : List[Value]) : Value = {
     checkArgsLength(vs,2,2)
     val item = vs(0)
     val vec = vs(1)
     vec.checkVector()
     return new VVector(item::vec.getList())
   }
}




//
//  Expressions
//


class Env (val content: List[(String, Value)]) {

      override def toString () : String = {
          var result = ""
	  for (entry <- content) {
	     result = result + "(" + entry._1 + " <- " + entry._2 + ") "
	  }
	  return result
      }


      // push a single binding (id,v) on top of the environment

      def push (id : String, v : Value) : Env =
          new Env((id,v)::content)


      // lookup value for an identifier in the environment

      def lookup (id : String) : Value = {
      	  for (entry <- content) {
	      if (entry._1 == id) {
	      	 return entry._2
	      }
	  }
	  throw new Exception("Environment error: unbound identifier "+id)
      }

     def getList () : List[(String,Value)] = {
         return content
     }
}



abstract class Exp {

    def eval (env : Env) : Value

    def error (msg : String) : Nothing = {
       throw new Exception("Eval error: "+ msg + "\n   in expression " + this)
    }
}

/**************** QUESTION 3 ****************/
class EString (val v:Value) extends Exp {
    // value string

    override def toString () : String =
        "EString(" + v + ")"

    def eval (env:Env) : Value =
        v
}

class ELiteral (val v:Value) extends Exp {
    // value literal

    override def toString () : String =
        "ELiteral(" + v + ")"

    def eval (env:Env) : Value =
        v
}



class EIf (val ec : Exp, val et : Exp, val ee : Exp) extends Exp {
    // Conditional expression

    override def toString () : String =
        "EIf(" + ec + "," + et + "," + ee +")"

    def eval (env:Env) : Value = {
        val ev = ec.eval(env)
	var result = et.eval(env)

	if (ev.isBoolean()) {
	  if (!ev.getBool()) {
  	    result =  ee.eval(env)
	  } else {
	    result = et.eval(env)
	  }
//CAUSES ISSUES WITH SUM:	  println("if ("+ ev.getBool() + ") then {" + et.eval(env) +"} else {" +ee.eval(env) +"} results in " +result)
          return result
	} else {
	  error("condition not a Boolean")
        }
    }
}


class EId (val id : String) extends Exp {

    override def toString () : String =
        "EId(" + id + ")"

    def eval (env : Env) : Value = {
        println(""+id +" = " + env.lookup(id))
    	return env.lookup(id)
    }

}


class EApply (val f: Exp, val args: List[Exp]) extends Exp {
   override def toString () : String =
      "EApply(" + f + "," + args + ")"

   def eval (env : Env) : Value = {
      val vf = f.eval(env)
      val vargs = args.map((e:Exp) => e.eval(env))

      println("function "+f + "(" + vargs + ")") //"fun(" + args + ") { " + f + "}"
      return vf.apply(vargs)
   }
}


class EFunction (val params : List[String], val body : Exp) extends Exp {

   override def toString () : String =
     "EFunction(" + params + "," + body + ")"

   def eval (env : Env) : Value = {
      println("fun(" +params +") {" +body +"}")
      return new VRecClosure("",params,body,env)
   }

}

class ERecFunction (val self: String, val params: List[String], val body : Exp) extends Exp {

   override def toString () : String =
     "ERecFunction(" + self + "," + params + "," + body + ")"

   def eval (env : Env) : Value = {
   //   println("fun(" +self +", " +params +") {" +body +"}")
      return new VRecClosure(self,params,body,env)
   }
}


  /*************************** QUESTION 3 ***********************/
class EDictionary (val params : List[(Exp, Exp)]) extends Exp {

   override def toString () : String =
     "EDictionary(" + params + ")"

   def eval (env : Env) : Value = {
      val keys = params.map((e) => e._1.eval(env))
      val values = params.map((e) => e._2.eval(env))
      var evaluatedParams = keys zip values

      new VDictionary(evaluatedParams, env)
   }
}



class ESubDictionary (val params : List[(Exp, Exp)], val id : Exp) extends Exp {

   override def toString () : String =
     "ESubDictionary(" + params + ", " + id +")"

   def eval (env : Env) : Value = {
      val keys = params.map((e) => e._1.eval(env))
      val values = params.map((e) => e._2.eval(env))
      var evaluatedParams = keys zip values
      val lookupVal = id.eval(env)

     // For every entry in the dictionary, see if the key matched
      for (entry <- evaluatedParams) {
	if (Ops.operEqual(List(lookupVal, entry._1)).getBool()) {
	    return entry._2
	 }
      }
      throw new Exception("Dictionary access error: unbound identifier " + id)
   }

}








//
// SURFACE SYNTAX (S-expressions)
//


import scala.util.parsing.combinator._


class SExpParser extends RegexParsers {

   // tokens

   def LP : Parser[Unit] = "(" ^^ { s => () }
   def RP : Parser[Unit] = ")" ^^ { s => () }
   def LB : Parser[Unit] = "[" ^^ { s => () }
   def RB : Parser[Unit] = "]" ^^ { s => () }
   def PLUS : Parser[Unit] = "+" ^^ { s => () }
   def TIMES : Parser[Unit] = "*" ^^ { s => () }
   def INT : Parser[Int] = """[0-9]+""".r ^^ { s => s.toInt }
   def IF : Parser[Unit] = "if" ^^ { s => () }
   def ID : Parser[String] = """[a-zA-Z_+*:.?=<>!|][a-zA-Z0-9_+*:.?=<>!|]*""".r ^^ { s => s }
   def AND : Parser[Unit] = "and" ^^ {s => ()}
   def OR : Parser[Unit] = "or" ^^ {s => ()}
   def LET : Parser[Unit] = "let" ^^ {s => ()}
   def COND : Parser[Unit] = "cond" ^^ {s => ()}

   def DEFINE : Parser[Unit] = "define" ^^ {s => ()}
   def DEFUN : Parser[Unit] = "defun" ^^ {s => ()}
   def QUIT : Parser[Unit] = "#quit" ^^ {s => ()}
   def ENV : Parser[Unit] = "#env" ^^ {s => ()}
   def PARSE : Parser[Unit] = "#parse" ^^ {s => ()}

   def FUN : Parser[Unit] = "fun" ^^ { s => () }
   def BAR : Parser[Unit] = "|" ^^ { s => () }
   def GETS : Parser[Unit] = "<-" ^^ { s => () }


   def DICT : Parser[Unit] = "dict" ^^ {s => ()}
   def SUB : Parser[Unit] = "sub" ^^ {s => ()}
   def STRING : Parser[String] = """[a-zA-Z0-9_+*:.?=<>!|]*""".r ^^ { s => s }


   // grammar

      /**************** QUESTION 1 ****************/
   def expr_and : Parser[Exp] =
      LP ~ AND ~ andList ~ RP ^^ {case _ ~ _ ~ list ~ _ => list }

   def andList : Parser[Exp] =
      (multipleArgs | twoArgs) ^^ {e => e}

   def multipleArgs : Parser[Exp] =
      expr ~ andList ^^ {case e1 ~ list => new EIf(e1, list, new ELiteral(new VBoolean(false))) }

   def twoArgs : Parser[Exp] =
      expr ~ expr ^^ {case e1 ~ e2 => new EIf(e1, e2, new ELiteral(new VBoolean(false)))}


   def expr_or : Parser[Exp] =
      LP ~ OR ~ orList ~ RP ^^ {case _ ~ _ ~ list ~ _ => list }

   def orList : Parser[Exp] =
      (multipleArgsOr | twoArgsOr) ^^ {e => e}

   def multipleArgsOr : Parser[Exp] =
      expr ~ orList ^^ {case e1 ~ list => new EIf(e1, new ELiteral(new VBoolean(true)), list)}

   def twoArgsOr : Parser[Exp] =
      expr ~ expr ^^ {case e1 ~ e2 =>  new EIf(e1,new ELiteral(new VBoolean(true)),e2)}



   def expr_let : Parser[Exp] =
      LP ~ LET ~ LP ~ rep(binding) ~ RP ~ expr ~ RP ^^ {case _ ~ _ ~ _ ~ bindings ~ _ ~ e ~ _ =>
      	       	      		      	   	     	     new EApply(new EFunction(bindings.unzip._1, e), bindings.unzip._2)}

   def binding : Parser [(String, Exp)] =
      LP ~ ID ~ expr ~ RP ^^ { case _ ~ id ~ e ~ _ => (id,e) }



   def expr_cond : Parser[Exp] =
      LP ~ COND ~ condList ~ RP ^^ {case  _ ~ _ ~ list ~ _  => list }

   def condList : Parser[Exp] =
     (multipleConds | oneCond) ^^ {e => e}

   def multipleConds : Parser[Exp] =
      LP ~ expr ~ expr ~ RP ~ condList ^^ {case _ ~ e1 ~ e2 ~ _ ~ list => new EIf(e1, e2, list)}

   def oneCond : Parser[Exp] =
      LP ~ expr ~ expr ~ RP ^^ {case _ ~ e1 ~ e2 ~ _=> e2}




   def atomic_int : Parser[Exp] = INT ^^ { i => new ELiteral(new VInteger(i)) }

   def atomic_id : Parser[Exp] =
      ID ^^ { s => new EId(s) }

   def atomic_string : Parser[Exp] = STRING ^^ { s => new ELiteral(new VString(s)) }

   def atomic : Parser[Exp] =
      (atomic_int | atomic_id) ^^ { e => e}

   def expr_if : Parser[Exp] =
      LP ~ IF ~ expr ~ expr ~ expr ~ RP ^^
        { case _ ~ _ ~ e1 ~ e2 ~ e3 ~ _ => new EIf(e1,e2,e3) }

   def expr_map : Parser[Exp] =
      LB ~ expr ~ BAR ~ ID ~ GETS ~ expr ~ RB ^^
           { case _ ~ e1 ~ _ ~ id ~ _ ~ e2 ~ _ =>
	        new EApply(new ELiteral(new VPrimOp(Ops.operMap)),List(new EFunction(List(id),e1), e2)) }

   def expr_mapfilter : Parser[Exp] =
      LB ~ expr ~ BAR ~ ID ~ GETS ~ expr ~ BAR ~ expr ~ RB ^^
           { case _ ~ e1 ~ _ ~ id ~ _ ~ e2 ~ _ ~ e3 ~ _ =>
	        new EApply(new ELiteral(new VPrimOp(Ops.operMap)),
		           List(new EFunction(List(id),e1),
                                new EApply(new ELiteral(new VPrimOp(Ops.operFilter)),
				           List(new EFunction(List(id),e3),
					        e2)))) }

   def expr_vec : Parser[Exp] =
      LB ~ rep(expr) ~ RB ^^ { case _ ~ es ~ _ => new EApply(new ELiteral(new VPrimOp(Ops.operVector)),es) }

   def expr_fun : Parser[Exp] =
      LP ~ FUN ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^
        { case _ ~ _ ~ _ ~ params ~ _ ~ e ~ _ => new EFunction(params,e) }

   def expr_funr : Parser[Exp] =
      LP ~ FUN ~ ID ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^
        { case _ ~ _ ~ self ~ _ ~ params ~ _ ~ e ~ _ => new ERecFunction(self,params,e) }

   def expr_app : Parser[Exp] =
      LP ~ expr ~ rep(expr) ~ RP ^^ { case _ ~ ef ~ eargs ~ _ => new EApply(ef,eargs) }

   def expr : Parser[Exp] =
      ( atomic | accessDictionary | dictionary | expr_if | expr_cond | expr_and | expr_or | expr_let | expr_map | expr_mapfilter | expr_vec | expr_fun | expr_funr | expr_app) ^^
           { e => e }


   /********************** QUESTION 2 ************************/
   def definition : Parser[(String, Exp)] =
      (convenientFunction | regularDefinition) ^^ {e => e}

   def regularDefinition : Parser[(String,Exp)] =
      LP ~ DEFINE ~ ID ~ expr ~ RP ^^ {case _ ~ _ ~ id ~ e1 ~ _ => (id, e1)}

   def convenientFunction : Parser [(String,Exp)] =
      LP ~ DEFUN ~ ID ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^ {case _ ~ _ ~ name ~ _ ~ list ~ _ ~ body ~ _ => (name, new ERecFunction(name, list, body))}

   def shell_entry : Parser[ShellEntry] =
      (shell_parse | shell_quit | shell_env | shell_def | shell_expr) ^^ {e => e}

   def shell_expr : Parser[ShellEntry] =
      expr ^^ { e => new SEexpr(e) }

   def shell_def : Parser[ShellEntry] =
      definition ^^ { e => new SEdefine(e) }

   def shell_quit : Parser[ShellEntry] =
      QUIT ^^ {case _ => new SEquit()}

   def shell_env : Parser[ShellEntry] =
      ENV ^^ {case _ => new SEenv()}

   def shell_parse : Parser[ShellEntry] =
      PARSE ~ expr ^^ {case _ ~ e => new SEparse(e)}


   /********************** QUESTION 3 ************************/
    def dictionary : Parser[Exp] =
      LP ~ DICT ~ rep(dicBinding) ~ RP ^^ {case _ ~ _ ~ values ~ _ => new EDictionary(values)}

   def accessDictionary : Parser[Exp] =
      LP ~ SUB ~  LP ~ DICT ~ rep(dicBinding) ~ RP ~ expr ~ RP ^^ {case _ ~ _ ~ _ ~ _ ~ values ~ _ ~ key ~ _ => new ESubDictionary(values, key)}

    def dicBinding : Parser [(Exp, Exp)] =
      LP ~ expr ~ expr ~ RP ^^ { case _ ~ e1 ~ e2 ~ _ => (e1,e2) }

}



//
//  Shell
//

abstract class ShellEntry {

   // abstract class for shell entries
   // (representing the various entries you
   //  can type at the shell)

   def processEntry (env:Env) : Env
}

class SEexpr (e:Exp) extends ShellEntry {

   def processEntry (env:Env) : Env = {
      /************* PRINTING EXPRESSION HERE *****************/
  //    println("Expression: " +e)
      // Call a stepper function here, updating a counter for # times stepped?, or have use a built-in pause if it exists


      /************* PRINTING ENVIRONMENT HERE *****************/
  /*    print("Environment: ")
      val list = env.getList()
      for ((id, value) <- list ) {
        println(id + " = " + value)
      }
      // For each id in the expression, lookup value in the env, print it
*/
      /************* PRINTING FINAL VALUE HERE *****************/
      val v = e.eval(env)
      println("Evaluates to: " +v)

      return env
   }
}

class SEdefine (val content: (String,Exp)) extends ShellEntry {

   def processEntry (env:Env) : Env = {
      val v = content._2.eval(env)
      val id = content._1
      println(id +" defined")
      return env.push(id, v)
   }
}

class SEdictionary (val content: EDictionary) extends ShellEntry {

   def processEntry (env:Env) : Env = {
      return env
   }
}

class SEquit extends ShellEntry {
   def processEntry (env:Env) : Env = {
     println("Good Bye")
     sys.exit(1)
   }
}

class SEenv extends ShellEntry  {
   def processEntry (env:Env) : Env = {
      val list = env.getList()

    for ((id, value) <- list ) {
      println(id + " = " + value)
    }

      return env
   }
}

class SEparse (val e : Exp) extends ShellEntry {

   def processEntry (env:Env) : Env = {
      println(e)
      return env
   }
}


object Shell {
   def main (argv:Array[String]) : Unit = {
      // shell()

      /************** GUI **************/
      val ui = new UI
      ui.visible = true
      println("End of main function")
   }

}

/*************** GUI ****************/
class UI extends MainFrame {
  title = "GridBagPanel"
  preferredSize = new Dimension(1000, 800)

  var inDebugMode = false
  val toggle = new ToggleButton("Debug Mode")
  val textField = new TextField { columns = 32 }
  val display = new TextArea
  val envdisplay = new ListView{
      background = Color.lightGray
  }

  display.editable_=(false)
  val runButton = new Button("Run")



  contents = new GridBagPanel {
    def constraints(x: Int, y: Int,
		    gridwidth: Int = 1, gridheight: Int = 1,
		    weightx: Double = 0.0, weighty: Double = 0.0,
		    fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None)
    : Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c
    }

    add(runButton,
	constraints(0, 0, gridheight=2, fill=GridBagPanel.Fill.Both))

    // Toggle Button for debug on/off
    add(toggle, constraints(2, 0))

    // Text line for input
    add(textField,constraints(1, 0, weightx=1.0, fill=GridBagPanel.Fill.Horizontal))

    // Area to display information
    add(display,constraints(1, 1, gridheight=2, weighty = 1.0,
		    fill=GridBagPanel.Fill.Both))

    // Area to display envrionment
    add(envdisplay, constraints(1, 3, gridheight=1, weighty = 1.0,
        fill=GridBagPanel.Fill.Both))

    // Close the window
    add(Button("Close") { sys.exit(0) },
	constraints(0, 4, gridwidth=3, fill=GridBagPanel.Fill.Horizontal))
  }



    listenTo(toggle)
    listenTo(runButton)

    // react to events
    reactions += {
      // IF DEBUG BUTTON WAS PRESSED
      case ButtonClicked(component) if component == toggle =>
        if (toggle.selected){
	  display.text = "**** ENTERED DEBUG MODE ****"
	  inDebugMode = true
        }
	else{
	  display.text = "**** EXITED DEBUG MODE ****"
	  inDebugMode = false
        }

      // IF RUNNING THE CODE
      case ButtonClicked(component) if component == runButton =>
        display.text = textField.text
	shell()

	// Still need to clear the text after button is pressed

    }



   /*************************** UPDATES TO THE ORIGINAL SHELL ******************************/
   val parser = new SExpParser

   def parse (input:String) : ShellEntry = {

      parser.parseAll(parser.shell_entry, input) match {
         case parser.Success(result,_) => result
         case failure : parser.NoSuccess => throw new Exception("Cannot parse "+input+": "+failure.msg)
      }
   }

   val nullEnv = new Env(List())

   //
   // Standard environment
   //

   val stdEnv = new Env(List(
     ("true",new VBoolean(true)),
     ("false",new VBoolean(false)),
     ("not", new VRecClosure("",List("a"), new EIf(new EId("a"), new ELiteral(new VBoolean(false)), new ELiteral(new VBoolean(true))),nullEnv)),
     ("+", new VPrimOp(Ops.operPlus)),
     ("*", new VPrimOp(Ops.operTimes)),
     ("=", new VPrimOp(Ops.operEqual)),
     ("<", new VPrimOp(Ops.operLess)),
     ("map", new VPrimOp(Ops.operMap)),
     ("filter", new VPrimOp(Ops.operFilter)),
     ("empty?",new VPrimOp(Ops.operEmpty)),
     ("first",new VPrimOp(Ops.operFirst)),
     ("rest",new VPrimOp(Ops.operRest)),
     ("empty",new VVector(List())),
     ("cons",new VPrimOp(Ops.operCons))
   ))


   def shell () : Unit = {

       var env = stdEnv

       print("FUNC> ")
       try {
         val input = textField.text

         val se = parse(input)
	 env = se.processEntry(env)
       } catch {
         case e : Exception => println(e.getMessage)
       }
   }

}
