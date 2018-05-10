/************* GUI *************/
import scala.swing._
import scala.swing.event._

/********** TEXT FILE *********/
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import java.io.OutputStream
import java.io.{BufferedOutputStream, FileOutputStream}
import java.awt.{Color, Graphics2D}


//
// language FUNC with (simple) static types
//
// tail-optimized evaluation (via exp.evalTail)
//
// 


//
//  Values
//


abstract class Value {

   def getInt () : Int = 0
   def getBool () : Boolean = false
   def getList () : List[Value] = List()

   // we still need to distinguish primitive operations from functions :/
   def isPrimOp () : Boolean = false
   def applyOper (args:List[Value]) : Value = new VInteger(0)

   def getParams () : List[String] = List()
   def getSelf () : String = ""
   def getBody () : Exp = new EInteger(0)
   def getEnv () : Env[Value] = new Env(List())
}


class VInteger (val i:Int) extends Value {

  override def toString () : String = i.toString()
  override def getInt () : Int = i
}


class VBoolean (val b:Boolean) extends Value {

  override def toString () : String = b.toString()
  override def getBool () : Boolean = b
}


class VVector (val l:List[Value]) extends Value {

  override def toString () : String = 
     return l.addString(new StringBuilder(), "[ ", " ", " ]").toString()

  override def getList () : List[Value] = l
}


class VPrimOp (val oper : (List[Value]) => Value) extends Value {

  override def toString () : String = "primop(" + oper + ")"

  override def isPrimOp () : Boolean = true
  override def applyOper (args:List[Value]) : Value = oper(args)
}


class VRecClosure (val self: String, val params: List[String], val body:Exp, val env:Env[Value]) extends Value {

  override def toString () : String = params + " | " + self + " => " + body 

  override def getParams () : List[String] = params
  override def getSelf () : String = self
  override def getBody () : Exp = body
  override def getEnv () : Env[Value] = env
}





//
//  Primitive operations
//

object Ops { 

   def runtimeError (msg: String) : Nothing = {
       throw new Exception("Runtime error: "+msg)
   }

   def operPlus (vs:List[Value]) : Value = {
   
      val v1 = vs(0)
      val v2 = vs(1)
      
      return new VInteger(v1.getInt() + v2.getInt())
   }
   
   
   def operTimes (vs: List[Value]):Value = {
   
      val v1 = vs(0)
      val v2 = vs(1)
      
      return new VInteger(v1.getInt() * v2.getInt())
   }
   
   
   def operEqual (vs: List[Value]) : Value = {
       
      val v1 = vs(0)
      val v2 = vs(1)
   
      return new VBoolean(v1.getInt() == v2.getInt())
   }
   
   
   def operLess (vs: List[Value]) : Value = {
   
       val v1 = vs(0)
       val v2 = vs(1)
   
       return new VBoolean(v1.getBool() < v2.getBool())
   }
   
   
   def operEmpty (vs : List[Value]) : Value = {
   
     val v = vs(0)
     return new VBoolean(v.getList().length == 0)
   }
   
   
   def operFirst (vs : List[Value]) : Value = {
     val v = vs(0)
     val l = v.getList()
     if (l.length == 0) {
       runtimeError("Taking first of an empty vector")
     }
     return l(0)
   }
   
   
   def operRest (vs : List[Value]) : Value = {
     val v = vs(0)
     val l = v.getList()
     if (l.length == 0) {
       runtimeError("Taking rest of an empty vector")
     }
     return new VVector(l.tail)
   }
   
   
   def operCons (vs : List[Value]) : Value = {
     val item = vs(0)
     val vec = vs(1)
     return new VVector(item::vec.getList())
   }

}




//
//  Types
//


abstract class Type {

   def isSame (t:Type) : Boolean
   
   def isInteger () : Boolean = return false
   def isBoolean () : Boolean = return false
   def isIntVector () : Boolean = return false
   def isFunction () : Boolean = return false

   def funParams () : List[Type] = {
      throw new Exception("Type error: type is not a function\n   "+this)
   }

   def funResult () : Type = {
      throw new Exception("Type error: type is not a function\n   "+this)
   }
      
}


object TInteger extends Type {

   override def toString () : String = "int"
   
   def isSame (t:Type):Boolean = return t.isInteger()
   override def isInteger () : Boolean = true
}

object TBoolean extends Type {

   override def toString () : String = "bool"
   
   def isSame (t:Type):Boolean = return t.isBoolean()
   override def isBoolean () : Boolean = true
}

object TIntVector extends Type {

   override def toString () : String = "ivector"

   def isSame (t:Type):Boolean = return t.isIntVector()
   override def isIntVector () : Boolean = true
}

class TFunction (val params:List[Type], val result:Type) extends Type {

   override def toString () : String =
     "(fun "+params.addString(new StringBuilder(),"("," ",")").toString() + " " + result + ")"
   
   def isSame (t:Type):Boolean = {
   
     if (!t.isFunction()) {
        return false
     }

     if (t.funParams().length != params.length) {
        return false
     }
     for ((t1,t2) <- t.funParams().zip(params)) {
        if (!t1.isSame(t2)) {
	   return false
	}
     }
     return t.funResult().isSame(result)
   }
   
   override def isFunction () : Boolean = return true
   override def funParams () : List[Type] = return params
   override def funResult () : Type = return result
}


//
//  Expressions
//


class Env[A] (val content: List[(String, A)]) { 

      override def toString () : String = {
          var result = ""
	  for (entry <- content) {
	     result = result + "(" + entry._1 + " <- " + entry._2 + ") "
	  }
	  return result
      }

      
      def push (id : String, v : A) : Env[A] =
          // push a single binding (id,v) on top of the environment

          new Env[A]((id,v)::content)


      def lookup (id : String) : A = {
            // lookup value for an identifier in the environment
      
      	  for (entry <- content) {
	      if (entry._1 == id) {
	      	 return entry._2
	      }
	  }
	  throw new Exception("Environment error: unbound identifier "+id)
      }

     def getList () : List[(String,A)] = {
         return content
     }
}



abstract class Exp {

    def error (msg : String) : Nothing = { 
       throw new Exception("Eval error: "+ msg + "\n   in expression " + this)
    }

    def terror (msg : String) : Nothing = { 
       throw new Exception("Type error: "+ msg + "\n   in expression " + this)
    }

    def typeOf (symt:Env[Type]) : Type
    
    def typeCheck (t:Type, symt:Env[Type]) : Boolean = {
    	val t2 = this.typeOf(symt)
	return t.isSame(t2)
    }

    def eval (env : Env[Value], isDebug : Boolean, into : Boolean) : Value = {
      var currExp = this
      
      currExp match {
	 case EInteger(i) => {
	     new VInteger(i)
	 }
	 case EBoolean(b) => {
	     new VBoolean(b)
	 }
	 case EVector(es) => {
	     val vs = es.map((e:Exp) => e.eval(env, isDebug, into))
	     
             new VVector(vs)
	 }
	 case EId(id) => {
	     val lookup = env.lookup(id)
	     if(!lookup.isPrimOp()){
	       val writer = new PrintWriter(new FileOutputStream(new File("steps.txt"),true))
	       writer.println(id +" = " +lookup)
	       writer.close()
	     }

             return lookup
	 }
	 case ERecFunction(self, params, typ, body) => {
	     new VRecClosure(self, params, body, env)
	 }
	 case EBreakpoint(e) => {
	    if(isDebug) {
	     // new file for environment bp @ exp: env = ....
	      println("~ BREAKPOINT ~")
	      
	      val writer = new PrintWriter(new FileOutputStream(new File("steps.txt"),true))
	      writer.println("~ BREAKPOINT ~")
	      writer.close()

              // Print environment
	      val envWriter = new PrintWriter(new FileOutputStream(new File("env.txt"),false))
	      val list = env.getList()

              for ((id, value) <- list ) {
                envWriter.println(id + " = " + value)
              }
	      envWriter.close()
	      
	    }
	    e.evalTail(env, isDebug, into)
	 }
       }
     }
   


    def evalTail (env : Env[Value], isDebug : Boolean, into : Boolean) : Value = {
    //  var file = Source.fromFile("steps.txt")
      var currExp = this
      var currEnv = env

      while(true) {
         currExp match {
	    case EIf(ec,et,ee) => {
	      if(isDebug){
	        // Print the original statement
		val expString = (new EIf(ec,et,ee)).toString()
	        println(expString)
        
                // FileOutputStream and "true" used to append (not erase old info)
		val writer = new PrintWriter(new FileOutputStream(new File("steps.txt"),true))
		writer.println(expString)
		writer.println("Value: " +(new EIf(ec,et,ee)).evalTail(env, false, into))
		writer.close()
		
		// Evaluate the conditional
		val writer2 = new PrintWriter(new FileOutputStream(new File("steps.txt"),true))
		
		val ifStr = "if " +ec.evalTail(currEnv, isDebug, into).getBool() +" then { " +et +" } else { " +ee +" }"
		println(ifStr)
                writer2.println(ifStr)

	        writer2.close()
	      }
             
              val ev = ec.evalTail(currEnv, false, into)
	      
              if (!ev.getBool()) {
                currExp = ee
              } else {
                currExp = et
              } 
	    }

	    case EVector(es) => {
               val vs = es.map((e:Exp) => {e.evalTail(currEnv, isDebug, into)})
               return new VVector(vs)
	    }

	    case EApply(f,args) => {
               if(isDebug){
		  val exprStr = (new EApply(f,args)).toString()
		  println(exprStr)
		  
	       	  val writer = new PrintWriter(new FileOutputStream(new File("steps.txt"),true))
		  writer.println(exprStr)
		  writer.println("Value: " +(new EApply(f, args)).evalTail(env, false, into))
		  writer.close()
	       }
	       
	       val vf = f.evalTail(currEnv,isDebug, into)
               val vargs = args.map((e:Exp) => e.evalTail(currEnv, isDebug, into))
	       	 
               if (vf.isPrimOp()) {
	         val returnVal = vf.applyOper(vargs)
		 println("Value: "+returnVal)
		 
                 return returnVal
               } else {
                 // defined function
                 // push the vf closure as the value bound to identifier self
                 var new_env = vf.getEnv().push(vf.getSelf(),vf)
                 for ((p,v) <- vf.getParams().zip(vargs)) {
                    new_env = new_env.push(p,v)
                 }
                 currEnv = new_env
                 currExp = vf.getBody()
               }
	     }
	     
	     case ELet(bindings,body) => {
                var new_env = currEnv
		
 		if(isDebug){
		  val thisString = (new ELet(bindings,body)).toString()
		  println(thisString)
		  
		  val writer = new PrintWriter(new FileOutputStream(new File("steps.txt"),true))
		  writer.println(thisString)
		  writer.println("Value: " +(new ELet(bindings,body)).evalTail(env,false,into))
		  writer.close()
		}

                for ((n,e) <- bindings) {
                  val v = e.evalTail(currEnv, isDebug, into)
                  new_env = new_env.push(n,v)
                }
		
		currEnv = new_env
		currExp = body
             }
	     // every other expression type evaluates normally
	     case _ => {
	        return currExp.eval(currEnv, isDebug, into)
             }
           }
        }
	return new VInteger(0) // needed for typechecking
  }
   
}



case class EInteger (val i:Integer) extends Exp {
    // integer literal

    override def toString () : String = 
        i.toString()

    def typeOf (symt:Env[Type]) : Type = 
        TInteger
}


case class EBoolean (val b:Boolean) extends Exp {
    // boolean literal

    override def toString () : String = 
        b.toString()

    def typeOf (symt:Env[Type]) : Type = 
        TBoolean
}


case class EVector (val es: List[Exp]) extends Exp {
    // Vectors

   override def toString () : String =
      es.addString(new StringBuilder(),"[", " ", "]").toString()
      
    def typeOf (symt:Env[Type]) : Type = {
       for (e <- es) {
         if (!e.typeCheck(TInteger,symt)) {
	   terror("Vector component not an integer")
	 }
       }
       return TIntVector
    }
}


case class EIf (val ec : Exp, val et : Exp, val ee : Exp) extends Exp {
    // Conditional expression

    override def toString () : String = {
        "if " +ec +" then { " +et +" } else { " +ee +" }"	
    }
	
    def typeOf (symt:Env[Type]) : Type = {
      if (ec.typeCheck(TBoolean,symt)) {
        val t = et.typeOf(symt)
	if (ee.typeCheck(t,symt)) {
	  return t
	} else {
	  terror("Branches of conditional have different types")
	}
      } else {
        terror("Condition should be Boolean")
      }
    }
}


case class EId (val id : String) extends Exp {

    override def toString () : String =
       id

    def typeOf (symt:Env[Type]) : Type =  symt.lookup(id)
}




case class EApply (val f: Exp, val args: List[Exp]) extends Exp {
   override def toString () : String = {
       var str =  args mkString " "
       "(" +f + " " +str + ")"
   }
      
      
    def typeOf (symt:Env[Type]) : Type = {
      val t = f.typeOf(symt)
      if (t.isFunction()) {
        val params = t.funParams()
        if (params.length != args.length) {
	   terror("Wrong number of arguments")
	} else {
	   // check the argument types
	   for ((pt,a) <- params.zip(args)) {
	     if (!a.typeCheck(pt,symt)) {
	        terror("Argument "+a+" not of expected type")
	     }
	   }
	   return t.funResult()
	}
      } else {
        terror("Applied expression not of function type")
      }
   }   
}


case class ERecFunction (val self: String, val params: List[String], val typ: Type, val body : Exp) extends Exp {

   override def toString () : String = {
     var str =  params mkString ", "
     "recFunction(" + self + ", " + str + ") { " + body + "}"
   }
     
   def typeOf (symt:Env[Type]) : Type = {
      if (!typ.isFunction()) {
        terror("Function not defined with function type")
      }
      var tparams = typ.funParams()
      var tresult = typ.funResult()
      if (params.length != tparams.length) {
        terror("Wrong number of types supplied")
      }
      var new_symt = symt
      for ((p,pt) <- params.zip(tparams)) {
        new_symt = new_symt.push(p,pt)
      }
      // assume self has the declared function type
      new_symt = new_symt.push(self,typ)
      if (body.typeCheck(tresult,new_symt)) {
        return typ
      } else {
        terror("Return type of function not same as declared")
      }
    }      
}


case class ELet (val bindings : List[(String,Exp)], val ebody : Exp) extends Exp {

    override def toString () : String = {
       // List to store the better format
       var string = ""
       
       // Change the comma to an equals sign
       for ((n,e) <- bindings){
         string = string +"(" + (n.toString()) + " = " +e.toString() +") "
       }
       
      "let [ " +string +"] in " +ebody +""
   }

   def typeOf (symt:Env[Type]) : Type = {
       var new_symt = symt
       for ((n,e) <- bindings) {
         val t = e.typeOf(symt)
	 new_symt = new_symt.push(n,t)
       }
       return ebody.typeOf(new_symt)
    }

}

/*********************** BREAKPOINT EXPRESSION *************************/
case class EBreakpoint (val e : Exp) extends Exp {
    override def toString () : String =
        "bp(" + e + ")"

    def typeOf (symt:Env[Type]) : Type = {
       return e.typeOf(symt)
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
   def INT : Parser[Int] = """-?[0-9]+""".r ^^ { s => s.toInt }
   def IF : Parser[Unit] = "if" ^^ { s => () }
   def ID : Parser[String] = """[a-zA-Z_+*\-:.?=<>!|][a-zA-Z0-9_+\-*:.?=<>!|]*""".r ^^ { s => s }

   def FUN : Parser[Unit] = "fun" ^^ { s => () }
   def LET : Parser[Unit] = "let" ^^ { s => () }

   def TINT : Parser[Unit] = "int" ^^ { s => () } 
   def TBOOL : Parser[Unit] = "bool" ^^ { s => () }
   def TINTV : Parser[Unit] = "ivector" ^^ { s => () }
   def TFUN : Parser[Unit] = "tfun" ^^ { s => () }
   def BREAKPOINT : Parser[Unit] = "bp" ^^ {s => ()}
   
   // grammar

   def atomic_int : Parser[Exp] = INT ^^ { i => new EInteger(i) }

   def atomic_id : Parser[Exp] =
      ID ^^ { s => new EId(s) }

   def atomic : Parser[Exp] =
      ( atomic_int | atomic_id ) ^^ { e => e}
      
   def expr_if : Parser[Exp] =
      LP ~ IF ~ expr ~ expr ~ expr ~ RP ^^
        { case _ ~ _ ~ e1 ~ e2 ~ e3 ~ _ => new EIf(e1,e2,e3) }

   def binding : Parser[(String,Exp)] =
      LP ~ ID ~ expr ~ RP ^^ { case _ ~ n ~ e ~ _ => (n,e) }
      
   def expr_let : Parser[Exp] =
      LP ~ LET ~ LP ~ rep(binding) ~ RP ~ expr ~ RP ^^
           { case _ ~ _ ~ _ ~ bindings ~ _ ~ e2 ~ _ => new ELet(bindings,e2) }

   def expr_vec : Parser[Exp] =
      LB ~ rep(expr) ~ RB ^^ { case _ ~ es ~ _ => new EVector(es) }

   def expr_fun : Parser[Exp] =
      LP ~ FUN ~ LP ~ rep(ID) ~ RP ~ typ ~ expr ~ RP ^^
        { case _ ~ _ ~ _ ~ params ~ _ ~ typ ~ e ~ _ => new ERecFunction("",params,typ,e) }

   def expr_funr : Parser[Exp] =
      LP ~ FUN ~ ID ~ LP ~ rep(ID) ~ RP ~ typ ~ expr ~ RP ^^
        { case _ ~ _ ~ self ~ _ ~ params ~ _ ~ typ ~ e ~ _ => new ERecFunction(self,params,typ,e) }

   def expr_app : Parser[Exp] =
      LP ~ expr ~ rep(expr) ~ RP ^^ { case _ ~ ef ~ eargs ~ _ => new EApply(ef,eargs) }

   def expr_breakpoint : Parser[Exp] =
      LP ~ BREAKPOINT ~ expr ~ RP ^^ { case _ ~ _ ~ e ~ _ => new EBreakpoint(e)}



   def expr : Parser[Exp] =
      ( atomic | expr_breakpoint | expr_if | expr_vec | expr_fun | expr_funr | expr_let | expr_app) ^^
           { e => e }

   def typ_int : Parser[Type] =
      TINT ^^ { _ => TInteger }
      
   def typ_bool : Parser[Type] =
      TBOOL ^^ { _ => TBoolean }

   def typ_intvec : Parser[Type] =
      TINTV ^^ { _ => TIntVector }

   def typ_fun : Parser[Type] =
      LP ~ TFUN ~ LP ~ rep(typ) ~ RP ~ typ ~ RP ^^
         { case _ ~ _ ~ _ ~ tparams ~ _ ~ tresult ~ _ => new TFunction(tparams,tresult) }

   def typ : Parser[Type] =
      ( typ_int | typ_bool | typ_intvec | typ_fun ) ^^ { e => e }

   def shell_entry : Parser[ShellEntry] =
      (LP ~ "define" ~ ID ~ expr ~ RP  ^^ { case _ ~ _ ~ n ~ e ~ _  => new SEdefine(n,e) }) |
      (expr ^^ { e => new SEexpr(e) }) |
      ("#quit" ^^ { s => new SEquit() })
      
}



//
//  Shell 
//

abstract class ShellEntry {

   // abstract class for shell entries
   // (representing the various entries you
   //  can type at the shell)

   def processEntry (env:Env[Value],symt:Env[Type],isDebug:Boolean, into : Boolean) : (Env[Value],Env[Type])
}


class SEexpr (e:Exp) extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type],isDebug:Boolean, into : Boolean) : (Env[Value],Env[Type]) = {
      val t = e.typeOf(symt)
      // uses tail-optimized form of evaluation!
      val v = e.evalTail(env,isDebug, into)
      println(v+" : "+t)
      
      // Add result to the end of the file
      val writer = new PrintWriter(new FileOutputStream(new File("steps.txt"),true))
      writer.println("RESULT = " +v.toString())
      writer.println("END")
      writer.close()
      
      return (env,symt)
   }
}

class SEdefine (n:String, e:Exp) extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type],isDebug:Boolean, into : Boolean) : (Env[Value],Env[Type]) = {
      val t = e.typeOf(symt)
      val v = e.eval(env, isDebug, into)
      println(n + " defined with type " + t)
      
      val writer = new PrintWriter(new FileOutputStream(new File("steps.txt"),true))
      writer.println("" +n + " defined with type " + t)
      writer.close()
      
      return (env.push(n,v),symt.push(n,t))
   }

}

class SEquit extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type], isDebug:Boolean, into : Boolean) : (Env[Value],Env[Type]) = {
      System.exit(0)
      return (env,symt)
   }
}


// MAIN FUNCTION TO RUN SHELL
object Shell {
   def main (argv:Array[String]) : Unit = {

      /************** GUI **************/
      val ui = new UI
      ui.visible = true
   }
}



/*************** GUI ****************/
class UI extends MainFrame {
  title = "GridBagPanel"
  preferredSize = new Dimension(700, 400)

  var inDebugMode = false
  var into = false
  var counter = 0
  var numLines = 0
  val toggle = new ToggleButton("Debug Mode")
  val textField = new TextField { columns = 32 }
  val display = new TextArea
  display.editable_=(false)

  val runButton = new Button("Run")
  val stepInto = new Button("Step Into")
//  val stepOver = new Button("Step Over")
  val breakPoint = new Button("Next Breakpoint")

  var hitBP = false
  var continue = true

  val envdisplay = new TextArea{
      background = Color.lightGray
  }
  display.editable_=(false)


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
    
    // Step into vs over buttons
    add(stepInto, constraints(2, 1, gridheight=2, fill=GridBagPanel.Fill.Both))
  //  add(stepOver, constraints(3, 1, gridheight=2, fill=GridBagPanel.Fill.Both))
    add(breakPoint, constraints(3, 1, gridheight=2, fill=GridBagPanel.Fill.Both))

    
    // Text line for input
    add(textField,constraints(1, 0, weightx=1.0, fill=GridBagPanel.Fill.Horizontal))

    // Area to display information
    add(display,constraints(1, 1, gridheight=3, weighty = 1.0, 
		    fill=GridBagPanel.Fill.Both))

    // Close the window
    add(Button("Close") { sys.exit(0) }, 
	constraints(0, 4, gridwidth=3, fill=GridBagPanel.Fill.Horizontal))

    // Area to display envrionment
    add(envdisplay, constraints(2, 3, gridheight=1, gridwidth = 3, weighty = 1.0,
                    fill=GridBagPanel.Fill.Both))
  }

    listenTo(toggle)
    listenTo(runButton)

    // react to events
    reactions += {
      // IF DEBUG BUTTON WAS PRESSED
      case ButtonClicked(component) if component == toggle =>
        if (toggle.selected){
	  display.text = "**** ENTERED DEBUG MODE ****\n"
	  inDebugMode = true
	  
	  // Only listen to step buttons when in debug mode
	  listenTo(stepInto)
//	  listenTo(stepOver)
	  listenTo(breakPoint)
        }
	else{
	  display.text = "**** EXITED DEBUG MODE ****\n"
	  inDebugMode = false
	  
	  // Step buttons should not work outside debug mode
	  deafTo(stepInto)
//	  deafTo(stepOver)
	  deafTo(breakPoint)
        }

      // IF STEPPING INTO
       case ButtonClicked(component) if component == stepInto =>
	 into = true

         val currentFile = Source.fromFile("steps.txt").getLines()
	 val currentLine = currentFile drop(counter) next()

         if(counter < (numLines-1)){
           display.append(currentLine)
	   display.append("\n")
	   counter = counter + 1
	 }
	 else{
	   counter = 0
	   numLines = 0
	 }
	 

 /*     // IF STEPPING OVER
       case ButtonClicked(component) if component == stepOver =>
         display.text = "STEP OVER!"
*/
      // IF BREAKPOINT
       case ButtonClicked(component) if component == breakPoint =>
	 val currentFile = Source.fromFile("steps.txt").getLines()
	 var finalCounter = -1

         // When the line is not equal to the breakpoint yet
	 for(line <- currentFile){
	   if (!(line == "~ BREAKPOINT ~")){
	     // Keeps track of how many lines we've seen
             counter = counter + 1
	   }
	   else{
	     // Keep track of the line number where the breakpoint was hit
	     finalCounter = counter
	   }
	 }
	    
         // If you reached the end of the file without hitting a breakpoint
         if(finalCounter == -1){
	   counter = 0
	   display.append("No breakpoints found.\n")
	 }
	 // If you hit a breakpoint, display the next line
	 else{
	   envdisplay.text = ""
	   val file = Source.fromFile("steps.txt").getLines()
	   counter = finalCounter + 1
	   finalCounter = 0
	   
	   var bpLine = file drop(counter) next()
	   counter = counter + 1
	   display.append(bpLine+"\n")

           val envFile = Source.fromFile("env.txt").getLines()
	   for(line <- envFile){
	      envdisplay.append(line+"\n")
	   }
	   
	 }


      // IF RUNNING THE CODE
      case ButtonClicked(component) if component == runButton =>
        // Make sure file is clear before beginning the new evaluation	
        val writer = new PrintWriter("steps.txt")
        counter = 0
	numLines = 0
	
        display.append("\n ------ Evaluating ------ \n")

	shell()
	for(line <- Source.fromFile("steps.txt").getLines()){
          numLines = numLines + 1
	}
	if(!inDebugMode){
	  val wholeFile = Source.fromFile("steps.txt").getLines()
	  val resultLine = wholeFile drop((numLines-2)) next()
	  display.append(resultLine)
	  counter = 0
	  numLines = 0
	}

    }

   /*************************** UPDATES TO THE ORIGINAL SHELL ******************************/

   val parser = new SExpParser

   def parse (input:String) : ShellEntry = {
      parser.parseAll(parser.shell_entry, input) match {
         case parser.Success(result,_) => result
         case failure : parser.NoSuccess => throw new Exception("Cannot parse "+input+": "+failure.msg)
      }  
   }


   def time[R](block: => R):R = {
     val t0 = System.currentTimeMillis()
     val result = block
     val t1 = System.currentTimeMillis()
     println("Elapsed time: " + (t1-t0) + "ms")
     result
   }
   
   val nullEnv = new Env[Value](List())
   
   //
   // Standard environment
   //

   val stdEnv = new Env[Value](List(
     ("true",new VBoolean(true)),
     ("false",new VBoolean(false)),
     ("not", new VRecClosure("",List("a"), new EIf(new EId("a"), new EBoolean(false), new EBoolean(true)),nullEnv)),
     ("+", new VPrimOp(Ops.operPlus)),
     ("*", new VPrimOp(Ops.operTimes)),
     ("=", new VPrimOp(Ops.operEqual)),
     ("<", new VPrimOp(Ops.operLess)),
     ("empty?",new VPrimOp(Ops.operEmpty)),
     ("first",new VPrimOp(Ops.operFirst)),
     ("rest",new VPrimOp(Ops.operRest)),
     ("empty",new VVector(List())),
     ("cons",new VPrimOp(Ops.operCons)),
   ))

   
   val stdSymt = new Env[Type](List(
     ("true",TBoolean),
     ("false",TBoolean),
     ("not", new TFunction(List(TBoolean),TBoolean)),
     ("+", new TFunction(List(TInteger,TInteger),TInteger)),
     ("*", new TFunction(List(TInteger,TInteger),TInteger)),
     ("=", new TFunction(List(TInteger,TInteger),TBoolean)),
     ("<", new TFunction(List(TInteger,TInteger),TBoolean)),
     ("empty?",new TFunction(List(TIntVector),TBoolean)),
     ("first",new TFunction(List(TIntVector),TInteger)),
     ("rest",new TFunction(List(TIntVector),TIntVector)),
     ("empty", TIntVector),
     ("cons",new TFunction(List(TInteger,TIntVector),TIntVector))
   ))

  var env = stdEnv
  var symt = stdSymt

   def shell () : Unit = {   
       
       print("TFUNC> ")
       try {
          // Accept input from the GUI text field
          val input = textField.text
	  
          val se = parse(input)
	  val result = time { se.processEntry(env,symt,inDebugMode,into) }
	  env = result._1
	  symt = result._2

       }catch {
          case e : Exception => println(e.getMessage)
       }
   }
}