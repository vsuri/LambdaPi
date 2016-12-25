import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException
import java.util.ArrayList
import java.util.regex._
import java.io._
//remove if not needed
import scala.collection.JavaConversions._

object LambdaTranslator {

  var piExp: String = ""

  var ascii: Int = 65

  var Expressions: ArrayList[String] = new ArrayList[String]()

  def main(args: Array[String]) {
    val console = System.console()
    var input = ""
    input += console.readLine("Enter a Lambda Expression\nAccepted syntax: \n Use L as the Lambda symbol\n Enter input using the following terms-\n  variable : i.e. x\n  lambda abstraction : i.e. Lx.y\n  function application : i.e. MN\n\n --> ")
    Expressions.add(input)
    for (n <- 0 until Expressions.size) {
      piExp = ""
      val Exp = Expressions.get(n)
      val head = new Node()
      println("--------------------- \n")
      ascii = 65
      head.Value = "_"
      head.Name = ascii.toChar
      ascii += 1
      println("Successfully read string: " + Exp)
      println("Storing in tree ....")
      AddToTree(Exp, head)
      ConvertToPi(head)
      println("pi-calculus output:\n" + piExp + "\n")
    }
  }

  def ReadLambdaExpressions(filename: String) {
    val Exp = ""
    var br: BufferedReader = null
    try {
      var CurrentLine: String = null
      br = new BufferedReader(new FileReader(filename))
      while ((CurrentLine = br.readLine()) != null) {
        if (CurrentLine.charAt(0) != '#') Expressions.add(CurrentLine)
      }
    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      try {
        if (br != null) br.close()
      } catch {
        case ex: IOException => ex.printStackTrace()
      }
    }
  }

  def CheckRegex(Exp: String): String = {
    var Match: String = null
    val pattern = "L[a-z].[a-zA-Z]|[A-Z][A-Z]|[a-z]"
    val r = Pattern.compile(pattern)
    val m = r.matcher(Exp)
    Match = if (m.find()) m.group(0) else ""
    Match
  }

  def AddToTree(Exp: String, Current: Node) {
    val SubExp = CheckRegex(Exp)
    if (SubExp.length == 1) {
      val tempNode = new Node(Current, ascii.toChar, "" + SubExp.charAt(0), 0)
      ascii += 1
      Current.SetChildRight(tempNode)
      Current = tempNode
      Exp = Exp.replaceFirst(SubExp, " ")
      AddToTree(Exp, Current)
    } else if (SubExp.length == 4 && SubExp.charAt(0) == 'L' && SubExp.charAt(2) == '.') {
      val lambdaNode = new Node(Current, ascii.toChar, SubExp.substring(0, 2), 1)
      ascii += 1
      Current.SetChildLeft(lambdaNode)
      Current = lambdaNode
      Exp = Exp.replaceFirst(SubExp.substring(0, 2), " ")
      AddToTree(Exp, Current)
    } else if (SubExp.length == 2) {
      val blankNode = new Node()
      blankNode.SetParent(Current)
      Current.SetChildLeft(blankNode)
      blankNode.Name = ascii.toChar
      ascii += 1
      val leftNode = new Node(Current, ascii.toChar, "" + SubExp.charAt(0), 1)
      ascii += 1
      blankNode.SetChildLeft(leftNode)
      blankNode.Value = "_"
      val rightNode = new Node(Current, ascii.toChar, "" + SubExp.charAt(1), 0)
      ascii += 1
      blankNode.SetChildRight(rightNode)
      Current = leftNode
      Exp = Exp.replaceFirst(SubExp, " ")
      AddToTree(Exp, Current)
    }
  }

  def PrintTree(N: Node) {
    println(N.GetValue())
    if (N.hasLeft) PrintTree(N.GetChildLeft())
    if (N.hasRight) PrintTree(N.GetChildRight())
    return
  }

  def ConvertToPi(N: Node) {
    var Parent = '_'
    var LeftChild = '_'
    var RightChild = '_'
    if (N.hasParent) Parent = N.GetParent().Name
    if (N.hasLeft) LeftChild = N.GetChildLeft().Name
    if (N.hasRight) RightChild = N.GetChildRight().Name
    if (piExp.length > 1) piExp += "| "
    piExp += N.Name + " < " + Parent + ", " + LeftChild + ", " + RightChild + 
      ", " + 
      N.GetValue() + 
      "> "
    if (N.hasLeft) ConvertToPi(N.GetChildLeft())
    if (N.hasRight) ConvertToPi(N.GetChildRight())
    return
  }
}
