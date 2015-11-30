package main

/**
 * @author fabiochiusano
 */
object main {
  def main(args: Array[String]) {
    GPTrees.run
  }
}

object GPTrees {

  val r = scala.util.Random
  
  def getRandomIntIn(low: Int, high: Int) = low + r.nextInt(high - low + 1)

  abstract class Tree {
    // General
    def eval(mapId: Map[String, Double]): Double
    def height: Int
    def randomNode(h: Int): Tree
    
    // Reproduction.
    def update(oldNode: Tree, newNode: Tree): Tree
    def mutate(constMin: Int, constMax: Int, numOfVars: Int): Tree = {
      val hMutation = getRandomIntIn(1, this.height)
      val nodeToMutate = this.randomNode(hMutation)
      val newNode = nodeToMutate match {
        case BinFuncNode(op, l, r) => BinFuncNode(getRandomOp, l, r)
        case _ =>
          if (getRandomIntIn(1,2) == 1) ConstTermNode(getRandomIntIn(constMin, constMax))
          else IdTermNode(getRandomId(numOfVars))
      }
      this.update(nodeToMutate, newNode)
    }
    def crossover(other: Tree): (Tree, Tree) = {
      val h = getRandomIntIn(1, Math.min(this.height, other.height))
      val n1 = this.randomNode(h)
      val n2 = other.randomNode(h)
      (this.update(n1, n2), other.update(n2, n1))
    }
    def reproduce: Tree = this
  }
  
  abstract class FuncNode extends Tree
  case class BinFuncNode(f: Op, l: Tree, r: Tree) extends FuncNode {
    // General.
    def eval(mapId: Map[String, Double]): Double = f match {
      case PlusOp => l.eval(mapId) + r.eval(mapId)
      case MinusOp => l.eval(mapId) - r.eval(mapId)
      case TimesOp => l.eval(mapId) * r.eval(mapId)
      case DivOp =>
        val (num, den) = (l.eval(mapId), r.eval(mapId))
        if (den == 0) 1 else num/den
    }
    def height: Int = Math.max(l.height, r.height) + 1
    def randomNode(h: Int): Tree = {
      if (this.height == h) this
      else {
        if (l.height >= h && r.height >= h) if (getRandomIntIn(1,2) == 1) l.randomNode(h) else r.randomNode(h)
          else if (l.height >= h) l.randomNode(h)
          else if (r.height >= h) r.randomNode(h)
          else this
      }
    }
    
    // Reproduction.
    def update(oldNode: Tree, newNode: Tree): Tree =
      if (this == oldNode) newNode else BinFuncNode(f, l.update(oldNode, newNode), r.update(oldNode, newNode))
    
    override def toString = "(" + l.toString + " " + f.toString + " " + r.toString + ")"
  }
  
  abstract class TermNode extends Tree {
    // General.
    lazy val height: Int = 1
    def randomNode(h: Int): Tree = this
  }
  case class ConstTermNode(n: Double) extends TermNode {
    // General
    def eval(mapId: Map[String, Double]): Double = n
    
    // Reproduction.
    def update(oldNode: Tree, newNode: Tree): Tree = if (this == oldNode) newNode else this
    
    override def toString = n.toString
  }
  case class IdTermNode(name: String) extends TermNode {
    // General
    def eval(mapId: Map[String, Double]): Double = mapId(name)
    
    // Reproduction.
    def update(oldNode: Tree, newNode: Tree): Tree = if (this == oldNode) newNode else this
    
    override def toString = name
  }
  
  abstract class Op
  object PlusOp extends Op {
    override def toString = "+"
  }
  object MinusOp extends Op {
    override def toString = "-"
  }
  object TimesOp extends Op {
    override def toString = "*"
  }
  object DivOp extends Op {
    override def toString = "/"
  }
  
  def getRandomId(numOfVars: Int): String = {
    val a = r.nextInt(numOfVars)
    if (a == 0) "x"
    else if (a == 1) "y"
    else "z"
  }
  
  def getRandomProb: Int = r.nextInt(101)
  
  def getRandomOp: Op = {
    val a = getRandomProb
    if (a <= 25) PlusOp
    else if (a <= 50) MinusOp
    else if (a <= 75) TimesOp
    else DivOp
  }
  
  def getRandomTermNode(constMin: Int, constMax: Int, numOfVars: Int): TermNode = {
    val a = getRandomProb
    if (a <= 50) ConstTermNode(getRandomIntIn(constMin, constMax))
    else IdTermNode(getRandomId(numOfVars))
  }
  
  def getRandomTree(minH: Int, maxH: Int, constMin: Int, constMax: Int, numOfVars: Int): Tree = {
    if (minH == 1) {
      if (maxH == 1) {
        getRandomTermNode(constMin, constMax, numOfVars)
      }
      else {
        val a = getRandomProb
        if (a <= 50.0) {
          getRandomTermNode(constMin, constMax, numOfVars)
        }
        else {
          val op = getRandomOp
          val lH = getRandomIntIn(1, maxH - 1)
          val rH = getRandomIntIn(1, maxH - 1)
          val l = getRandomTree(lH, lH, constMin, constMax, numOfVars)
          val r = getRandomTree(rH, rH, constMin, constMax, numOfVars)
          BinFuncNode(op, l, r)
        }
      }
    }
    else {
      val op = getRandomOp
      val lH = getRandomIntIn(minH - 1, maxH - 1)
      val sH = getRandomIntIn(1, maxH - 1)
      val longTree = getRandomTree(lH, lH, constMin, constMax, numOfVars)
      val shortTree = getRandomTree(sH, sH, constMin, constMax, numOfVars)
      val a = getRandomProb
      if (a <= 50) BinFuncNode(op, longTree, shortTree)
      else BinFuncNode(op, shortTree, longTree)
    }
  }
                                                  
  def nextGeneration(trees: List[Tree], expected: List[(Map[String, Double], Double)], minConst: Int, maxConst: Int, numVars: Int): List[Tree] = {
    val pairList = for{
      tree <- trees
    } yield (tree, expected.map{ case(env, n) => Math.abs(tree.eval(env) - n) }.foldRight(0.0)(_ + _))
    val sortedPairList = pairList.sortBy(x => x._2).map(x => x._1)
    val repr = sortedPairList(0).reproduce
    val (cross1, cross2) = sortedPairList(0).crossover(sortedPairList(1))
    val mut = sortedPairList(1).mutate(minConst, maxConst, numVars)
    List(repr, cross1, cross2, mut)
  }
  
  def run = {
    val a = getRandomTree(2,4,-5,5,1)
    val b = getRandomTree(2,4,-5,5,1)
    val c = getRandomTree(2,4,-5,5,1)
    val d = getRandomTree(2,4,-5,5,1)
    
    val l1 = List(a,b,c,d)
    
    val expected = List((Map("x" -> -1.0), 1.0), (Map("x" -> 1.0), 3.0), (Map("x" -> 0.0), 1.0), (Map("x" -> 3.0), 13.0))
    
    val l2 = nextGeneration(l1, expected, -5, 5, 1)
    val l3 = nextGeneration(l2, expected, -5, 5, 1)
    val l4 = nextGeneration(l3, expected, -5, 5, 1)
    
    println("Pop 1 : " + l1)
    println("Fitnesses : " + l1.map(tree => expected.map{ case(env, n) => Math.abs(tree.eval(env) - n) }.foldRight(0.0)(_ + _)))
    println("Pop 2 : " + l2)
    println("Fitnesses : " + l2.map(tree => expected.map{ case(env, n) => Math.abs(tree.eval(env) - n) }.foldRight(0.0)(_ + _)))
    println("Pop 3 : " + l3)
    println("Fitnesses : " + l4.map(tree => expected.map{ case(env, n) => Math.abs(tree.eval(env) - n) }.foldRight(0.0)(_ + _)))
    println("Pop 4 : " + l4)
    println("Fitnesses : " + l4.map(tree => expected.map{ case(env, n) => Math.abs(tree.eval(env) - n) }.foldRight(0.0)(_ + _)))
  }
}