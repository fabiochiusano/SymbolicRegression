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
  
  def getRandomIntFromGeometric(low: Int, max: Int): Int =
    if (low == max) low
    else if (getRandomIntIn(low, low + 1) == low) low
    else getRandomIntFromGeometric(low + 1, max)
  
    case class Population(trees: List[Tree], numOfTrees: Int, minConst: Int, maxConst: Int, numOfVars: Int, maxHeight: Int) {
    // Constructors.
    def this(numOfTrees: Int, minConst: Int, maxConst: Int, numOfVars: Int, maxHeight: Int) =
      this(for (i <- (1 to numOfTrees).toList)
        yield getRandomTree(1, maxHeight, minConst, maxConst, numOfVars), numOfTrees, minConst, maxConst, numOfVars, maxHeight)
    
    // Next generation.
    def nextGeneration(expected: List[(Map[String, Double], Double)]): Population = {
      // Returns a list of pairs (tree, sum of all errors of such tree in the environments).
      val pairList = for{
        tree <- trees
      } yield (tree, expected.map{ case(env, n) => Math.abs(tree.eval(env) - n) }.foldRight(0.0)(_ + _))
      // Sort (tree, sumError)s going from small sumError to big sumError (from high fitness to small fitness).
      val sortedTrees = pairList.sortBy(x => x._2).map(x => x._1)
      // Create the offspring:
      // - 50% of crossover.
      // - 25% of reproduction.
      // - 25% of mutation.
      val numOfCrossover= if (Math.round(numOfTrees / 2.0) % 2 == 0) (Math.round(numOfTrees / 2.0)).toInt else (Math.round(numOfTrees / 2.0) - 1).toInt
      val numOfReproduce = Math.round((numOfTrees - numOfCrossover) / 2)
      val numOfMutation = numOfTrees - numOfCrossover - numOfReproduce
      def getRandomCrossoverFromSortedTrees(sortedTrees: List[Tree]): (Tree, Tree) = {
        val i1 = getRandomIntFromGeometric(1, sortedTrees.length)
        val parent1 = sortedTrees(i1 - 1)
        val newSortedPairList = sortedTrees.filter(tree => tree != parent1)
        val i2  = getRandomIntFromGeometric(1, newSortedPairList.length)
        val parent2 = newSortedPairList(i2 - 1)
        parent1.crossover(parent2)
      }
      val treesFromCrossover = (for (i <- (1 to numOfCrossover by 2).toList) yield getRandomCrossoverFromSortedTrees(sortedTrees)).map(pair => List(pair._1, pair._2)).flatten
      val treesFromReproduce = for (i <- (1 to numOfReproduce).toList) yield sortedTrees(getRandomIntFromGeometric(1, sortedTrees.length) - 1)
      val treesFromMutation = for (i <- (1 to numOfMutation).toList) yield sortedTrees(getRandomIntFromGeometric(1, sortedTrees.length) - 1).mutate(minConst, maxConst, numOfVars)
      val newTrees = treesFromCrossover ::: treesFromReproduce ::: treesFromMutation
      new Population(newTrees, numOfTrees, minConst, maxConst, numOfVars, maxHeight)
    }
    
    def getHighestFitness(expected: List[(Map[String, Double], Double)]): (Tree, Double) = {
      val treeAndErrors = for (tree <- trees) yield (tree, expected.map{case (env, y) => Math.abs(tree.eval(env) - y)}.foldLeft(0.0)(_ + _))
      treeAndErrors.sortBy(x => x._2).last
    }
    
    def getLowestFitness(expected: List[(Map[String, Double], Double)]): (Tree, Double) = {
      val treeAndErrors = for (tree <- trees) yield (tree, expected.map{case (env, y) => Math.abs(tree.eval(env) - y)}.foldLeft(0.0)(_ + _))
      treeAndErrors.sortBy(x => x._2).head
    }
    
    override def toString: String = (for (i <- (1 to numOfTrees).toList) yield ("Tree " + i.toString + " : " + trees(i-1).toString)).mkString("\n")
  }

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
    ('a' + numOfVars - 1).toChar.toString
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
  
  def run = {
    val pop1 = new Population(10, -2, 2, 1, 5)
    val expected = List((Map("a" -> -1.0), 1.0), (Map("a" -> 1.0), 3.0), (Map("a" -> 0.0), 1.0), (Map("a" -> 3.0), 13.0))
    
    var pops = List(pop1)
    (1 to 100).foreach{i =>
      pops = pops :+ pops.last.nextGeneration(expected)
    }
    
    println((for (pop <- pops) yield (pop + "\nBest fitness: " + pop.getLowestFitness(expected) + "\n --- \n")))

  }
}