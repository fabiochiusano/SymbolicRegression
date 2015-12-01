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

  def getRandomIntFromGeometric(low: Int, max: Int, step: Int): Int =
    if (low == max) low
    else if (getRandomIntIn(1, step) == 1) low
    else getRandomIntFromGeometric(low + 1, max, step)
    
    case class Population(trees: List[Tree], numOfTrees: Int, minConst: Int, maxConst: Int, numOfVars: Int, maxHeight: Int) {
    // Constructors.
    def this(numOfTrees: Int, minConst: Int, maxConst: Int, numOfVars: Int, maxHeight: Int) =
      this(for (i <- (1 to numOfTrees).toList)
        yield getRandomTree(3, maxHeight, minConst, maxConst, numOfVars).simplify, numOfTrees, minConst, maxConst, numOfVars, maxHeight)

    // Next generation.
    def nextGeneration(expected: List[(Map[String, Double], Double)]): Population = {
      // Returns a list of pairs (tree, sum of all errors of such tree in the environments).
      val pairList = for {
        tree <- trees
      } yield (tree, expected.map { case (env, n) => Math.abs(tree.eval(env) - n) }.foldRight(0.0)(_ + _))
      // Sort (tree, sumError)s going from small sumError to big sumError (from high fitness to small fitness).
      val sortedTrees = pairList.sortBy(x => x._2).map(x => x._1)
      // Create the offspring:
      // - 50% of crossover.
      // - 25% of reproduction.
      // - 25% of mutation.
      val numOfReproduce = Math.round(numOfTrees / 20)
      val numOfCrossover = if (Math.round(numOfTrees - numOfReproduce / 2.0) % 2 == 0)
        Math.round(numOfTrees - numOfReproduce / 2) else (Math.round(numOfTrees / 2.0) - 1).toInt
      val numOfMutation = Math.round((numOfTrees - numOfCrossover - numOfReproduce) / 3)
      val numOfRandom = numOfTrees - numOfCrossover - numOfMutation - numOfReproduce
      def getRandomCrossoverFromSortedTrees(sortedTrees: List[Tree]): (Tree, Tree) = {
        val i1 = getRandomIntFromGeometric(1, sortedTrees.length, sortedTrees.length / 2)
        val parent1 = sortedTrees(i1 - 1)
        val newSortedPairList = sortedTrees.filter(tree => tree != parent1)
        val i2 = getRandomIntFromGeometric(1, newSortedPairList.length, newSortedPairList.length / 2)
        val parent2 = newSortedPairList(i2 - 1)
        parent1.crossover(parent2)
      }
      val treesFromCrossover = (for (i <- (1 to numOfCrossover by 2).toList) yield getRandomCrossoverFromSortedTrees(sortedTrees)).map(pair => List(pair._1, pair._2)).flatten
      val treesFromReproduce = sortedTrees.take(numOfReproduce)//for (i <- (1 to numOfReproduce).toList) yield sortedTrees(getRandomIntFromGeometric(1, sortedTrees.length) - 1)
      val treesFromMutation = for (i <- (1 to numOfMutation).toList) yield sortedTrees(getRandomIntFromGeometric(1, sortedTrees.length, sortedTrees.length / 4) - 1).mutate(minConst, maxConst, numOfVars)
      val treesFromRandom = for (i <- (1 to numOfRandom).toList) yield getRandomTree(3, maxHeight, minConst, maxConst, numOfVars).simplify
      val newTrees = (treesFromCrossover ::: treesFromReproduce ::: treesFromMutation ::: treesFromRandom).map { 
        t => if(getRandomIntIn(1, numOfTrees) == 1) t.simplify else t }
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
          if (getRandomIntIn(1, 2) == 1) ConstTermNode(getRandomIntIn(constMin, constMax))
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
    
    //util
    def simplify: Tree = {
      var t = this
      (1 to t.height).toList.foreach { i => t = simplifyAux(t) }
      def simplifyAux(t: Tree): Tree = t match {
        case BinFuncNode(f, l, r) => (l, r) match {
          case (ConstTermNode(n1), ConstTermNode(n2)) => f match {
            case PlusOp => ConstTermNode(n1 + n2)
            case MinusOp => ConstTermNode(n1 - n2)
            case TimesOp => ConstTermNode(n1 * n2)
            case DivOp => if(n2 != 0) ConstTermNode(n1 / n2) else ConstTermNode(scala.Double.MaxValue)
          }
          case (IdTermNode(n1), IdTermNode(n2)) if(n1 == n2) => f match {
            case PlusOp => t
            case MinusOp => ConstTermNode(0.0)
            case TimesOp => t
            case DivOp => ConstTermNode(1.0)
          }
          case _ => BinFuncNode(f, simplifyAux(l), simplifyAux(r))
        }
        case _ => t
      }
      t
    }
  }

  abstract class FuncNode extends Tree
  case class BinFuncNode(f: Op, l: Tree, r: Tree) extends FuncNode {
    // General.
    def eval(mapId: Map[String, Double]): Double = f match {
      case PlusOp  => l.eval(mapId) + r.eval(mapId)
      case MinusOp => l.eval(mapId) - r.eval(mapId)
      case TimesOp => l.eval(mapId) * r.eval(mapId)
      case DivOp =>
        val (num, den) = (l.eval(mapId), r.eval(mapId))
        if (den == 0) scala.Double.MaxValue else num / den
    }
    def height: Int = Math.max(l.height, r.height) + 1
    def randomNode(h: Int): Tree = {
      if (this.height == h) this
      else {
        if (l.height >= h && r.height >= h) if (getRandomIntIn(1, 2) == 1) l.randomNode(h) else r.randomNode(h)
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
    ('a' + getRandomIntIn(0, numOfVars - 1)).toChar.toString
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
      } else {
        val a = getRandomProb
        if (a <= 50.0) {
          getRandomTermNode(constMin, constMax, numOfVars)
        } else {
          val op = getRandomOp
          val lH = getRandomIntIn(1, maxH - 1)
          val rH = getRandomIntIn(1, maxH - 1)
          val l = getRandomTree(lH, lH, constMin, constMax, numOfVars)
          val r = getRandomTree(rH, rH, constMin, constMax, numOfVars)
          BinFuncNode(op, l, r)
        }
      }
    } else {
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
    /*print("Number of generations: ")
    val numOfGens = readLine.toInt
    print("Number of trees: ")
    val numOfTrees = readLine.toInt
    print("Max height: ")
    val maxHeight = readLine.toInt
    print("Min and max constant: ")
    val Array(minConst, maxConst) = readLine.split(' ').map(x => x.toInt)
    print("Number of variables: ")
    val numOfVars = readLine.toInt
    print("Number of environments: ")
    val numOfEnv = readLine.toInt
    val environments = (for (i <- 1 to numOfEnv) yield {
      print("Environment number " + i + " (please list all the variables in order and then the expected result): ")
      val nums = readLine.split(' ').map(x => x.toInt)
      val binds = for (j <- 0 until numOfVars) yield (('a' + j).toChar.toString, nums(j).toDouble)
      val expected = nums(numOfVars)
      (binds.toMap, expected.toDouble)
    }).toList*/
    val numOfGens = 100
    val numOfTrees = 100
    val maxHeight = 15
    val Array(minConst, maxConst) = Array(-5, 5)
    val numOfVars = 2
    val numOfEnv = 21*21
    //val environments = List((Map("a" -> -3.0), 7.0),(Map("a" -> -2.0), 3.0),(Map("a" -> -1.0), 1.0),(Map("a" -> 0.0), 1.0),
    //    (Map("a" -> 1.0), 3.0),(Map("a" -> 2.0), 7.0),(Map("a" -> 3.0), 13.0))
    val environments = (for(i <- -20 to 20 by 2; j <- -20 to 20 by 2) yield {
      val value = i * j - 1 + 2 * i - 7 * j
      (Map("a" -> i.toDouble, "b" -> j.toDouble), value.toDouble)
    }).toList

    val initialPopulation = new Population(numOfTrees, minConst, maxConst, numOfVars, maxHeight)
    
    def generations(n: Int): List[Population] = if (n == 1) List(initialPopulation) else {
      val gens = generations(n - 1)
      val newPop = gens.head.nextGeneration(environments)
      newPop :: gens
    }
    
    val gens = generations(numOfGens)
    
    println(gens.reverse.map(p => "Population " + (gens.length-gens.indexOf(p)) + " :\n" + 
        {val tupl = p.getLowestFitness(environments); tupl._1.toString() + "\nFitness: " + tupl._2}).mkString("\n"))
    
    //println("\n\n" + gens(0).getLowestFitness(environments)._1 + "\n\nSimplified version: " + gens(0).getLowestFitness(environments)._1.simplify)
  }
}