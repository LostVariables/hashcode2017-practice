import scala.io.Source
import java.io._
import scala.collection.mutable.Map
import scala.collection.immutable.{Map => IMap}
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.Random
import java.util.NoSuchElementException
import scala.concurrent.Future
import scala.concurrent.Await
import java.util.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import org.jenetics._
import engine._
import util._
import scala.language.implicitConversions
import scala.collection.JavaConversions._

object Main extends App{
    def solve(filename: String) = {
        val fin = Source.fromFile(s"inputs/$filename.in")
        val lines = fin.getLines().toList
        fin.close
        val Array(cR, cC, cL, cH) = lines.head.split(" ").map(_.toInt)

        val pizza: List[String] = lines.tail

        type Slice = (Int, Int, Int, Int)

        def count(slice: Slice): (Int, Int) = {
            val (r1, c1, r2, c2) = slice
            val rows = pizza.map(_.substring(c1, c2 + 1)).zipWithIndex.filter{
                case (_, i) => i >= r1 && i <= r2
            }.unzip._1 map {
                s: String => s.foldLeft((0,0)) {
                    (tm, c) =>
                        val (t,m) = tm
                        if (c == 'T') (t+1, m) else (t, m+1)
                }
            }
            rows.foldLeft((0,0)) {
                (ttm, tnm) =>
                val (tt, tm) = ttm
                val (t, m) = tnm
                (tt + t, tm + m)
            }
        }

        def isFree(x: Int, y: Int, firstFreePerCol: List[Int]) = {
            firstFreePerCol(y) <= x
        }

        def isSliceFree(slice: Slice, firstFreePerCol: List[Int]) = {
            val (r1, c1, r2, c2) = slice

            (for {
                x <- r1 to r2
                y <- c1 to c2
            } yield (x,y)).forall {
                case (x, y) => isFree(x, y, firstFreePerCol)
            }
        }

        def isValid(slice: Slice, firstFreePerCol: List[Int]) = {
            val (r1, c1, r2, c2) = slice
            // print(s"$slice $cR $cC")
            if (r2 < cR && c2 < cC && isSliceFree(slice, firstFreePerCol)) {
                // println(s"${isSliceFree(slice, firstFreePerCol)} ${count(slice)}")
                val (t, m) = count(slice)
                t >= cL && m >= cL && t + m  <= cH
            } else {
                false
            }
        }

        val dimensions = {
            var d: List[(Int, Int)] = List()
            for {
                cT <- cH to 1 by -1
                h <- 1 to cT
                w = cT/h
                if cT % h == 0 && w*h >= 2*cL
            } d = (w, h) :: d
            d
        }

        def firstFree(firstFreePerCol: List[Int]) = {
            var (firstI, firstJ) = (cR,cC)
            for {
                (i,j) <-firstFreePerCol.zipWithIndex
                if i < firstI
            } {
                firstI = i
                firstJ = j
            }
            (firstI, firstJ)
        }

        def countFree2(firstFreePerCol: List[Int]) = {
            firstFreePerCol.foldLeft(0) {
                (s, i) => s + cR - i
            }
        }

        def updateFreeOnPush(slice: Slice, firstFreePerCol: List[Int]) = {
            val (r1, c1, r2, c2) = slice
            val height = r2 - r1 + 1
            (c1 to c2).foldLeft(firstFreePerCol) {
                (f, j) =>
                    f.updated(j, f(j) + height)
            }
        }

        def updateFreeOnPop(slice: Slice, firstFreePerCol: List[Int]) = {
            val (r1, c1, r2, c2) = slice
            val height = r2 - r1 + 1
            (c1 to c2).foldLeft(firstFreePerCol) {
                (f, j) =>
                    f.updated(j, f(j) - height)
            }
        }

        def createSlice2(cell: (Int, Int), dim: (Int, Int)) = {
            val (i, j) = cell
            val (w, h) = dim

            (i, j, i + w - 1, j + h -1)
        }

        trait Context { self =>
            def slices: List[Slice]
            def dimIndexes: List[Int]
            def firstFreePerCol: List[Int]

            def updateOnPush(dimIndex: Int) = new Context {
                val s = self.createSlice(dimIndex)
                val slices = s::self.slices
                val dimIndexes = dimIndex::self.dimIndexes
                val firstFreePerCol = updateFreeOnPush(s, self.firstFreePerCol)
            }

            def updateOnPop = new Context {
                val slices = self.slices.tail
                val dimIndexes = self.dimIndexes.tail
                val firstFreePerCol = updateFreeOnPop(self.slices.head, self.firstFreePerCol)
            }

            def createSlice(dimIndex: Int): Slice = {
                createSlice2(firstFree(self.firstFreePerCol), dimensions(dimIndex))
            }

            def isDimValid(dimIndex: Int): Boolean = {
                val s = self.createSlice(dimIndex)
                isValid(s, self.firstFreePerCol)
            }

            def countFree: Int = {
                countFree2(self.firstFreePerCol)
            }
        }

        object initContext extends Context {
            val slices = List()
            val dimIndexes = List()
            val firstFreePerCol = List.fill(cC)(0)
        }


         trait Context2 { self =>
            val freeCells: IMap[(Int, Int), Boolean]
            val slices: List[Slice]
            val freePositions: IndexedSeq[(Int, Int)]
            val noMore = false
            def isSliceFree(slice: Slice): Boolean = {
                val (r1, c1, r2, c2) = slice
                val cells = for {
                    x <- r1 to r2
                    y <- c1 to c2
                } yield (x,y)

                cells.forall(freeCells(_))
            }
            def isDimValid(position: (Int, Int), dimIndex: Int): Boolean = {
                val slice = createSlice2(position, dimensions(dimIndex))
                val (r1, c1, r2, c2) = slice
                lazy val (t,m) = count(slice)

                r2 < cR && c2 < cC &&
                isSliceFree(slice) &&
                (t >= cL && m >= cL &&
                m + t <= cH)
            }
            def updateOnPush(position: (Int, Int), dimIndex: Int): Context2 = {
                val slice = createSlice2(position, dimensions(dimIndex))
                val (r1, c1, r2, c2) = slice
                val cells = for {
                    x <- r1 to r2
                    y <- c1 to c2
                } yield (x,y)

                new Context2 {
                    val slices = slice :: self.slices
                    val freeCells = cells.foldLeft(self.freeCells)(_.updated(_,false))
                    val freePositions = self.freePositions.filter(freeCells(_))
                }
            }

            def noMoreInst: Context2 = {
                new Context2 {
                    val slices = self.slices
                    val freeCells = self.freeCells
                    val freePositions = self.freePositions
                    override val noMore = true
                }
            }

            def countFree: Int = {
                if (noMore) 0
                else cR*cC - freeCells.keys.toList.length
            }

            def shouldKill: Boolean = {
                self.freePositions.filter {
                    p => (0 until dimensions.length).exists(self.isDimValid(p,_))
                } isEmpty
            }
        }
        val positions = for {
            x <- 0 until cR
            y <- 0 until cC
        } yield (x,y)
        object initContext2 extends Context2 {
            override val slices = List[Slice]()
            override val freeCells: IMap[(Int, Int), Boolean] = IMap().withDefault(_ => true)
            override val freePositions: IndexedSeq[(Int, Int)] = positions
        }

        def genSolve(threshold: Int): List[Slice] = {
            val maxSlices = cR*cC/(cL*2)

            val nextContext: java.util.function.Supplier[Context2] = () => {
                val rand = new Random()
                val posrand = new Random()
                val maxSlices = cR*cC/(cL*2)

                @tailrec def aux(length: Int, c: Context2, retry: Int): Context2 = {
                    if (retry < 0) initContext2;
                    else if (length == 0) c
                    else {
                        val position = c.freePositions(posrand.nextInt(c.freePositions.length))
                        val validDimIndexes = (0 until dimensions.length).filter(c.isDimValid(position, _))
                        lazy val dimIndex = validDimIndexes(rand.nextInt(validDimIndexes.length))
                        if (!validDimIndexes.isEmpty) aux(length-1, c.updateOnPush(position, dimIndex), retry)
                        else aux(length, c, retry - 1)
                    }
                }

                val length = 5
                aux(length, initContext2, cR*cC)
            }

            val decoder: java.util.function.Function[Genotype[AnyGene[Context2]], Context2] =
                gt => gt.getGene.getAllele

            val gtf: Factory[Genotype[AnyGene[Context2]]] =
                Genotype.of(
                    AnyChromosome.of(nextContext)
                )

            val codec = Codec.of(gtf, decoder)

            val alterer = new Alterer[AnyGene[Context2], Integer] {
                override def alter(population: Population[AnyGene[Context2], Integer], generation: Long): Int = {
                    val newPhenotypes = population.map {
                        pht => {
                            val chromosome = pht.getGenotype.getChromosome
                            val gene = chromosome.getGene
                            val c: Context2 = gene.getAllele
                            val rand = new Random()
                            val posrand = new Random()

                            @tailrec def aux(retry: Int): Context2 = {
                                if (retry < 0 && c.shouldKill) initContext2
                                else {
                                    val dimIndex = rand.nextInt(dimensions.length)
                                    val position = c.freePositions(posrand.nextInt(c.freePositions.length))
                                    if(c.isDimValid(position, dimIndex)) {
                                        c.updateOnPush(position, dimIndex)
                                    }
                                    else aux(retry - 1)
                                }
                            }

                            val newGene = gene.newInstance(aux(c.freePositions.length))
                            val newChromosome = chromosome.newInstance(ISeq.of(newGene))
                            pht.newInstance(Genotype.of(newChromosome), generation)
                        }
                    }
                    population.clear
                    population.addAll(newPhenotypes)
                    newPhenotypes.length
                }
            }

            val eval: java.util.function.Function[Context2, Integer] = c => c.countFree

            val engine = Engine.builder[Context2, AnyGene[Context2], Integer](eval, codec).
                            optimize(Optimize.MINIMUM).
                            populationSize(4).
                            alterers(alterer).
                            offspringFraction(0.7).
                            survivorsSelector(new TruncationSelector).
                            offspringSelector(new TruncationSelector).
                            build

            val bestPhenotype = engine.stream.
                                    limit {
                                        limit.byFitnessThreshold[Integer](threshold + 1)
                                    } collect(EvolutionResult.toBestPhenotype[AnyGene[Context2], Integer])

            decoder(bestPhenotype.getGenotype).slices
        }

        def solveWithThreShold(threshold: Int) = Future {
            // def par(dimIndexStart: Int, c: Context): Future[List[Slice]] = {
            //     val parLevel = 4
            //     val nums = (dimIndexStart until dimIndexStart + parLevel)
            //     val fs = nums map {
            //         d: Int => aux(d,c)
            //     }
            //     Future.firstCompletedOf(fs)
            //     //(_ != None).map(_.getOrElse(List[Slice]()))
            // }
            // @tailrec def aux(dimIndex: Int, c: Context): Future[List[Slice]] = {
            //     // println(s"${c.slices.length} ${dimIndex}")
            //     if (dimIndex >= dimensions.length) {
            //         if (c.slices.length <= 0) Future.failed(new NoSuchElementException)
            //         else aux(c.dimIndexes.head + 1, c.updateOnPop)
            //     } else {
            //         if (c.isDimValid(dimIndex)) {
            //             val updatedC = c.updateOnPush(dimIndex)
            //             if (countFree(updatedC.firstFreePerCol) <= threshold) Future.successful (updatedC.slices)
            //             else aux(0, updatedC)
            //         } else aux(dimIndex + 1, c)
            //     }
            // }
            // aux(0, initContext)
            // def initial = Stream.tabulate(dimensions.length)((_, initContext))
            //
            // from(initial).filter {
            //     case (_, c) => c.countFree <= threshold
            // } match {
            //     case (_, c: Context) #:: _ => c.slices
            // }

            // def aux2(s: Stream[(Int, Context)]): List[Slice] = {
            //     s match {
            //         case (_, c) #:: tail => if(c.countFree <= threshold) c.slices else aux2(tail)
            //         case Stream.Empty => throw new NoSuchElementException
            //     }
            // }
            //
            // aux2(from(initial))

            genSolve(threshold)
        }


        def from(initial: => Stream[(Int, Context)]): Stream[(Int, Context)] = {
            initial match {
                case (dimIndex, c) #:: tail => {
                    if (c.isDimValid(dimIndex)) {
                        val uC = c.updateOnPush(dimIndex)
                        def newSlices = Stream.tabulate(dimensions.length)((_, uC))
                        (dimIndex, uC) #::
                                    // from(tail) ++ from(newSlices) //wfs
                                    from(newSlices) ++ from(tail) // dfs
                    } else from(tail)
                }
                case Stream.Empty => Stream.Empty
            }
        }

        def validate(slices: List[Slice]): Boolean = {
            @tailrec def aux(slices: List[Slice], firstFreePerColl: List[Int]): Boolean = {
                if (slices.isEmpty)
                    true
                else if (isValid(slices.head, firstFreePerColl))
                    aux(slices.tail, updateFreeOnPush(slices.head, firstFreePerColl))
                else
                    false
            }
            val res = !slices.isEmpty && aux(slices, List.fill(cC)(0))
            println(s"The result is ${if (res) "" else "not "}valid")
            res
        }

        def runWithTimeOut[T](m: Long)(f: => T): Option[T] = {
            Try(Await.result(Future(f), m.minutes)).toOption
        }

        println(dimensions.zipWithIndex)
        val results: Map[Int, List[Slice]] = Map()
        // (0 until cR*cC by 100).
        // List(cR*cC - 2000).
        List(0).
        // (41000 to 49000 by 1000).
        foreach {
            t: Int =>
            println(s"$t - started")
            val f = solveWithThreShold(t)
            Try(Await.result(f, 10 minutes)) match {
                case Success(x) => {
                    results(t) = x.reverse
                    println(s"$t - ended")
                }
                case Failure(e) => e.printStackTrace;println(s"$t - failed ${e.getMessage}")
            }

            // onComplete {
            //     case Success(x) => {
            //         results(t) = x.reverse
            //         println(s"$t - ended")
            //     }
            //     case Failure(e) => println(e)
            // }
        }

        Try(handle(results(results.keys.min)))

        def handle(slices: List[Slice]) {
            validate(slices)
            write(slices)
            writeHtml(slices)
        }

        def write(slices: List[Slice]) {
            val pw = new PrintWriter(new File(s"submissions/$filename.out"))
            pw.write(s"${slices.length}\n")
            for (slice <- slices) {
                val (r1, c1, r2, c2) = slice
                pw.write(s"$r1 $c1 $r2 $c2\n")
            }

            pw.close
        }

        def writeHtml(slices: List[Slice]) {
            val pw = new PrintWriter(new File(s"html/$filename.html"))
            pw.write {
                """
                <!DOCTYPE html>
                <html>
                    <head>
                        <title>Pizza</title>
                        <link rel="stylesheet" href="style.css" />
                        <link rel="stylesheet" href="bootstrap.css" />
                    </head>
                    <body>
                """
            }
            for ((row,i) <- pizza.zipWithIndex) {
                pw.write(s"""<div class="row">""")
                for ((cell, j) <- row.zipWithIndex) {
                    val sliceIdx = slices.indexWhere {
                        case (r1, c1, r2, c2) => i >= r1 && i <= r2 && j >= c1 && j <= c2
                    } % 17
                    val classS = if (cell == 'T') "tomato" else "mushroom"
                    pw.write(s"""<div class="slice-$sliceIdx $classS col-md-1"></div>""")
                }
                pw.write("</div>")
            }
            pw.write("</body></html>")
            pw.close
        }
    }

    solve("example")
    solve("small")
    // solve("medium")
    // solve("big")
}
