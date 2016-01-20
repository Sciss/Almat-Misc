case class T(x: Double, y: Double)

val dir = file("/home/hhrutz/Documents/projects/Unlike/moor_8024_json")
val files = dir.children(_.name.endsWith("json"))
val filesS = files.sortBy(_.name)
val lines = filesS.map { f =>
  val s = scala.io.Source.fromFile(f).getLines.next()
  val i = s.indexOf(':') + 1
  val j = s.indexOf(',', i)
  val k = s.indexOf(':', j) + 1
  val l = s.indexOf(',', k)
  val x = s.substring(i, j).toDouble
  val y = s.substring(k, l).toDouble
  T(x, y)
}

val tmaxx = lines.maxBy(_.x)
val tmaxy = lines.maxBy(_.y)
val fmaxx = filesS(lines.indexOf(tmaxx)) // moor_8024-02557-02558.json
val fmaxy = filesS(lines.indexOf(tmaxy)) // moor_8024-05180-05181.json

// moor_8024/moor_8024-02557.jpg
// moor_8024/moor_8024-05180.jpg

lines(lines.indexOf(tmaxx)+1)
lines(lines.indexOf(tmaxx)+2)
lines(lines.indexOf(tmaxx)+3)
lines(lines.indexOf(tmaxx)+4)



