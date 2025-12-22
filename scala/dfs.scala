import scala.io.Source
import java.nio.file.{Files, Paths}

object DFS {
  def main(args: Array[String]): Unit = {
    // Очень простой парсинг аргументов
    var inputPath = "input.txt"
    var outputPath = "output.txt"

    var i = 0
    while (i < args.length) {
      args(i) match {
        case "-i" if i + 1 < args.length => inputPath = args(i + 1); i += 1
        case "--input" if i + 1 < args.length => inputPath = args(i + 1); i += 1
        case "-o" if i + 1 < args.length => outputPath = args(i + 1); i += 1
        case "--output" if i + 1 < args.length => outputPath = args(i + 1); i += 1
        case _ =>
      }
      i += 1
    }

    // Чтение файла
    val lines = try {
      Source.fromFile(inputPath, "UTF-8").getLines().map(_.trim).filter(_.nonEmpty).toVector
    } catch {
      case _: java.io.FileNotFoundException =>
        Files.write(Paths.get(outputPath), "Файл input.txt не найден".getBytes("UTF-8"))
        println("Ошибка: файл не найден")
        return
    }

    if (lines.isEmpty) {
      Files.write(Paths.get(outputPath), "Пустой файл".getBytes("UTF-8"))
      return
    }

    val n = lines.length - 1
    val graph = lines.take(n).map { line =>
      line.split("\\s+").map(_.toInt).toVector
    }

    val Array(start, end) = lines.last.split("\\s+").map(_.toInt)

    // Самый простой рекурсивный DFS
    def findPath(current: Int, target: Int, visited: Set[Int], path: List[Int]): Option[List[Int]] = {
      val newPath = current :: path
      if (current == target) {
        Some(newPath)
      } else {
        val newVisited = visited + current
        var found: Option[List[Int]] = None
        var j = 0
        while (j < graph(current).length && found.isEmpty) {
          if (graph(current)(j) == 1 && !newVisited.contains(j)) {
            findPath(j, target, newVisited, newPath) match {
              case some @ Some(_) => found = some
              case None => ()
            }
          }
          j += 1
        }
        found
      }
    }

    val result = findPath(start, end, Set.empty, Nil) match {
      case Some(p) => p.reverse.mkString(" -> ")
      case None => "Путь не найден"
    }

    Files.write(Paths.get(outputPath), result.getBytes("UTF-8"))
    println(s"Готово. Результат в $outputPath")
  }
}