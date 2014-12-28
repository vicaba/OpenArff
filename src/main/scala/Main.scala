import java.io.{File, FileOutputStream}

import com.arff.{Data, Relation, Attribute, ArffWriter}

/**
 * Created by Work on 26/12/14.
 */
object Main {

  def main(args: Array[String]) {
    val writer = ArffWriter(new FileOutputStream(new File("./assets/test.arff")))
    writer.add(Relation("student_mark"))
    writer.add(Attribute.Real("mark"))
    writer.add(Attribute.Nominal("student", List("ls25637", "ls25648")))
    writer.add(Data(List("9", "ls25637")))
    writer.flush()
    writer.close()

  }

}
