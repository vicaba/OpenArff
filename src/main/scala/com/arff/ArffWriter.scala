package com.arff

import java.io.OutputStream

import scala.collection.mutable

trait AbstractAttribute {
  val name: String
}

case object Attribute {

  val attributeDef = "@attribute"

  case class Real(override val name: String) extends AbstractAttribute {
    val _type = "real"
  }

  case class Nominal(override val name: String, values: List[String]) extends AbstractAttribute

}

trait AbstractRelation {
  val name: String
}

case object Relation {

  val attributeDef = "@relation"

  def apply(name: String) = new Relation(name)

  protected case class Relation(override val name: String) extends AbstractRelation

}

trait AbstractData {
  val values: collection.Seq[String]
}

case object Data {

  val attributeDef = "@data"

  def apply(values: collection.Seq[String]) = new Data(values)

  protected case class Data(override val values: collection.Seq[String]) extends AbstractData

}

case class ArffWriter(outStream: OutputStream) {

  protected val definedAttributes = new mutable.ArrayBuffer[(AbstractAttribute)]

  protected val definedRelations = new mutable.ArrayBuffer[AbstractRelation]

  protected val definedData = new mutable.ArrayBuffer[AbstractData]

  def add(data: AbstractData) = definedData += data

  def add(relation: AbstractRelation) = definedRelations += relation

  def add(attr: AbstractAttribute) = definedAttributes += attr

  def close() = outStream.close()

  def flush() = {
    definedRelations.foreach {
      relation =>
        outStream.write((Relation.attributeDef + " " + relation.name + "\n").getBytes)
    }

    definedAttributes.foreach {
      case attribute =>
        attribute match {
          case attr: Attribute.Real =>
            outStream.write((Attribute.attributeDef + " " + attr.name + " " + attr._type + "\n").getBytes)
          case attr: Attribute.Nominal =>
            outStream.write((Attribute.attributeDef + " " + attr.name + " { ").getBytes)
            attr.values.zipWithIndex.foreach {
              case (value, index) =>
                index match {
                  case i if i == (attr.values.size - 1) =>
                    outStream.write(("'" + value + "'").getBytes)
                  case _ =>
                    outStream.write(("'" + value + "', ").getBytes)
                }
            }
            outStream.write((" } \n").getBytes)
        }
    }

    def getDataAccordingAttributeType(value: String, index: Int): String = {
      definedAttributes(index) match {
        case Attribute.Real(_) => value
        case Attribute.Nominal(_, _) => "'" + value + "'"
      }
    }

    outStream.write((Data.attributeDef + "\n").getBytes)

    definedData.foreach {
      data => data.values.zipWithIndex.foreach {
        case (value, index) =>
          index match {
            case i if i == (data.values.size - 1) =>
              outStream.write((getDataAccordingAttributeType(value, index)).getBytes)
            case _ =>
              outStream.write(("" + getDataAccordingAttributeType(value, index) + ", ").getBytes)
          }
      }

        outStream.write(("\n").getBytes)

    }

    outStream.flush()

  }
}

