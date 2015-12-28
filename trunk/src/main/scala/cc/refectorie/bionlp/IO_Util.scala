package cc.refectorie.bionlp

import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}
import cc.factorie.{Template, DotStatistics1, Model}

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */


object IO_Util {
  def saveModel(filename:String, model:Model) {
    System.err.println("Writing model to "+filename)
    val fos = new FileOutputStream(filename)
    val gzos = new GZIPOutputStream(fos)
    val oos = new ObjectOutputStream(gzos)
    oos.writeObject(model)
    oos.flush; oos.close
    gzos.flush; gzos.close
    fos.flush; fos.close
  }
  def loadModel(filename:String) : Model = {
    System.err.println("Loading model from "+filename)
    val fis = new FileInputStream(filename)
    val gzis = new GZIPInputStream(fis)
    val ois = new ObjectInputStream(gzis)
    val o = ois.readObject()
    ois.close
    gzis.close
    fis.close
    val model = o.asInstanceOf[Model]
    model.foreach((x) => x.asInstanceOf[Template with DotStatistics1[Event]].init)
    model
  }

    def saveObject(filename:String, model:Object) {
    System.err.println("Writing model to "+filename)
    val fos = new FileOutputStream(filename)
    val gzos = new GZIPOutputStream(fos)
    val oos = new ObjectOutputStream(gzos)
    oos.writeObject(model)
    oos.flush; oos.close
    gzos.flush; gzos.close
    fos.flush; fos.close
  }
  def loadObject(filename:String) : Object = {
    System.err.println("Loading model from "+filename)
    val fis = new FileInputStream(filename)
    val gzis = new GZIPInputStream(fis)
    val ois = new ObjectInputStream(gzis)
    val o = ois.readObject()
    ois.close
    gzis.close
    fis.close
    o
  }
}