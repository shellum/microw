package com.finalhack.microw

import java.io.FileOutputStream

object BytecodeGenerator {
  val referenceFile = "reverseEngineering/C.class"
  val outFile = "testOutput/C.class"
  val out = new FileOutputStream(outFile)

  def main(args: Array[String]): Unit = {
    writeSimpleSpecBytecode()
    compareFiles(referenceFile, outFile)
  }

  val CONSTANT_UTF8 = Array(0x01,0x00)
  val CONSTANT_MAGIC = Array(0xca,0xfe,0xba,0xbe)
  val CONSTANT_JAVA6 = Array(0x00,0x00,0x00,0x32)
  val CONSTANT_EMPTYARRAY_2 = Array(0x00,0x00)
  def CONSTANT_POOLSIZE(size: Int): Array[Int] = Array(size & 0xff00,size & 0xff)
  def CONSTANT_NAMEANDTYPE(nameIndex: Int, typeIndex: Int) = Array(0x0c) ++ CONSTANT_POINTER_2(nameIndex) ++ CONSTANT_POINTER_2(typeIndex)
  def CONSTANT_FIELDREF(classIndex: Int, nameTypeIndex: Int) = Array(0x09) ++ CONSTANT_POINTER_2(classIndex) ++ CONSTANT_POINTER_2(nameTypeIndex)
  def CONSTANT_METHODREF(classIndex: Int, nameIndex: Int) = Array(0x0a) ++ CONSTANT_POINTER_2(classIndex) ++ CONSTANT_POINTER_2(nameIndex)
  def CONSTANT_CLASSREF(nameIndex: Int): Array[Int] = Array(0x07) ++ CONSTANT_POINTER_2(nameIndex)
  def CONSTANT_NUMBER_2(number: Int): Array[Int] = Array(number & 0xff00,number & 0xff)
  def CONSTANT_COUNT_2(count: Int): Array[Int] = Array(count & 0xff00,count & 0xff)
  def CONSTANT_POINTER_2(index: Int): Array[Int] = Array(index & 0xff00,index & 0xff)
  def CONSTANT_BYTELENGTH_4(number: Int): Array[Int] = Array(number & 0xff000000,number & 0xff0000,number & 0xff00,number & 0xff)

  def writeSimpleSpecBytecode() = {
    write(CONSTANT_MAGIC)           // Magic
    write(CONSTANT_JAVA6)           // Bytecode version (50 -> 6)
    writeSimpleSpecConstantPool()     // Constant Count & Pool
    write(CONSTANT_NUMBER_2(0x21))    // Access Flags
    write(CONSTANT_POINTER_2(4))      // this class
    write(CONSTANT_POINTER_2(5))      // super class
    write(CONSTANT_COUNT_2(0))        // interface count
    write(CONSTANT_EMPTYARRAY_2)      // interface array
    write(CONSTANT_COUNT_2(2))        // field count
    // Field 1
    write(CONSTANT_NUMBER_2(1))        // Access Flags
    write(CONSTANT_POINTER_2(6))       // Field Name
    write(CONSTANT_POINTER_2(7))       // Field Descriptor
    write(CONSTANT_COUNT_2(1))         // Attributes Count
    // Attribute for Field 1
    write(CONSTANT_POINTER_2(8))           // Attribute Name index
    write(CONSTANT_BYTELENGTH_4(0x1d))     // Attribute length
    write(CONSTANT_NUMBER_2(1))            // Max stack
    write(CONSTANT_NUMBER_2(1))            // Max locals
    write(CONSTANT_BYTELENGTH_4(0x05))     // Code length
    write(Array(0x2a,0xb7,0x00,0x01,0xb1)) // Code
    write(CONSTANT_COUNT_2(0))             // Exception table length
    write(CONSTANT_COUNT_2(1))             // Attribute count
    write(CONSTANT_POINTER_2(9))           // LineNumberTable
    write(CONSTANT_BYTELENGTH_4(0x06))     // LineNumberTable Length
    write(CONSTANT_COUNT_2(1))             // lineNumberTable Count
    write(CONSTANT_NUMBER_2(0))            // Start PC
    write(CONSTANT_NUMBER_2(1))            // Line Number

    // Field 2
    write(CONSTANT_NUMBER_2(9))     // Access Flags
    write(CONSTANT_POINTER_2(0x0a)) // Field Name
    write(CONSTANT_POINTER_2(0x0b)) // Field Descriptor
    write(CONSTANT_COUNT_2(1))      // Attributes Count
    //Attribute for Field 2
    write(CONSTANT_POINTER_2(8))        // Attribute Name Index
    write(CONSTANT_BYTELENGTH_4(0x2b))  // Attribute Length
    write(CONSTANT_NUMBER_2(2))         // Max Stack
    write(CONSTANT_NUMBER_2(2))         // Max Locals
    write(CONSTANT_BYTELENGTH_4(0x0b))  // Code Length
    write(Array(0x10,0x08,0x3c,0xb2,0x00,0x02,0x1b,0xb6,0x00,0x03,0xb1)) //Code
    write(CONSTANT_COUNT_2(0))          // Exception Table Length
    write(CONSTANT_COUNT_2(1))          // Code Attribute Count
    write(CONSTANT_POINTER_2(9))        // LineNumberTable
    write(CONSTANT_BYTELENGTH_4(0x0e))  // LineNumberTable Length
    write(CONSTANT_COUNT_2(0x03))       // Attribute Table Size
    write(CONSTANT_NUMBER_2(0))         // Start PC
    write(CONSTANT_NUMBER_2(3))         // Line Number
    write(CONSTANT_NUMBER_2(3))         // Start PC
    write(CONSTANT_NUMBER_2(4))         // Line Number
    write(CONSTANT_NUMBER_2(10))        // Start PC
    write(CONSTANT_NUMBER_2(5))         // Line Number

    write(Array(0x00,0x01,              // Method Count
      0x00,0x0c,0x00,0x00,0x00,0x02,0x00,0x0d)) // ???
    out.close()
  }

  def writeSimpleSpecConstantPool() = {
    write(CONSTANT_POOLSIZE(27))      // Constant Pool Size
    write(CONSTANT_METHODREF(5,14))   //(1)
    write(CONSTANT_FIELDREF(15,16))   //(2)
    write(CONSTANT_METHODREF(17,18))  //(3)
    write(CONSTANT_CLASSREF(19))      //(4)
    write(CONSTANT_CLASSREF(20))      //(5)
    write(CONSTANT_UTF8,"<init>")     //(6)
    write(CONSTANT_UTF8,"()V")        //(7)
    write(CONSTANT_UTF8,"Code")       //(8)
    write(CONSTANT_UTF8,"LineNumberTable") //(9)
    write(CONSTANT_UTF8,"main")            //(10)
    write(CONSTANT_UTF8,"([Ljava/lang/String;)V") //(11)
    write(CONSTANT_UTF8,"SourceFile")   //(12)
    write(CONSTANT_UTF8,"C.java")       //(13)
    write(CONSTANT_NAMEANDTYPE(6,7))    //(14)
    write(CONSTANT_CLASSREF(21))        //(15)
    write(CONSTANT_NAMEANDTYPE(22,23))  //(16)
    write(CONSTANT_CLASSREF(24))        //(17)
    write(CONSTANT_NAMEANDTYPE(25,26))  //(18)
    write(CONSTANT_UTF8,"C")                //(19)
    write(CONSTANT_UTF8,"java/lang/Object") //(20)
    write(CONSTANT_UTF8,"java/lang/System") //(21)
    write(CONSTANT_UTF8,"out")              //(22)
    write(CONSTANT_UTF8,"Ljava/io/PrintStream;")  //(23)
    write(CONSTANT_UTF8,"java/io/PrintStream")    //(24)
    write(CONSTANT_UTF8,"println")                //(25)
    write(CONSTANT_UTF8,"(I)V")                   //(26)
  }

  def write(data: Array[Int]): Unit = {
    for(part <- data)
      out.write(part & 0xff)
  }
  def write(data: Array[Int], str: String): Unit = {
    for(part <- data)
      out.write(part & 0xff)
    out.write(str.length.toByte)
    out.write(str.toArray.map(_.toByte))
  }

  def compareFiles(masterFile: String, testFile: String) = {
    val masterArray = scala.io.Source.fromFile(masterFile, "ISO-8859-1").map(_.toByte).toArray
    val testArray = scala.io.Source.fromFile(testFile,"ISO-8859-1").map(_.toByte).toArray
    var counter = 0
    for(x <- masterArray) {
      val masterByte = masterArray(counter)
      val testByte = testArray(counter)
      if (masterByte != testByte) {
        println("Mismatch at byte #"+counter+" "+masterByte+" vs "+testByte)
        System.exit(1)
      }
      if (masterByte.toChar >=32 && masterByte.toChar <= 126)
      print(masterByte.toChar)
      else
      print(".")
      counter += 1
      if (counter % 12 == 0) println()
    }
  }

}
