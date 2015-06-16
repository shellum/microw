package com.finalhack.microw

import java.io.FileOutputStream

import scala.collection.mutable

object BytecodeGenerator {
  val referenceFile = "reverseEngineering/C.class"
  val outFile = "testOutput/C.class"
  val out = new FileOutputStream(outFile)
  var nextConstantPoolIndex = 1

  var allCodeForAllMethods: Array[Int] = Array()

  def main(args: Array[String]): Unit = {
    val testCode =
      """
        |main -> 3*4.
      """.stripMargin

    writeSimpleSpecBytecode()

    val codeArray: Array[Int] = compile(testCode)
writeMoreBytecode()
    finishSimpleSpecBytecode()

    compareFiles(referenceFile, outFile)
  }

  def compile(codeToCompile: String): Array[Int] = {
    val lexer = new Lexer {
      override val input = new Input {
        override val code = codeToCompile
      }
    }

    val tokens = lexer.getAllTokens()

    val parser = new Parser()
    parser.setTokens(tokens)
    while (parser.hasMoreTokens) parser.expr

    val stack = parser.parseTree.makeStack

    generateOpCodes(stack)
  }

  def writeConstantPoolEntriesForMethod(name: String, params: String): (Int,Int) = {
    write(CONSTANT_UTF8, name)
    write(CONSTANT_UTF8, params)
    nextConstantPoolIndex += 2
    (nextConstantPoolIndex - 2, nextConstantPoolIndex - 1)
  }

  def generateOpCodes(stack: mutable.Stack[AstNode]): Array[Int] = {
    var codeArray = Array[Int]()
    var methodStartIndex = 0

    val REGEX_NUMBER = "[0-9]+".r

    // TODO: Move me to a more sutible location (this method write for <init>)
    allCodeForAllMethods = allCodeForAllMethods ++ writeMethod(FLAGS_PUBLIC, 8, 9, 3,Array(0x2a,0xb7,0x00,0x04,0xb1),Array())

    var previousElement = AstNode()

    for (element <- stack) {
      element.value.value match {
        case "." => methodStartIndex = codeArray.length
        case "->" =>

          val poolEntries = writeConstantPoolEntriesForMethod(previousElement.value.value, "([Ljava/lang/String;)V")
          allCodeForAllMethods = allCodeForAllMethods ++ writeMethod(FLAGS_PUBLIC|FLAGS_STATIC, poolEntries._1, poolEntries._2, 3,
            codeArray.slice(methodStartIndex,codeArray.length), // Inject compiled code
            Array(  0x3c, // Push int to stack, save to local 1
              0xb2,0x00,10, // Initialize System.out
              0x1b, // Load local 1 from stack
              0xb6,0x00,16, // Invoke println
              0xb1) // Return void
          )

        case "-" => codeArray = codeArray :+ 0x64
        case "*" => codeArray = codeArray :+ 0x68
        case "+" => codeArray = codeArray :+ 0x60
        case "" =>
        case REGEX_NUMBER() => codeArray = codeArray :+ 0x10 :+ element.value.value.toInt
        case _ =>
      }
      previousElement = element
    }
    codeArray
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

  def FLAGS_PUBLIC = 1
  def FLAGS_PRIVATE = 2
  def FLAGS_PROTECTED = 4
  def FLAGS_STATIC = 8

  def writeMethod(accessFlags: Int, methodName: Int, methodDescriptor: Int, codePointer: Int, customCodeArray: Array[Int], forcedCodeArray: Array[Int]): Array[Int] = {
    val bytesInCodeAttributeWithoutCode = 12
    val codeLength = customCodeArray.size + forcedCodeArray.size

    val maxStack = customCodeArray.filter(_==0x10).length + 1 // <init> seems to be "special"
    val maxLocals = customCodeArray.filter(Array(0x2a,0x2b,0x2c,0x2d,0x3b,0x3c,0x3d,0x3e).contains(_)).length + 5 // TODO: how to count frame pointer stack entries (Args passed)? Assume 5 for now

    var methodArray: Array[Int] = Array()
    methodArray = methodArray ++ CONSTANT_NUMBER_2(accessFlags)
    methodArray = methodArray ++ CONSTANT_POINTER_2(methodName)
    methodArray = methodArray ++ CONSTANT_POINTER_2(methodDescriptor)
    methodArray = methodArray ++ CONSTANT_COUNT_2(1) // One method attribute
    methodArray = methodArray ++ CONSTANT_POINTER_2(codePointer)
    methodArray = methodArray ++ CONSTANT_BYTELENGTH_4(codeLength + bytesInCodeAttributeWithoutCode) // Attribute Length
    methodArray = methodArray ++ CONSTANT_NUMBER_2(maxStack) // Max stack
    methodArray = methodArray ++ CONSTANT_NUMBER_2(maxLocals) // Max locals
    methodArray = methodArray ++ CONSTANT_BYTELENGTH_4(codeLength)
    methodArray = methodArray ++ customCodeArray
    methodArray = methodArray ++ forcedCodeArray
    methodArray = methodArray ++ CONSTANT_COUNT_2(0) // Exception table length
    methodArray = methodArray ++ CONSTANT_COUNT_2(0) // Attribute count

    methodArray
  }



  def writeSimpleSpecBytecode() = {
    write(CONSTANT_MAGIC) // Magic
    write(CONSTANT_JAVA6) // Bytecode version (50 -> 6)
    writeSimpleSpecConstantPool() // Constant Count & Pool
  }

  def writeMoreBytecode() = {
    write(CONSTANT_NUMBER_2(0x21))    // Access Flags
    write(CONSTANT_POINTER_2(22))      // this class
    write(CONSTANT_POINTER_2(5))      // super class
    write(CONSTANT_COUNT_2(0))        // interface count
    write(CONSTANT_COUNT_2(0))      // field count
    write(CONSTANT_COUNT_2(2))        // method count

  }

  def finishSimpleSpecBytecode() = {
    write(allCodeForAllMethods)

    write(Array(0x00,0x01,              // Attributes Count
      0x00,0x01,                        // Attribute 1: Source File
      0x00,0x00,0x00,0x02,              // Attribute Length
      0x00,0x02))                       // Pointer to Source File Value
    out.close()

  }

//TODO: need to build metadata table with ast that counts methods
 //TODO:  need to add methods to cfg
  def writeSimpleSpecConstantPool() = {
    write(CONSTANT_POOLSIZE(26))      // Constant Pool Size
    write(CONSTANT_UTF8,"SourceFile") //(1)
    write(CONSTANT_UTF8,"C.java")     //(2)
    write(CONSTANT_UTF8,"Code")       //(3)

    nextConstantPoolIndex = 4
    addStaticMethod("java/lang/Object", "<init>", "()V")

    addStaticField("java/lang/System", "out", "Ljava/io/PrintStream;")

    addStaticMethod("java/io/PrintStream", "println", "(I)V")

    write(CONSTANT_CLASSREF(23))                  //(22)
    write(CONSTANT_UTF8,"C")                      //(23)

    nextConstantPoolIndex += 2



  }

  def addStaticMethod(className: String, name: String, `type`: String): Int = {
    //Method
    val base = nextConstantPoolIndex
    write(CONSTANT_METHODREF(base+1,base+3))      //(base)
    write(CONSTANT_CLASSREF(base+2))              //(base+1)
    write(CONSTANT_UTF8, className)    //(base+2)
    write(CONSTANT_NAMEANDTYPE(base+4,base+5))    //(base+3)
    write(CONSTANT_UTF8, name)                //(base+4)
    write(CONSTANT_UTF8, `type`)                   //(base+5)
    nextConstantPoolIndex += 6
    base
  }

  def addStaticField(className: String, name: String, `type`: String): Int = {
    //Field
    val base = nextConstantPoolIndex
    write(CONSTANT_FIELDREF(base+1,base+3))   //(base)
    write(CONSTANT_CLASSREF(base+2))      //(base+1)
    write(CONSTANT_UTF8, className) //(base+2)
    write(CONSTANT_NAMEANDTYPE(base+4,base+5))      //(base+3)
    write(CONSTANT_UTF8, name)              //(base+4)
    write(CONSTANT_UTF8, `type`)  //(base+5)
    nextConstantPoolIndex += 6
    base
  }

  def getAndIncrementConstantPoolIndex: Int = {
    nextConstantPoolIndex += 1
    nextConstantPoolIndex - 1
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
