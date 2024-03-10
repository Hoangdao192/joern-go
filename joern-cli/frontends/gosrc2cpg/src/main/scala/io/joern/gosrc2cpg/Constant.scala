package io.joern.gosrc2cpg

object Constant {

    val PRIMITIVE_TYPES: Array[String] = Array(
        //  Number
        "uint8", "uint16", "uint32", "uint64", "int8", "int16", "int32", "int64",
        "uint", "int", "uintptr",
        "float32", "float64",
        "complex64", "complex128",
        "byte",
        "rune",
        //  String
        "string",
        //  Boolean
        "bool"
    )

}
