package io.joern.gosrc2cpg

object Constant {

    object PrimitiveTypes {
        type PrimitiveTypes = String
        //  Unsigned integer
        val UINT = "uint"
        val UINT8 = "uint8"
        val UINT16 = "uint16"
        val UINT32 = "uint32"
        //  Signed integer
        val INT8 = "int8"
        val INT16 = "int16"
        val INT = "int"
        val INT32 = "int32"
        val INT64 = "int64"
        //  Floating point
        val FLOAT32 = "float32"
        val FLOAT64 = "float64"
        //  Complex
        val COMPLEX64 = "complex64"
        val COMPLEX128 = "complex128"
        //  Other
        val BYTE = "byte"
        val RUNE = "rune"
        val STRING = "string"
        val BOOLEAN = "bool"
    }

    val PRIMITIVE_TYPES: Array[String] = Array(
        PrimitiveTypes.UINT, PrimitiveTypes.UINT8, PrimitiveTypes.UINT16, PrimitiveTypes.UINT32,
        PrimitiveTypes.INT, PrimitiveTypes.INT8, PrimitiveTypes.INT16,
        PrimitiveTypes.INT32, PrimitiveTypes.INT64,

        PrimitiveTypes.FLOAT32, PrimitiveTypes.FLOAT64,

        PrimitiveTypes.COMPLEX64, PrimitiveTypes.COMPLEX128,

        PrimitiveTypes.BYTE, PrimitiveTypes.RUNE,
        PrimitiveTypes.STRING, PrimitiveTypes.BOOLEAN
    )

}
