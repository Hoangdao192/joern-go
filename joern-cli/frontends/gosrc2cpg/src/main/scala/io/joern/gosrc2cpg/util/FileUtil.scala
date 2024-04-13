package io.joern.gosrc2cpg.util

import org.apache.commons.compress.archivers.ArchiveEntry

import java.io.{BufferedInputStream, File, FileInputStream}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import java.nio.file.{Files, Path}

object FileUtil {

    def extractTarGz(file: File, dest: File): Unit = {
        val bufferedInputStream = new BufferedInputStream(FileInputStream(file))
        val tarInputStream = TarArchiveInputStream(
            GzipCompressorInputStream(bufferedInputStream)
        )
        var entry: ArchiveEntry = tarInputStream.getNextEntry
        while (entry != null) {
            val extractTo = dest.toPath.resolve(entry.getName)
            if (entry.isDirectory) {
                Files.createDirectories(extractTo)
            } else {
                Files.copy(tarInputStream, extractTo)
            }
            entry = tarInputStream.getNextEntry
        }
    }

}
