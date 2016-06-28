package com.rarnu.mi8

import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Environment
import android.os.Handler
import android.os.StatFs
import android.util.Log
import java.io.*
import java.nio.charset.Charset
import java.text.DecimalFormat

/**
 * Created by rarnu on 3/25/16.
 */
object FileUtils {

    val S_IRU = 400
    val S_IRG = 40
    val S_IRO = 4
    val S_IWU = 200
    val S_IWG = 20
    val S_IWO = 2
    val S_IXU = 100
    val S_IXG = 10
    val S_IXO = 1
    val S_IRWU = S_IRU or S_IWU
    val S_IRWG = S_IRG or S_IWG
    val S_IRWO = S_IRO or S_IWO
    val S_IRXU = S_IRU or S_IXU
    val S_IRXG = S_IRG or S_IXG
    val S_IRXO = S_IRO or S_IXO
    val S_IRWXU = S_IRU or S_IWU or S_IXU
    val S_IRWXG = S_IRG or S_IWG or S_IXG
    val S_IRWXO = S_IRO or S_IWO or S_IXO

    fun mkdir(path: String): Boolean {
        var ret = false
        val d = File(path)
        if (!d.exists()) {
            ret = d.mkdirs()
        }
        return ret
    }

    fun rewriteFile(file: File?, text: String, encoding: String) = writeFileByStream(file, text, false, encoding)

    fun rewriteFile(file: File?, text: String) = writeFileByWriter(file, text, false)

    fun rewriteFile(path: String, text: String) = rewriteFile(File(path), text)

    fun rewriteFile(path: String, text: String, encoding: String) = rewriteFile(File(path), text, encoding)

    fun appendFile(file: File?, text: String) = writeFileByWriter(file, text, true)

    fun appendFile(file: File?, text: String, encoding: String) = writeFileByStream(file, text, true, encoding)

    private fun writeFileByWriter(file: File?, text: String, append: Boolean) {
        val writer = FileWriter(file, append)
        if (append) {
            writer.append(text)
        } else {
            writer.write(text)
        }
        writer.close()
    }

    private fun writeFileByStream(file: File?, text: String, append: Boolean, encoding: String) {
        val fos = FileOutputStream(file, append)
        val writer = OutputStreamWriter(fos, encoding)
        writer.write(text)
        writer.close()
        fos.close()
    }

    fun appendFile(path: String, text: String) = appendFile(File(path), text)

    fun appendFile(path: String, text: String, encoding: String) = appendFile(File(path), text, encoding)

    fun deleteFile(path: String): Boolean = File(path).delete()

    fun deleteDir(path: String): Boolean {
        deleteSubFiles(path)
        return File(path).delete()
    }

    fun deleteSubFiles(path: String) {
        val f = File(path)
        if (!f.exists()) {
            return
        }
        if (!f.isDirectory) {
            return
        }
        val tempList = f.list()
        var temp: File?
        for (t in tempList) {
            temp = File(path, t)
            if (temp.isFile) {
                temp.delete()
            }
            if (temp.isDirectory) {
                deleteSubFiles(path + File.separator + t)
                temp.delete()
            }
        }
    }

    fun copyFile(source: String, dest: String, hProgress: Handler?) {
        val oldFile = File(source)
        if (oldFile.exists()) {
            val ins = FileInputStream(source)
            val fos = FileOutputStream(dest)
            val size = ins.available()
            MessageUtils.sendMessage(hProgress, Actions.WHAT_COPY_START, 0, size)
            var count = 0
            var n: Int
            val buffer = ByteArray(1024)
            while (true) {
                n = ins.read(buffer)
                if (n != -1) {
                    fos.write(buffer, 0, n)
                    count += n
                    MessageUtils.sendMessage(hProgress, Actions.WHAT_COPY_PROGRESS, count, size)
                } else {
                    break
                }
            }
            ins.close()
            fos.close()
            MessageUtils.sendMessage(hProgress, Actions.WHAT_COPY_FINISH)
        }
    }

    fun copyFolder(source: String, dest: String) {
        File(dest).mkdirs()
        val a = File(source)
        val file = a.list()
        var temp: File?
        for (f in file) {
            temp = File(source, f)
            if (temp.isFile) {
                val input = FileInputStream(temp)
                val output = FileOutputStream(dest + File.separator + temp.name)
                val b = ByteArray(1024)
                var len: Int
                while (true) {
                    len = input.read(b)
                    if (len != -1) {
                        output.write(b, 0, len)
                    } else {
                        break
                    }
                }
                output.flush()
                output.close()
                input.close()
            }
            if (temp.isDirectory) {
                copyFolder(source + File.separator + f, dest + File.separator + f)
            }
        }
    }

    fun moveFile(source: String, dest: String, hProgress: Handler?) {
        copyFile(source, dest, hProgress)
        deleteFile(source)
    }

    fun moveFolder(source: String, dest: String) {
        copyFolder(source, dest)
        deleteDir(source)
    }

    fun renameFile(source: String, dest: String) {
        val fSource = File(source)
        val fDest = File(dest)
        fSource.renameTo(fDest)
    }

    fun readFile(file: File?): MutableList<String>? {
        val reader = FileReader(file)
        val bufReader = BufferedReader(reader)
        var line: String?
        val fileText = arrayListOf<String>()
        while (true) {
            line = bufReader.readLine()
            if (line != null) {
                fileText.add(line)
            } else {
                break
            }
        }
        bufReader.close()
        reader.close()
        return fileText
    }

    fun fileList2string(list: MutableList<String>?): String? {
        var ret = ""
        for (s in list!!) {
            ret += "$s\n"
        }
        return ret
    }

    fun readFile(path: String): MutableList<String>? = readFile(File(path))

    fun readAssetFile(context: Context, fileName: String): String? {
        val ins = context.assets.open(fileName)
        val buffer = ByteArray(1024)
        val output = ByteArrayOutputStream()
        var len: Int
        while (true) {
            len = ins.read(buffer)
            if (len != -1) {
                output.write(buffer, 0, len)
            } else {
                break
            }
        }
        ins.close()
        output.close()
        val text = String(output.toByteArray())
        return text.trim()
    }

    fun readAssertFileAsList(context: Context, fileName: String): MutableList<String>?  {
        val ins = context.assets.open(fileName)
        val reader = InputStreamReader(ins)
        val bufReader = BufferedReader(reader)
        var line: String?
        var fileText = arrayListOf<String>()
        while (true) {
            line = bufReader.readLine()
            if (line != null) {
                fileText.add(line)
            } else {
                break
            }
        }
        ins.close()
        bufReader.close()
        reader.close()
        return fileText
    }

    fun copyAssetFile(context: Context, fileName: String, saveDir: String, hProgress: Handler?): Boolean {
        val fAsset = File(saveDir)
        if (!fAsset.exists()) {
            fAsset.mkdirs()
        }
        try {
            val buffer = ByteArray(1024)
            val dest = File(saveDir, fileName)
            if (dest.exists()) {
                dest.delete()
            }
            val ins = context.assets.open(fileName)
            val fos = BufferedOutputStream(FileOutputStream(dest))
            var count = 0
            val size = ins.available()
            MessageUtils.sendMessage(hProgress, Actions.WHAT_COPY_START, 0, size)
            var n: Int
            while (true) {
                n = ins.read(buffer, 0, buffer.size)
                if (n != -1) {
                    fos.write(buffer, 0, n)
                    count += n
                    MessageUtils.sendMessage(hProgress, Actions.WHAT_COPY_PROGRESS, count, size)
                } else {
                    break
                }
            }
            ins.close()
            fos.close()
            MessageUtils.sendMessage(hProgress, Actions.WHAT_COPY_FINISH)
            return true
        } catch (e: Exception) {
            return false
        }
    }

    fun readInnerFile(context: Context, path: String): String? = readFileStream(context.openFileInput(path))

    fun readFileString(path: String): String? = readFileStream(FileInputStream(path))

    fun readFileString(path: String, encoding: String): String? = String(readFileString(path)!!.toByteArray(), Charset.forName(encoding))

    private fun readFileStream(ins: InputStream?): String? {
        val buffer = ByteArray(1024)
        val output = ByteArrayOutputStream()
        var len: Int
        while (true) {
            len = ins!!.read(buffer)
            if (len != -1) {
                output.write(buffer, 0, len)
            } else {
                break
            }
        }
        ins.close()
        output.close()
        val text = String(output.toByteArray())
        return text.trim()
    }

    fun getDirSize(path: String): Long = getDirSize(File(path))

    fun getDirSize(dir: File?): Long {
        if (dir == null) {
            return 0
        }
        if (!dir.isDirectory) {
            return 0
        }
        var dirSize = 0L
        val files = dir.listFiles()
        for (file in files) {
            if (file.isFile) {
                dirSize += file.length()
            } else if (file.isDirectory) {
                dirSize += file.length()
                dirSize += getDirSize(file)
            }
        }
        return dirSize
    }


    fun getFileSize(path: String): Long = File(path).length()

    fun getReadableFileSize(size: Long): String {
        val units = arrayOf("B", "KB", "MB", "GB", "TB", "PB")
        var nSize = size * 1L * 1.0F
        val mod = 1024.0F
        var i = 0
        while (nSize >= mod) {
            nSize /= mod
            i++
        }
        val df = DecimalFormat(".00")
        return "${df.format(nSize)} ${units[i]}"
    }

    @Suppress("DEPRECATION")
    fun getTotalDataSzie(): Long {
        val path = Environment.getDataDirectory()
        val stat = StatFs(path.path)
        val blockSize: Long
        val blockCount: Long
        if (Build.VERSION.SDK_INT >= 21) {
            blockSize = stat.blockSizeLong
            blockCount = stat.blockCountLong
        } else {
            blockSize = stat.blockSize.toLong()
            blockCount = stat.blockCount.toLong()
        }
        return Math.abs(blockSize * blockCount)
    }

    @Suppress("DEPRECATION")
    fun getAvailableDataSize(): Long {
        val path = Environment.getDataDirectory()
        val stat = StatFs(path.path)
        val blockSize: Long
        val blocks: Long
        if (Build.VERSION.SDK_INT >= 21) {
            blockSize = stat.blockSizeLong
            blocks = stat.availableBlocksLong
        } else {
            blockSize = stat.blockSize.toLong()
            blocks = stat.availableBlocks.toLong()
        }
        return Math.abs(blocks * blockSize)
    }

    @Suppress("DEPRECATION")
    fun getTotalSdcardSize(): Long {
        val path = Environment.getExternalStorageDirectory()
        val stat = StatFs(path.path)
        val blockSize: Long
        val totalBlocks: Long
        if (Build.VERSION.SDK_INT >= 21) {
            blockSize = stat.blockSizeLong
            totalBlocks = stat.blockCountLong
        } else {
            blockSize = stat.blockSize.toLong()
            totalBlocks = stat.blockCount.toLong()
        }
        return Math.abs(blockSize * totalBlocks)
    }

    @Suppress("DEPRECATION")
    fun getAvailableSdcardSize(): Long {
        val path = Environment.getExternalStorageDirectory()
        val stat = StatFs(path.path)
        val blockSize: Long
        val availableBlocks: Long
        if (Build.VERSION.SDK_INT >= 21) {
            blockSize = stat.blockSizeLong
            availableBlocks = stat.availableBlocksLong
        } else {
            blockSize = stat.blockSize.toLong()
            availableBlocks = stat.availableBlocks.toLong()
        }
        return Math.abs(blockSize * availableBlocks)
    }

    fun getPackageSize(context: Context): Long {
        var size = 0L
        try {
            val info = context.packageManager.getApplicationInfo(context.packageName, 0)
            val dir = info.dataDir
            size = getDirSize(File(dir))
        } catch(e: Exception) {

        }
        return Math.abs(size)
    }

    fun loadListFromFile(path: String): MutableList<*>? = loadObjetFromFile(path) as MutableList<*>

    fun saveListToFile(list: MutableList<*>, path: String) = saveObjectToFile(list, path)

    fun loadObjetFromFile(path: String): Any? {
        var obj: Any? = null
        try {
            val input = FileInputStream(path)
            val ois = ObjectInputStream(input)
            obj = ois.readObject()
            ois.close()
            input.close()
        } catch (e: Exception) {

        }
        return obj
    }

    fun saveObjectToFile(obj: Any?, path: String) {
        try {
            val output = FileOutputStream(path)
            val oos = ObjectOutputStream(output)
            oos.writeObject(obj)
            oos.close()
            output.close()
        } catch (e: Exception) {

        }
    }

    fun setPermission(filePath: String, permission: Int) {
        try {
            var cls = Class.forName("android.os.FileUtils")
            var method = cls.getDeclaredMethod("setPermissions", String::class.java, Integer.TYPE, Integer.TYPE, Integer.TYPE)
            var ret = method.invoke(null, filePath, permission, -1, -1)
            Log.e("LOG", "setPermission => $ret")
        } catch(e: Exception) {
            Log.e("LOG", "setPermission => ${e.message}")
        }
    }

    fun sendScanCmd(context: Context, path: String) {
        context.sendBroadcast(Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, Uri.parse("file://$path")))
    }

}