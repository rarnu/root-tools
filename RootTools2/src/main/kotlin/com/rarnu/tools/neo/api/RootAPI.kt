package com.rarnu.tools.neo.api

import android.content.Context
import android.util.Log
import com.rarnu.tools.neo.utils.FileUtils
import com.rarnu.tools.neo.utils.RootUtils
import java.io.File
import java.text.DecimalFormat

/**
 * Created by rarnu on 11/21/16.
 */
object RootAPI {

    var rejected = false
    var systemRW = false

    private var _DU_CMD = ""

    fun mount(): Boolean {
        val cmd = "mount -o remount,rw /system"
        var b = true
        val ret = RootUtils.runCommand(cmd, true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        if (ret.error != "" && ret.error.toLowerCase().contains("denied")) {
            b = false
        }
        return b
    }

    // isSystemRW
    fun isSystemRW(): Boolean {
        val cmd = "mount"
        val ret = RootUtils.runCommand(cmd, true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        val sl = ret.result.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
        val b = sl.any { it.contains(" /system") && it.contains("ext4") && it.contains("rw") }
        return b
    }

    fun makePreferenceReadable(sdk: Int, packageName: String?) {
        // makePreferenceReadable
        if (sdk >= 24) {
            val cmd = "chmod -R 777 /data/data/$packageName/shared_prefs"
            val ret = RootUtils.runCommand(cmd, true)
            Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        }
    }

    fun freezeApplication(packageName: String?, isFreezed: Boolean): Boolean {
        // freezeApplication
        val cmd = "pm ${if (isFreezed) "disable" else "enable"} $packageName"
        val ret = RootUtils.runCommand(cmd, true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        return ret.error == ""
    }

    fun freezeComponent(packageName: String?, componentName: String?, isFreezed: Boolean): Boolean {
        // freezeComponent
        val cmd = "pm ${if (isFreezed) "disable" else "enable"} $packageName/$componentName"
        val ret = RootUtils.runCommand(cmd, true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        return ret.error == ""
    }

    fun freezeComponents(packageName: String?, componentNames: Array<String>?, isFreezed: Boolean): Boolean {
        // freezeComponents
        var b = true
        componentNames!!.indices.forEach {
            val cmd = "pm ${if (isFreezed) "disable" else "enable"} $packageName/${componentNames[it]}"
            val ret = RootUtils.runCommand(cmd, true)
            Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
            if (ret.error != "") {
                b = false
            }
        }
        return b
    }

    fun systemClean(ctx: Context?) {
        // systemClean
        var totalSize = 0L
        if (_DU_CMD == "") {
            _DU_CMD = duCmd
        }
        totalSize += cleanCache(ctx)
        totalSize += cleanANR(ctx)
        totalSize += cleanART(ctx)
        cleanCallback(ctx, DeviceAPI.STATUS_COMPLETE, "Total Cleaned: ${getReadableFileSize(totalSize)}")
    }

    fun writeFile(ctx: Context?, filePath: String?, text: String?, perm: Int): Boolean {
        // writeFile
        val CACHE = "/sdcard/.tmp/"
        var b = false
        val fCache = File(CACHE)
        if (!fCache.exists()) {
            fCache.mkdirs()
        }
        val tmpPath = CACHE + filePath?.substring(filePath.lastIndexOf("/") + 1)
        val tmpPathEx = filePath + ".tmp"
        try {
            FileUtils.rewriteFile(tmpPath, text)
        } catch (e: Exception) {

        }

        var modStr = perm.toString()
        while (modStr.length < 3) {
            modStr = "0" + modStr
        }
        val fTmp = File(tmpPath)
        if (fTmp.exists()) {
            var ret: RootUtils.CommandResult = RootUtils.runCommand("cp $tmpPath $tmpPathEx", true)
            Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
            if (ret.error == "") {
                ret = RootUtils.runCommand("chmod $modStr $tmpPathEx", true)
                Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
            }
            if (ret.error == "") {
                ret = RootUtils.runCommand("cp $tmpPathEx $filePath", true)
                Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
            }
            if (ret.error == "") {
                ret = RootUtils.runCommand("chmod $modStr $filePath", true)
                Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
            }
            b = ret.error == ""
        }
        return b
    }

    fun catFile(src: String?, dest: String?, perm: Int): Boolean {
        // catFile
        var modstr = perm.toString()
        while (modstr.length < 3) {
            modstr = "0" + modstr
        }
        var ret: RootUtils.CommandResult = RootUtils.runCommand("cat $src > $dest", true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        if (ret.error == "") {
            ret = RootUtils.runCommand("chmod $modstr $dest", true)
            Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        }
        return ret.error == ""
    }

    fun deleteFile(src: String?): Boolean {
        var ret: RootUtils.CommandResult = RootUtils.runCommand("rm $src", true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        return ret.error == ""
    }

    fun forceDeleteFile(path: String?) {
        // forceDeleteFile
        val cmd = "rm -f -r $path"
        val ret = RootUtils.runCommand(cmd, true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
    }

    fun forceDropCache() {
        // forceDropCache
        // RootUtils.CommandResult ret = RootUtils.runCommand("sync", true);
        var ret: RootUtils.CommandResult = RootUtils.runCommand("echo 3 > /proc/sys/vm/drop_caches", true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        ret = RootUtils.runCommand("echo 0 > /proc/sys/vm/drop_caches", true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
    }

    fun killProcess() {
        // killProcess
        var ret: RootUtils.CommandResult = RootUtils.runCommand("ps", true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        if (ret.error == "") {
            val slPid = arrayListOf<String>()
            val slPs = ret.result.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
            for (s in slPs) {
                if (s.startsWith("u0")) {
                    val pkgName = getPackageName(s)
                    Log.e("killProcess", "pkg: " + pkgName)
                    if (!pkgName.contains("core") && !pkgName.startsWith("android.") && !pkgName.contains("secure")) {
                        val pidstr = getProcessId(s)
                        Log.e("killProcess", "pid: " + pidstr)
                        if (pidstr != "") {
                            slPid.add(pidstr)
                        }
                    }
                }
            }
            for (s in slPid) {
                if (s.trim { it <= ' ' } != "") {
                    ret = RootUtils.runCommand("kill $s", true)
                    Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
                }
            }
        }
    }

    fun deleteSystemApp(pkgName: String?): Boolean {
        // deleteSystemApp
        val b = false
        var ret: RootUtils.CommandResult = RootUtils.runCommand("pm path $pkgName", true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        var outstr: String = ret.result
        if (ret.error != "" || outstr.trim { it <= ' ' } == "") {
            return b
        }
        outstr = outstr.replace("package:", "").trim { it <= ' ' }
        val apkPath = outstr
        val sc = getSlashCount(apkPath)
        if (sc == 4) {
            val parentDir = apkPath.substring(0, apkPath.lastIndexOf("/"))
            ret = RootUtils.runCommand("rm -r $parentDir", true)
            Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        } else if (sc == 3) {
            ret = RootUtils.runCommand("rm $apkPath", true)
            Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        }
        return ret.error == ""
    }

    fun isAppRequiredBySystem(pkgName: String?): Boolean {
        // isAppRequiredBySystem
        var b = false
        val ret = RootUtils.runCommand("pm path $pkgName", true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        var outstr: String = ret.result
        if (ret.error != "" || outstr.trim { it <= ' ' } == "") {
            return b
        }
        outstr = outstr.replace("package:", "").trim { it <= ' ' }
        val apkPath = outstr
        if (apkPath.contains("/data/app") || apkPath.contains("/data/priv-app")) {
            return b
        }
        if (pkgName == "android" || pkgName!!.startsWith("android.") || pkgName.startsWith("com.android.") || pkgName.startsWith("com.") && pkgName.contains(".android.")) {
            b = true
        }
        if (pkgName.endsWith(".core") || pkgName.endsWith(".securitycore")) {
            b = true
        }
        return b
    }

    private fun getSlashCount(path: String): Int = (0..path.length - 1).count { path[it] == '/' }

    private fun getProcessId(str: String): String {
        if (str.contains("com.rarnu.tools.neo") || str.endsWith(" su")) {
            return ""
        }
        var s = str.trim { it <= ' ' }
        var p = s.indexOf(" ")
        s = s.substring(p).trim { it <= ' ' }
        p = s.indexOf(" ")
        return s.substring(0, p).trim { it <= ' ' }
    }

    private fun getPackageName(str: String): String {
        val p = str.lastIndexOf(" ")
        return str.substring(p + 1).trim { it <= ' ' }
    }

    internal class CacheSize(var sizeReadable: String, var size: Long)

    private val duCmd: String
        get() {
            val BUSYBOX_PATH = arrayOf("/system/bin/busybox", "/system/xbin/busybox")
            val DU_PATH = arrayOf("/system/bin/du", "/system/xbin/du")
            val duExists = DU_PATH.any { File(it).exists() }
            if (duExists) {
                return "du"
            }
            val busyboxExists = BUSYBOX_PATH.any { File(it).exists() }
            if (busyboxExists) {
                return "busybox du"
            }
            return ""
        }

    private fun getCacheSize(path: String): CacheSize {
        val cmd = "$_DU_CMD -s -k \"$path\""
        val ret = RootUtils.runCommand(cmd, true)
        var sizeStr = "0"
        var size = 0L
        if (ret.error == "") {
            try {
                sizeStr = ret.result.substring(0, ret.result.indexOf("\t")).trim { it <= ' ' }
                size = Integer.parseInt(sizeStr).toLong()
            } catch (e: Exception) {
            }
        }
        return CacheSize(sizeStr, size)
    }

    private fun getReadableFileSize(size: Long): String {
        val UNITS = arrayOf("K", "M", "G", "T", "P")
        val MOOD = 1024.0
        var nSize = size * 1.0
        var i = 0
        while (nSize >= MOOD) {
            nSize /= MOOD
            i++
        }
        return DecimalFormat("0.##").format(nSize) + UNITS[i]
    }

    private fun deleteCache(path: String?): Boolean {
        val cmd = "rm -r \"$path\""
        val ret = RootUtils.runCommand(cmd, true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        return ret.error == ""
    }

    private fun deleteAnrLog(): Boolean {
        val CMD_DELETE_ANR = "rm -r /data/anr/*"
        val ret = RootUtils.runCommand(CMD_DELETE_ANR, true)
        Log.e("RootAPI", "result: ${ret.result}, error: ${ret.error}")
        return ret.error == ""
    }

    private fun isCachedAppInstalled(oriList: Array<String>, app: String): Boolean {
        if (app.trim { it == ' ' } == "" || app.startsWith("system") || app.startsWith("data@dalvik-cache")) {
            return true
        }
        var newAppPath = app.replace("data@app@", "")
        newAppPath = newAppPath.substring(0, newAppPath.indexOf("@"))

        val idx = oriList.indices.firstOrNull { oriList[it] == newAppPath } ?: -1
        return idx != -1
    }

    private fun isProfileInstalled(oriList: Array<String>, app: String): Boolean = oriList.any { it.contains(app) }

    private fun cleanANR(ctx: Context?): Long {
        var l = 0L
        val anrSize = getCacheSize("/data/anr/")
        if (deleteAnrLog()) {
            cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Clean ANR (${anrSize.sizeReadable})")
            l = anrSize.size
        }
        return l
    }

    private fun deleteRemainArtCache(ctx: Context?): Long {

        var ret: RootUtils.CommandResult = RootUtils.runCommand("ls /data/app", true)
        val listInstalled = ret.result.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
        ret = RootUtils.runCommand("pm list packages", true)
        val listAllInstalled = ret.result.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
        ret = RootUtils.runCommand("ls /data/dalvik-cache/arm", true)
        val listArm = ret.result.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
        ret = RootUtils.runCommand("ls /data/dalvik-cache/arm64", true)
        val listArm64 = ret.result.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
        ret = RootUtils.runCommand("ls /data/dalvik-cache/profiles", true)
        val listProfile = ret.result.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()

        var totalSize = 0L
        for (s in listArm) {
            if (s.trim { it <= ' ' } != "") {
                cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Scan $s")
                if (!isCachedAppInstalled(listInstalled, s)) {
                    val tmpPath = "/data/dalvik-cache/arm/" + s
                    val size = getCacheSize(tmpPath)
                    if (deleteCache(tmpPath)) {
                        cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Clean $s(${size.sizeReadable})")
                        totalSize += size.size
                    }
                }
            }
        }
        for (s in listArm64) {
            if (s.trim { it <= ' ' } != "") {
                cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Scan $s")
                if (!isCachedAppInstalled(listInstalled, s)) {
                    val tmpPath = "/data/dalvik-cache/arm64/" + s
                    val size = getCacheSize(tmpPath)
                    if (deleteCache(tmpPath)) {
                        cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Clean $s(${size.sizeReadable})")
                        totalSize += size.size
                    }
                }
            }
        }
        for (s in listProfile) {
            if (s.trim { it <= ' ' } != "") {
                cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Scan $s")
                if (!isProfileInstalled(listAllInstalled, s)) {
                    val tmpPath = "/data/dalvik-cache/profiles/" + s
                    val size = getCacheSize(tmpPath)
                    if (deleteCache(tmpPath)) {
                        cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Clean $s(${size.sizeReadable})")
                        totalSize += size.size
                    }
                }
            }
        }
        return totalSize
    }

    private fun cleanCache(ctx: Context?): Long {
        var totalSize = 0L
        val CMD_FIND_CACHE = "find /data/data/ -type dir -name \"cache\""
        val ret = RootUtils.runCommand(CMD_FIND_CACHE, true)
        if (ret.error != "") {
            cleanCallback(ctx, DeviceAPI.STATUS_ERROR, "Can not clean Cache")
            return 0
        }
        val items = ret.result.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
        for (s in items) {
            cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Scan $s")
            val cs = getCacheSize(s)
            if (cs.size > 16) {
                if (deleteCache(s)) {
                    cleanCallback(ctx, DeviceAPI.STATUS_PROGRESS, "Clean $s(${cs.sizeReadable})")
                    totalSize += cs.size
                }
            }
        }
        return totalSize
    }

    private fun cleanART(ctx: Context?): Long = deleteRemainArtCache(ctx)

    fun cleanCallback(ctx: Context?, status: Int, data: String?) = DeviceAPI.cleanCallback(ctx, status, data)

}
