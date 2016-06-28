package com.rarnu.mi8

import android.content.Context
import java.io.File
import kotlin.concurrent.thread

/**
 * Created by rarnu on 6/26/16.
 */
object UserUtils {

    val zeroId = "0"
    val zeroPath = "/storage/emulated/0/"

    fun getUserIdList(): MutableList<String> {
        val ret = arrayListOf<String>()
        val fUser = File("/data/system/users")
        for (f in fUser.list()) {
            val fn = f.replace(".xml", "", true)
            if (ret.indexOf(fn) == -1 && fn != "userlist" && fn != zeroId) {
                ret.add(fn)
            }
        }
        return ret
    }

    fun getUsersPath(users: MutableList<String>): MutableList<String> {
        val ret = arrayListOf<String>()
        for (u in users) {
            val path = "/storage/emulated/$u/"
            if (ret.indexOf(path) == -1 && path != zeroPath) {
                ret.add(path)
            }
        }
        return ret
    }

    fun syncToZero(context: Context, paths: MutableList<String>) {
        // manual sync
        thread {
            if (Config.isDoubleWeiXin(context)) {
                for (path in paths) {
                    for (wx in PathDefine.WEIXIN_PATH) {
                        // do NOT sync 'MicroMsg'
                        if (wx != PathDefine.WEIXIN_PATH[0]) {
                            val dest = "$zeroPath$wx"
                            FileUtils.copyFolder("$path$wx", dest)
                            FileUtils.sendScanCmd(context, dest)
                        }
                    }
                }
            }
            if (Config.isDoubleQQ(context)) {
                for (path in paths) {
                    for (qq in PathDefine.QQ_PATH) {
                        val dest = "$zeroPath$qq"
                        FileUtils.copyFolder("$path$qq", dest)
                        FileUtils.sendScanCmd(context, dest)
                    }
                }
            }
        }
    }

}