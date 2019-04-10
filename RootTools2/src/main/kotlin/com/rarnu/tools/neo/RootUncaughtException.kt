@file:Suppress("unused")

package com.rarnu.tools.neo

import android.content.Context
import android.os.Looper
import com.rarnu.kt.android.resStr
import com.rarnu.kt.android.toast
import java.io.PrintWriter
import java.io.StringWriter
import java.io.Writer
import kotlin.concurrent.thread

/**
 * Created by rarnu on 12/5/16.
 */
class RootUncaughtException(private var ctx: Context) : Thread.UncaughtExceptionHandler {

    override fun uncaughtException(thread: Thread?, ex: Throwable?) {
        thread {
            Looper.prepare()
            ctx.toast(ctx.resStr(R.string.toast_crash))
            try {
                Looper.loop()
            } catch (t: Throwable) {
            }
        }
        try {
            Thread.sleep(3000)
        } catch (e: Exception) {

        }
        System.exit(0)
    }

    private fun getErrorMessage(ex: Throwable?): String {
        var pw: PrintWriter? = null
        var writer: Writer? = null
        var error = ""
        try {
            writer = StringWriter()
            pw = PrintWriter(writer)
            ex?.printStackTrace(pw)
            error = writer.toString()
            error = error.replace("\n", "\n<br>")
        } catch (e: Exception) {
        } finally {
            pw?.close()
            writer?.close()
        }
        return error
    }
}