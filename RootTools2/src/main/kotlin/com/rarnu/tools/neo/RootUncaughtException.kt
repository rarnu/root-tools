package com.rarnu.tools.neo

import android.content.Context
import kotlin.concurrent.thread
import java.io.PrintWriter
import java.io.Writer
import java.io.StringWriter
import android.os.Looper
import android.widget.Toast
import com.rarnu.tools.neo.api.API


/**
 * Created by rarnu on 12/5/16.
 */
class RootUncaughtException : Thread.UncaughtExceptionHandler {

    private var ctx: Context? = null

    constructor(ctx: Context?) {
        this.ctx = ctx
    }

    override fun uncaughtException(thread: Thread?, ex: Throwable?) {
        thread {
            val msg = ex?.message
            val stack = getErrorMessage(ex)
            API.reportCrash(ctx, msg + "\n<br>" + stack)
            Looper.prepare()
            Toast.makeText(ctx, R.string.toast_crash, Toast.LENGTH_LONG).show()
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