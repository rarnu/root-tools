package com.rarnu.mi8

import android.os.Handler
import android.os.Message

/**
 * Created by rarnu on 3/25/16.
 */
object MessageUtils {

    fun sendMessage(h: Handler?, what: Int, arg1: Int = 0, arg2: Int = 0, obj: Any? = null) {
        if (h != null) {
            val msg = Message()
            msg.what = what
            msg.arg1 = arg1
            msg.arg2 = arg2
            msg.obj = obj
            h.sendMessage(msg)
        }
    }
}