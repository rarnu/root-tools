package com.rarnu.tools.neo.data

/**
 * Created by rarnu on 9/7/16.
 */
class Onekey(str: String) {
    var disabledComponents: Array<String>? = null
    init {
        try {
            val strs = str.split("\n").dropLastWhile(String::isEmpty).toTypedArray()
            disabledComponents = strs
        } catch (e: Exception) {

        }
    }

}
