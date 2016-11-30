package com.rarnu.tools.neo.data

/**
 * Created by rarnu on 9/7/16.
 */
class Onekey {

    var pkgName: String? = ""
    var disabledComponents: Array<String>? = null

    constructor(pkg: String?, str: String) {
        this.pkgName = pkg
        try {
            val strs = str.split("\n".toRegex()).dropLastWhile(String::isEmpty).toTypedArray()
            disabledComponents = strs
        } catch (e: Exception) {

        }

    }

}
