package com.rarnu.tools.neo.data

import com.rarnu.tools.neo.utils.PackageParserUtils

class CompInfo {

    /* PackageParser.Component<?> */
    var component: PackageParserUtils.Component? = null
    var enabled = false
    var fullPackageName: String? = null

    val compName: String?
        get() = component?.className?.substring(component?.className!!.lastIndexOf(".") + 1)

    val intents: List<String>
        get() {
            val result = arrayListOf<String>()
            if (component != null && component?.intents != null) {
                component!!.intents!!.filter { it.countActions() > 0 }.forEach { a -> (0..a.countActions() - 1).mapTo(result) { a.getAction(it) } }
            }
            return result
        }

}
