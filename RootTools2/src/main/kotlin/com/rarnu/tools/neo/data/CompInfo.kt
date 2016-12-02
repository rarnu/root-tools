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
                for (a in component!!.intents!!) {
                    if (a.countActions() > 0) {
                        for (i in 0..a.countActions() - 1) {
                            result.add(a.getAction(i))
                        }
                    }
                }
            }
            return result
        }

}
