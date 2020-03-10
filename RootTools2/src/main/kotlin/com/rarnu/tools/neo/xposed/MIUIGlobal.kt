@file:Suppress("Duplicates")

package com.rarnu.tools.neo.xposed

import com.rarnu.xfunc.*

/**
 * Created by rarnu on 9/17/16.
 */
class MIUIGlobal : XposedPackage() {
    override fun hook(pkg: XposedPkg) {
        if (pkg.packageName == "com.miui.analytics") {
            pkg.findClass("com.miui.analytics.internal.l").apply {
                findMethod("k").hook { replace { result = null } }
                findMethod("f").hook { replace { result = "" } }
                findMethod("h").hook { replace { result = null } }
            }
            pkg.findClass("com.miui.analytics.internal.l\$1").findMethod("run").hook { replace { result = null } }
            pkg.findClass("com.miui.analytics.internal.l\$2").findMethod("run").hook { replace { result = null } }
        }

        if (pkg.packageName == "android" || pkg.processName == "android" || pkg.packageName == "com.miui.system" || pkg.packageName == "miui.system") {
            pkg.findClass("android.os.SystemProperties").apply {
                findMethod("getInt", String::class.java, Integer.TYPE).hook {
                    after {
                        if (args[0] == "ro.debuggable") {
                            result = 1
                        }
                    }
                }
                findMethod("getBoolean", String::class.java, java.lang.Boolean.TYPE).hook {
                    after {
                        if (args[0] == "persist.sys.miui_optimization") {
                            result = true
                        }
                    }
                }
                findMethod("get", String::class.java).hook {
                    after {
                        if (args[0] == "ro.miui.cts" || args[0] == "ro.miui.cta") {
                            result = "1"
                        }
                    }
                }
            }
            pkg.findClass("miui.os.SystemProperties").apply {
                findMethod("getInt", String::class.java, Integer.TYPE).hook {
                    after {
                        if (args[0] == "ro.debuggable") {
                            result = 1
                        }
                    }
                }
                findMethod("getBoolean", String::class.java, java.lang.Boolean.TYPE).hook {
                    after {
                        if (args[0] == "persist.sys.miui_optimization") {
                            result = true
                        }
                    }
                }
                findMethod("get", String::class.java).hook {
                    after {
                        if (args[0] == "ro.miui.cts" || args[0] == "ro.miui.cta") {
                            result = "1"
                        }
                    }
                }
            }

        }
    }

}
