package com.rarnu.tools.neo.xposed

import android.content.pm.PackageManager
import de.robv.android.xposed.*
import de.robv.android.xposed.callbacks.XC_LoadPackage
import java.security.Signature

/**
 * Created by rarnu on 12/29/16.
 */
class CoreCrack2 : IXposedHookZygoteInit, IXposedHookLoadPackage {

    companion object {
        val INSTALL_ALLOW_DOWNGRADE = 128
    }

    // private var context: Context? = null

    @Throws(Throwable::class)
    override fun initZygote(param: IXposedHookZygoteInit.StartupParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
            try {
                XposedBridge.hookAllMethods(XposedHelpers.findClass("com.android.org.conscrypt.OpenSSLSignature", null), "engineVerify", object : XC_MethodHook() {
                    @Throws(Throwable::class)
                    override fun beforeHookedMethod(param: MethodHookParam) {
                        param.result = true
                    }
                })
            } catch (t: Throwable) {

            }

            XpUtils.findAndHookMethod("java.security.MessageDigest", null, "isEqual", ByteArray::class.java, ByteArray::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    param.result = true
                }
            })

            XpUtils.findAndHookMethod("java.security.Signature", null, "verify", ByteArray::class.java, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val alg = (param.thisObject as Signature).algorithm.toLowerCase()
                    val state = XposedHelpers.getIntField(param.thisObject, "state")
                    if ((alg == "sha1withrsa" || alg == "rsa-sha1" || alg == "1.3.14.3.2.26with1.2.840.113549.1.1.1") && state == 3) {
                        param.result = true
                    }
                }
            })

            XpUtils.findAndHookMethod("java.security.Signature", null, "verify", ByteArray::class.java, Integer.TYPE, Integer.TYPE, object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(param: MethodHookParam) {
                    val alg = (param.thisObject as Signature).algorithm.toLowerCase()
                    val state = XposedHelpers.getIntField(param.thisObject, "state")
                    if ((alg == "sha1withrsa" || alg == "rsa-sha1" || alg == "1.3.14.3.2.26with1.2.840.113549.1.1.1") && state == 3) {
                        param.result = true
                    }
                }
            })
        }
    }

    @Throws(Throwable::class)
    override fun handleLoadPackage(param: XC_LoadPackage.LoadPackageParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
            if (param.packageName == "android" || param.processName == "android") {
                val clsPackageManagerClass = XpUtils.findClass(param.classLoader, "com.android.server.pm.PackageManagerService")
                if (clsPackageManagerClass != null) {
//                    XposedBridge.hookAllConstructors(clsPackageManagerClass, object : XC_MethodHook() {
//                        @Throws(Throwable::class)
//                        override fun afterHookedMethod(param: MethodHookParam) {
//                            // this@CoreCrack2.context = param.args[0] as Context?
//                        }
//                    })
                    XposedBridge.hookAllMethods(clsPackageManagerClass, "installPackageAsUser", object : XC_MethodHook() {
                        @Throws(Throwable::class)
                        override fun beforeHookedMethod(param: MethodHookParam) {
                            val flags = param.args[2] as Int
                            if ((flags and INSTALL_ALLOW_DOWNGRADE) == 0) {
                                param.args[2] = (flags or INSTALL_ALLOW_DOWNGRADE)
                            }
                        }
                    })
                    XposedBridge.hookAllMethods(clsPackageManagerClass, "checkUpgradeKeySetLP", object : XC_MethodHook() {
                        @Throws(Throwable::class)
                        override fun beforeHookedMethod(param: MethodHookParam) {
                            param.result = true
                        }
                        @Throws(Throwable::class)
                        override fun afterHookedMethod(param: MethodHookParam) {
                            param.result = true
                        }
                    })
                    XposedBridge.hookAllMethods(clsPackageManagerClass, "verifySignaturesLP", object : XC_MethodHook() {
                        @Throws(Throwable::class)
                        override fun beforeHookedMethod(param: MethodHookParam) {
                            param.result = true
                        }
                        @Throws(Throwable::class)
                        override fun afterHookedMethod(param: MethodHookParam) {
                            param.result = true
                        }
                    })
                    XposedBridge.hookAllMethods(clsPackageManagerClass, "compareSignatures", object : XC_MethodHook() {
                        @Throws(Throwable::class)
                        override fun beforeHookedMethod(param: MethodHookParam) {
                            param.result = PackageManager.SIGNATURE_MATCH
                            /*
                            try {
                                val pi = this@CoreCrack2.context?.packageManager?.getPackageInfo("android", PackageManager.GET_SIGNATURES)
                                if (pi!!.signatures[0] != null) {
                                    val platform = Base64.encodeToString(pi.signatures[0].toByteArray(), 0).replace("\n".toRegex(), "")
                                    var z = false
                                    val sig1 = param.args[0] as Array<android.content.pm.Signature>?
                                    val sig2 = param.args[1] as Array<android.content.pm.Signature>?
                                    if (sig1 != null && sig1.isNotEmpty()) {
                                        sig1.forEach {
                                            if (Base64.encodeToString(it.toByteArray(), 0).replace("\n".toRegex(), "") == platform) {
                                                z = true
                                            }
                                        }
                                    }
                                    if (sig2 != null && sig2.isNotEmpty()) {
                                        sig2.forEach {
                                            if (Base64.encodeToString(it.toByteArray(), 0).replace("\n".toRegex(), "") == platform) {
                                                z = true
                                            }
                                        }
                                    }
                                    if (!z) {
                                        param.result = 0
                                    }

                                }
                            } catch (t: Throwable) {

                            }
                            */
                        }

                        @Throws(Throwable::class)
                        override fun afterHookedMethod(param: MethodHookParam) {
                            param.result = PackageManager.SIGNATURE_MATCH
                        }
                    })
                }
            }
        }
    }
}