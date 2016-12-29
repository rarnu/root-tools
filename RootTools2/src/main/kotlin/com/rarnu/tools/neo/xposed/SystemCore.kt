package com.rarnu.tools.neo.xposed

import android.content.Context
import android.content.pm.PackageInfo
import android.content.pm.PackageManager
import android.content.pm.Signature
import android.util.Base64
import de.robv.android.xposed.IXposedHookLoadPackage
import de.robv.android.xposed.IXposedHookZygoteInit
import de.robv.android.xposed.XC_MethodHook
import de.robv.android.xposed.XSharedPreferences
import de.robv.android.xposed.XposedBridge
import de.robv.android.xposed.XposedHelpers
import de.robv.android.xposed.callbacks.XC_LoadPackage

class SystemCore : IXposedHookZygoteInit, IXposedHookLoadPackage {

    private var _context: Context? = null

    @Throws(Throwable::class)
    override fun initZygote(paramStartupParam: IXposedHookZygoteInit.StartupParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()


        XposedBridge.hookAllMethods(XposedHelpers.findClass("com.android.org.conscrypt.OpenSSLSignature", null), "engineVerify", object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                prefs.reload()
                if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                    paramAnonymousMethodHookParam.result = true
                }
            }
        })

        XposedHelpers.findAndHookMethod("java.security.MessageDigest", null, "isEqual", ByteArray::class.java, ByteArray::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                prefs.reload()
                if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                    paramAnonymousMethodHookParam.result = true
                }
            }
        })

        XposedHelpers.findAndHookMethod("java.security.Signature", null, "verify", ByteArray::class.java, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                prefs.reload()
                if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                    if (((paramAnonymousMethodHookParam.thisObject as java.security.Signature).algorithm.toLowerCase() == "sha1withrsa"
                            || (paramAnonymousMethodHookParam.thisObject as java.security.Signature).algorithm.toLowerCase() == "rsa-sha1"
                            || (paramAnonymousMethodHookParam.thisObject as java.security.Signature).algorithm.toLowerCase() == "1.3.14.3.2.26with1.2.840.113549.1.1.1")
                            && XposedHelpers.getIntField(paramAnonymousMethodHookParam.thisObject, "state") == 3) {
                        paramAnonymousMethodHookParam.result = true
                    }
                }

            }
        })

        XposedHelpers.findAndHookMethod("java.security.Signature", null, "verify", ByteArray::class.java, Integer.TYPE, Integer.TYPE, object : XC_MethodHook() {
            @Throws(Throwable::class)
            override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                prefs.reload()
                if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                    if (((paramAnonymousMethodHookParam.thisObject as java.security.Signature).algorithm.toLowerCase() == "sha1withrsa"
                            || (paramAnonymousMethodHookParam.thisObject as java.security.Signature).algorithm.toLowerCase() == "rsa-sha1"
                            || (paramAnonymousMethodHookParam.thisObject as java.security.Signature).algorithm.toLowerCase() == "1.3.14.3.2.26with1.2.840.113549.1.1.1")
                            && XposedHelpers.getIntField(paramAnonymousMethodHookParam.thisObject, "state") == 3) {
                        paramAnonymousMethodHookParam.result = true
                    }
                }
            }
        })
    }

    @Throws(Throwable::class)
    override fun handleLoadPackage(paramLoadPackageParam: XC_LoadPackage.LoadPackageParam) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        if (paramLoadPackageParam.packageName == "android" && paramLoadPackageParam.processName == "android") {
            val localClass = XpUtils.findClass(paramLoadPackageParam.classLoader, "com.android.server.pm.PackageManagerService")
            XposedBridge.hookAllConstructors(XposedHelpers.findClass("com.android.server.pm.PackageManagerService", paramLoadPackageParam.classLoader), object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun afterHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                    prefs.reload()
                    if (!prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                        return
                    }
                    _context = paramAnonymousMethodHookParam.args[0] as Context
                }
            })
            XposedBridge.hookAllMethods(localClass, "compareSignatures", object : XC_MethodHook() {
                @Suppress("UNCHECKED_CAST")
                @SuppressWarnings("Duplicates")
                @Throws(Throwable::class)
                override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                    prefs.reload()
                    if (!prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                        return
                    }
                    val localObject: PackageInfo?
                    var k: Int
                    var j: Int
                    val arrayOfSignature2: Array<android.content.pm.Signature>?
                    var i: Int
                    var m: Int
                    val tmpStr: String
                    try {
                        localObject = _context?.packageManager?.getPackageInfo("android", PackageManager.GET_SIGNATURES)
                        if (localObject?.signatures!![0] == null) {
                            return
                        }
                        tmpStr = Base64.encodeToString(localObject?.signatures!![0].toByteArray(), 0).replace("\n".toRegex(), "")
                        k = 0
                        j = 0
                        val arrayOfSignature1 = paramAnonymousMethodHookParam.args[0] as Array<Signature>?
                        arrayOfSignature2 = paramAnonymousMethodHookParam.args[1] as Array<Signature>?
                        i = k
                        if (arrayOfSignature1 != null) {
                            i = k
                            if (arrayOfSignature1.isNotEmpty()) {
                                m = arrayOfSignature1.size
                                k = 0
                                while (true) {
                                    i = j
                                    if (k >= m) {
                                        break
                                    }
                                    if (Base64.encodeToString(arrayOfSignature1[k].toByteArray(), 0).replace("\n".toRegex(), "") == tmpStr) {
                                        j = 1
                                    }
                                    k += 1
                                }
                            }
                        }
                        k = i
                    } catch (p2: Exception) {
                        p2.printStackTrace()
                        return
                    }

                    if (arrayOfSignature2 != null) {
                        k = i
                        if (arrayOfSignature2.isNotEmpty()) {
                            m = arrayOfSignature2.size
                            j = 0
                            while (true) {
                                k = i
                                if (j >= m) {
                                    break
                                }
                                if (Base64.encodeToString(arrayOfSignature2[j].toByteArray(), 0).replace("\n".toRegex(), "") == tmpStr) {
                                    i = 1
                                }
                                j += 1
                            }
                        }
                    }
                    if (k == 0) {
                        paramAnonymousMethodHookParam.result = 0
                    }
                }
            })

        }
        if (paramLoadPackageParam.packageName == "com.android.settings") {
            XposedBridge.hookAllMethods(XposedHelpers.findClass("com.android.settings.applications.AppOpsDetails", paramLoadPackageParam.classLoader), "isPlatformSigned", object : XC_MethodHook() {
                @Throws(Throwable::class)
                override fun beforeHookedMethod(paramAnonymousMethodHookParam: XC_MethodHook.MethodHookParam) {
                    prefs.reload()
                    if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                        paramAnonymousMethodHookParam.result = false
                    }
                }
            })
        }

    }
}