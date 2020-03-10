@file:Suppress("Duplicates")

package com.rarnu.tools.neo.xposed

import android.content.pm.PackageManager
import com.rarnu.xfunc.*
import de.robv.android.xposed.*
import java.security.Signature

class CoreCrack2Zygote: XposedZygote() {


    override fun hook(zygote: XposedStartup) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()
        if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
            zygote.findClass("com.android.org.conscrypt.OpenSSLSignature").findAllMethod("engineVerify").hook {
                before {
                    result = true
                }
            }
            zygote.findClass("java.security.MessageDigest").findMethod("isEqual", ByteArray::class.java, ByteArray::class.java).hook {
                before {
                    result = true
                }
            }
            zygote.findClass("java.security.Signature").apply {
                findMethod("verify", ByteArray::class.java).hook {
                    before {
                        val alg = (thisObject as Signature).algorithm.toLowerCase()
                        val state = XposedHelpers.getIntField(thisObject, "state")
                        if ((alg == "sha1withrsa" || alg == "rsa-sha1" || alg == "1.3.14.3.2.26with1.2.840.113549.1.1.1") && state == 3) {
                            result = true
                        }
                    }
                }
                findMethod("verify", ByteArray::class.java, Integer.TYPE, Integer.TYPE).hook {
                    before {
                        val alg = (thisObject as Signature).algorithm.toLowerCase()
                        val state = XposedHelpers.getIntField(thisObject, "state")
                        if ((alg == "sha1withrsa" || alg == "rsa-sha1" || alg == "1.3.14.3.2.26with1.2.840.113549.1.1.1") && state == 3) {
                            result = true
                        }
                    }
                }
            }
        }
    }

}

class CoreCrack2Package: XposedPackage() {
    companion object {
        const val INSTALL_ALLOW_DOWNGRADE = 128
    }

    override fun hook(pkg: XposedPkg) {
        val prefs = XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF)
        prefs.makeWorldReadable()
        prefs.reload()

        if (pkg.packageName == "android" || pkg.processName == "android") {
            if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                pkg.findClass("com.android.server.pm.PackageManagerService")?.apply {
                    findAllMethod("installPackageAsUser").hook {
                        before {
                            val flags = args[2] as Int
                            if ((flags and INSTALL_ALLOW_DOWNGRADE) == 0) {
                                args[2] = (flags or INSTALL_ALLOW_DOWNGRADE)
                            }
                        }
                    }
                    findAllMethod("checkUpgradeKeySetLP").hook { replace { result = true } }
                    findAllMethod("verifySignaturesLP").hook { replace { result = true } }
                    findAllMethod("compareSignatures").hook { replace { result = PackageManager.SIGNATURE_MATCH } }
                }

            }
        }
    }

}