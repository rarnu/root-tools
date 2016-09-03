package com.rarnu.tools.neo.xposed;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.util.Base64;
import de.robv.android.xposed.IXposedHookLoadPackage;
import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XSharedPreferences;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers;
import de.robv.android.xposed.callbacks.XC_LoadPackage;

public class SystemCore implements IXposedHookZygoteInit, IXposedHookLoadPackage {

    private Context _context = null;
    public void initZygote(IXposedHookZygoteInit.StartupParam paramStartupParam) throws Throwable {
        final XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        XposedBridge.hookAllMethods(XposedHelpers.findClass("com.android.org.conscrypt.OpenSSLSignature", null), "engineVerify", new XC_MethodHook() {
            protected void beforeHookedMethod(XC_MethodHook.MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                prefs.reload();
                if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                    paramAnonymousMethodHookParam.setResult(true);
                }
            }
        });

        XposedHelpers.findAndHookMethod("java.security.MessageDigest", null, "isEqual", byte[].class, byte[].class, new XC_MethodHook() {
            protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                prefs.reload();
                if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                    paramAnonymousMethodHookParam.setResult(true);
                }

            }
        });

        XposedHelpers.findAndHookMethod("java.security.Signature", null, "verify", byte[].class, new XC_MethodHook() {
            protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                prefs.reload();
                if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                    if (((((java.security.Signature) paramAnonymousMethodHookParam.thisObject).getAlgorithm().toLowerCase().equals("sha1withrsa"))
                            || (((java.security.Signature) paramAnonymousMethodHookParam.thisObject).getAlgorithm().toLowerCase().equals("rsa-sha1"))
                            || (((java.security.Signature) paramAnonymousMethodHookParam.thisObject).getAlgorithm().toLowerCase().equals("1.3.14.3.2.26with1.2.840.113549.1.1.1")))
                            &&( XposedHelpers.getIntField(paramAnonymousMethodHookParam.thisObject, "state") == 3)) {
                        paramAnonymousMethodHookParam.setResult(true);
                    }
                }

            }
        });

        XposedHelpers.findAndHookMethod("java.security.Signature", null, "verify", byte[].class, Integer.TYPE, Integer.TYPE, new XC_MethodHook() {
            protected void beforeHookedMethod(MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                prefs.reload();
                if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                    if (((((java.security.Signature) paramAnonymousMethodHookParam.thisObject).getAlgorithm().toLowerCase().equals("sha1withrsa"))
                            || (((java.security.Signature) paramAnonymousMethodHookParam.thisObject).getAlgorithm().toLowerCase().equals("rsa-sha1"))
                            || (((java.security.Signature) paramAnonymousMethodHookParam.thisObject).getAlgorithm().toLowerCase().equals("1.3.14.3.2.26with1.2.840.113549.1.1.1")))
                            && (XposedHelpers.getIntField(paramAnonymousMethodHookParam.thisObject, "state") == 3)) {
                        paramAnonymousMethodHookParam.setResult(true);
                    }
                }
            }
        });
    }

    @Override
    public void handleLoadPackage(XC_LoadPackage.LoadPackageParam paramLoadPackageParam) throws Throwable {
        final XSharedPreferences prefs = new XSharedPreferences(XpStatus.PKGNAME, XpStatus.PREF);
        prefs.makeWorldReadable();
        if (paramLoadPackageParam.packageName.equals("android") && paramLoadPackageParam.processName.equals("android")) {
            Class localClass = XposedHelpers.findClass("com.android.server.pm.PackageManagerService", paramLoadPackageParam.classLoader);
            XposedBridge.hookAllConstructors(XposedHelpers.findClass("com.android.server.pm.PackageManagerService", paramLoadPackageParam.classLoader), new XC_MethodHook() {
                protected void afterHookedMethod(XC_MethodHook.MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    prefs.reload();
                    if (!prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                        return;
                    }
                    _context = ((Context) paramAnonymousMethodHookParam.args[0]);
                }
            });
            XposedBridge.hookAllMethods(localClass, "compareSignatures", new XC_MethodHook() {
                @SuppressWarnings("Duplicates")
                protected void beforeHookedMethod(XC_MethodHook.MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    prefs.reload();
                    if (!prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                        return;
                    }
                    PackageInfo localObject;
                    int k;
                    int j;
                    android.content.pm.Signature[] arrayOfSignature2;
                    int i;
                    int m;
                    String tmpStr;
                    try {
                        localObject = _context.getPackageManager().getPackageInfo("android", PackageManager.GET_SIGNATURES);
                        if (localObject.signatures[0] == null) {
                            return;
                        }
                        tmpStr = Base64.encodeToString(localObject.signatures[0].toByteArray(), 0).replaceAll("\n", "");
                        k = 0;
                        j = 0;
                        android.content.pm.Signature[] arrayOfSignature1 = (android.content.pm.Signature[]) paramAnonymousMethodHookParam.args[0];
                        arrayOfSignature2 = (android.content.pm.Signature[]) paramAnonymousMethodHookParam.args[1];
                        i = k;
                        if (arrayOfSignature1 != null) {
                            i = k;
                            if (arrayOfSignature1.length > 0) {
                                m = arrayOfSignature1.length;
                                k = 0;
                                for (; ; ) {
                                    i = j;
                                    if (k >= m) {
                                        break;
                                    }
                                    if (Base64.encodeToString(arrayOfSignature1[k].toByteArray(), 0).replaceAll("\n", "").equals(tmpStr)) {
                                        j = 1;
                                    }
                                    k += 1;
                                }
                            }
                        }
                        k = i;
                    } catch (Exception p2) {
                        p2.printStackTrace();
                        return;
                    }
                    if (arrayOfSignature2 != null) {
                        k = i;
                        if (arrayOfSignature2.length > 0) {
                            m = arrayOfSignature2.length;
                            j = 0;
                            for (; ; ) {
                                k = i;
                                if (j >= m) {
                                    break;
                                }
                                if (Base64.encodeToString(arrayOfSignature2[j].toByteArray(), 0).replaceAll("\n", "").equals(tmpStr)) {
                                    i = 1;
                                }
                                j += 1;
                            }
                        }
                    }
                    if (k == 0) {
                        paramAnonymousMethodHookParam.setResult(0);
                    }
                }
            });

        }
        if (paramLoadPackageParam.packageName.equals("com.android.settings")) {
            XposedBridge.hookAllMethods(XposedHelpers.findClass("com.android.settings.applications.AppOpsDetails", paramLoadPackageParam.classLoader), "isPlatformSigned", new XC_MethodHook() {
                protected void beforeHookedMethod(XC_MethodHook.MethodHookParam paramAnonymousMethodHookParam) throws Throwable {
                    prefs.reload();
                    if (prefs.getBoolean(XpStatus.KEY_CORECRACK, false)) {
                        paramAnonymousMethodHookParam.setResult(false);
                    }
                }
            });
        }

    }
}