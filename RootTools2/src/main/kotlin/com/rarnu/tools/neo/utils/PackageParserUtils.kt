package com.rarnu.tools.neo.utils

import android.content.ComponentName
import android.content.IntentFilter
import android.content.pm.*
import android.os.Build
import android.os.Bundle
import android.util.DisplayMetrics
import android.util.Log
import java.io.File

@Suppress("UNCHECKED_CAST")
/**
 * Created by rarnu on 4/8/16.
 */
class PackageParserUtils {

    companion object {
        val PARSE_IS_SYSTEM = 1 shl 0
        val PARSE_CHATTY = 1 shl 1
        val PARSE_MUST_BE_APK = 1 shl 2
        val PARSE_IGNORE_PROCESSES = 1 shl 3
        val PARSE_FORWARD_LOCK = 1 shl 4
        val PARSE_EXTERNAL_STORAGE = 1 shl 5
        val PARSE_IS_SYSTEM_DIR = 1 shl 6
        val PARSE_IS_PRIVILEGED = 1 shl 7
        val PARSE_COLLECT_CERTIFICATES = 1 shl 8
        val PARSE_TRUSTED_OVERLAY = 1 shl 9

        /**
         * @param pkg PackageParser.Package
         * @return ArrayList<Activity>
         */
        fun packageReceivers(pkg: Any?): MutableList<Any?>? {
            var ret: MutableList<Any?>? = null
            try {
                val fReceivers = pkg?.javaClass?.getDeclaredField("receivers")
                fReceivers?.isAccessible = true
                ret = fReceivers?.get(pkg) as MutableList<Any?>?
            } catch (e: Exception) {

            }
            return ret
        }

        /**
         * @param pkg PackageParser.Package
         * @return ArrayList<Permission>
         */
        fun packagePermissions(pkg: Any?): MutableList<Any?>? {
            var ret: MutableList<Any?>? = null
            try {
                val fPermissions = pkg?.javaClass?.getDeclaredField("permissions")
                fPermissions?.isAccessible = true
                ret = fPermissions?.get(pkg) as MutableList<Any?>?
            } catch (e: Exception) {

            }
            return ret
        }

        /**
         * @param pkg PackageParser.Package
         * @return ArrayList<PermissionGroup>
         */
        fun packagePermissionGroups(pkg: Any?): MutableList<Any?>? {
            var ret: MutableList<Any?>? = null
            try {
                val fPermissionGroups = pkg?.javaClass?.getDeclaredField("permissionGroups")
                fPermissionGroups?.isAccessible = true
                ret = fPermissionGroups?.get(pkg) as MutableList<Any?>?
            } catch (e: Exception) {

            }
            return ret
        }

        /**
         * @param pkg PackageParser.Package
         * @return ArrayList<Activity>
         */
        fun packageActivities(pkg: Any?): MutableList<Any?>? {
            var ret: MutableList<Any?>? = null
            try {
                val fActivities = pkg?.javaClass?.getDeclaredField("activities")
                fActivities?.isAccessible = true
                ret = fActivities?.get(pkg) as MutableList<Any?>?
            } catch (e: Exception) {

            }
            return ret
        }


        /**
         * @param pkg PackageParser.Package
         * @return ArrayList<Provider>
         */
        fun packageProviders(pkg: Any?): MutableList<Any?>? {
            var ret: MutableList<Any?>? = null
            try {
                val fProviders = pkg?.javaClass?.getDeclaredField("providers")
                fProviders?.isAccessible = true
                ret = fProviders?.get(pkg) as MutableList<Any?>?
            } catch (e: Exception) {

            }
            return ret
        }

        /**
         * @param pkg PackageParser.Package
         * @return ArrayList<Service>
         */
        fun packageServices(pkg: Any?): MutableList<Any?>? {
            var ret: MutableList<Any?>? = null
            try {
                val fServices = pkg?.javaClass?.getDeclaredField("services")
                fServices?.isAccessible = true
                ret = fServices?.get(pkg) as MutableList<Any?>?
            } catch (e: Exception) {

            }
            return ret
        }

        /**
         * @param pkg PackageParser.Package
         * @return ArrayList<Instrumentation>
         */
        fun packageInstrumentation(pkg: Any?): MutableList<Any?>? {
            var ret: MutableList<Any?>? = null
            try {
                val fInstrumentation = pkg?.javaClass?.getDeclaredField("instrumentation")
                fInstrumentation?.isAccessible = true
                ret = fInstrumentation?.get(pkg) as MutableList<Any?>?
            } catch (e: Exception) {

            }
            return ret
        }

        /**
         * @param pkg PackageParser.Package
         */
        fun packageApplicationInfo(pkg: Any?): ApplicationInfo? {
            var ret: ApplicationInfo? = null
            try {
                val fApplicationInfo = pkg?.javaClass?.getDeclaredField("applicationInfo")
                fApplicationInfo?.isAccessible = true
                ret = fApplicationInfo?.get(pkg) as ApplicationInfo?
            } catch (e: Exception) {

            }
            return ret
        }

    }

    private var _packageParser: Any? = null

    constructor() {
        try {
            val cPackageParser = Class.forName("android.content.pm.PackageParser")
            val constructor = cPackageParser.getDeclaredConstructor()
            constructor.isAccessible = true
            _packageParser = constructor.newInstance()

        } catch (e: Exception) {
            Log.e("LOG", "PackageParserUtils = > ${e.message}")
        }
    }

    /**
     * @return PackageParser.Package
     */
    fun parsePackage(filePath: String?, flag: Int): Any? {
        val sourceFile = File(filePath)
        var ret: Any? = null
        try {
            val parse = _packageParser?.javaClass?.getDeclaredMethod("parsePackage", File::class.java, Integer.TYPE)
            parse?.isAccessible = true
            ret = parse?.invoke(_packageParser, sourceFile, flag)
        } catch (e: Exception) {

        }
        return ret
    }

    fun packageCollectManifestDigest(pkg: Any?) {
        try {
            val m = _packageParser?.javaClass?.getDeclaredMethod("collectManifestDigest", Any::class.java)
            m?.isAccessible = true
            m?.invoke(_packageParser, pkg)
        } catch (e: Exception) {
        }
    }

    fun packageCollectCertificates(pkg: Any?, flags: Int) {
        try {
            val m = _packageParser?.javaClass?.getDeclaredMethod("collectCertificates", Any::class.java, Integer.TYPE)
            m?.isAccessible = true
            m?.invoke(_packageParser, pkg, flags)
        } catch (e: Exception) {

        }
    }

    class Permission : Component() {
        var info: PermissionInfo? = null
        var tree = false
        var group: PermissionGroup? = null

        companion object {
            fun fromComponent(component: Any?): Permission? {
                var ret = Permission()
                try {
                    val fInfo = component?.javaClass?.getDeclaredField("info")
                    fInfo?.isAccessible = true
                    ret.info = fInfo?.get(component) as PermissionInfo?
                    val fTree = component?.javaClass?.getDeclaredField("tree")
                    fTree?.isAccessible = true
                    ret.tree = fTree!!.getBoolean(component)
                    val fGroup = component?.javaClass?.getDeclaredField("group")
                    fGroup?.isAccessible = true
                    ret.group = fGroup?.get(component) as PermissionGroup?
                    ret.fill(component)
                } catch (e: Exception) {

                }
                return ret
            }
        }
    }

    class PermissionGroup : Component() {
        var info: PermissionGroupInfo? = null

        companion object {
            fun fromComponent(component: Any?): PermissionGroup? {
                var ret = PermissionGroup()
                try {
                    val fInfo = component?.javaClass?.getDeclaredField("info")
                    fInfo?.isAccessible = true
                    ret.info = fInfo?.get(component) as PermissionGroupInfo?
                    ret.fill(component)
                } catch (e: Exception) {

                }
                return ret
            }
        }
    }

    class Activity : Component() {
        var info: ActivityInfo? = null

        companion object {
            fun fromComponent(component: Any?): Activity? {
                var ret = Activity()
                try {
                    val fInfo = component?.javaClass?.getDeclaredField("info")
                    fInfo?.isAccessible = true
                    ret.info = fInfo?.get(component) as ActivityInfo?
                    ret.fill(component)
                } catch (e: Exception) {
                }
                return ret
            }
        }
    }

    class Provider : Component() {
        var info: ProviderInfo? = null
        var syncable = false

        companion object {
            fun fromComponent(component: Any?): Provider? {
                var ret = Provider()
                try {
                    val fInfo = component?.javaClass?.getDeclaredField("info")
                    fInfo?.isAccessible = true
                    ret.info = fInfo?.get(component) as ProviderInfo?
                    val fSync = component?.javaClass?.getDeclaredField("syncable")
                    fSync?.isAccessible = true
                    ret.syncable = fSync!!.getBoolean(component)
                    ret.fill(component)
                } catch (e: Exception) {

                }
                return ret
            }
        }
    }

    class Service : Component() {
        var info: ServiceInfo? = null

        companion object {
            fun fromComponent(component: Any?): Service? {
                var ret = Service()
                try {
                    val fInfo = component?.javaClass?.getDeclaredField("info")
                    fInfo?.isAccessible = true
                    ret.info = fInfo?.get(component) as ServiceInfo?
                    ret.fill(component)
                } catch (e: Exception) {
                }
                return ret
            }
        }

    }

    class Instrumentation : Component() {
        var info: InstrumentationInfo? = null

        companion object {
            fun fromComponent(component: Any?): Instrumentation? {
                var ret = Instrumentation()
                try {
                    val fInfo = component?.javaClass?.getDeclaredField("info")
                    fInfo?.isAccessible = true
                    ret.info = fInfo?.get(component) as InstrumentationInfo?
                    ret.fill(component)
                } catch (e: Exception) {
                }
                return ret
            }
        }
    }

    class IntentInfo : IntentFilter() {
        var hasDefault = false
        var labelRes = 0
        var nonLocalizedLabel: CharSequence? = null
        var icon = 0
        var logo = 0
        var banner = 0
        var preferred = 0

        companion object {
            fun fromComponent(component: Any?): IntentInfo? {
                var ret = IntentInfo()
                try {
                    val fHasDefault = component?.javaClass?.getDeclaredField("hasDefault")
                    fHasDefault?.isAccessible = true
                    ret.hasDefault = fHasDefault!!.getBoolean(component)
                    val fLabelRes = component?.javaClass?.getDeclaredField("labelRes")
                    fLabelRes?.isAccessible = true
                    ret.labelRes = fLabelRes!!.getInt(component)
                    val fNonLocalizedLabel = component?.javaClass?.getDeclaredField("nonLocalizedLabel")
                    fNonLocalizedLabel?.isAccessible = true
                    ret.nonLocalizedLabel = fNonLocalizedLabel?.get(component) as CharSequence?
                    val fIcon = component?.javaClass?.getDeclaredField("icon")
                    fIcon?.isAccessible = true
                    ret.icon = fIcon!!.getInt(component)
                    val fLogo = component?.javaClass?.getDeclaredField("logo")
                    fLogo?.isAccessible = true
                    ret.logo = fLogo!!.getInt(component)
                    val fBanner = component?.javaClass?.getDeclaredField("banner")
                    fBanner?.isAccessible = true
                    ret.banner = fBanner!!.getInt(component)
                    val fPreferred = component?.javaClass?.getDeclaredField("preferred")
                    fPreferred?.isAccessible = true
                    ret.preferred = fPreferred!!.getInt(component)
                } catch (e: Exception) {

                }
                return ret
            }
        }


    }

    open class Component {
        /**
         * PackageParser.Package
         */
        var owner: Any? = null
        var intents: MutableList<IntentFilter>? = null
        var className: String? = null
        var metaData: Bundle? = null
        private var _innerComponent: Any? = null

        constructor()

        fun getComponentName(): ComponentName? {
            var ret: ComponentName? = null
            try {
                val componentName = _innerComponent?.javaClass?.superclass?.getDeclaredMethod("getComponentName")
                componentName?.isAccessible = true
                ret = componentName?.invoke(_innerComponent) as ComponentName?
            } catch (e: Exception) {
            }
            return ret
        }

        fun fill(component: Any?) {
            _innerComponent = component
            try {
                val fOwner = component?.javaClass?.superclass?.getDeclaredField("owner")
                fOwner?.isAccessible = true
                owner = fOwner?.get(component)
            } catch (e: Exception) {
            }
            try {
                val fIntents = component?.javaClass?.superclass?.getDeclaredField("intents")
                fIntents?.isAccessible = true
                intents = fIntents?.get(component) as MutableList<IntentFilter>?
            } catch (e: Exception) {
            }
            try {
                val fClassName = component?.javaClass?.superclass?.getDeclaredField("className")
                fClassName?.isAccessible = true
                className = fClassName?.get(component) as String?
            } catch (e: Exception) {
            }
            try {
                val fMetaData = component?.javaClass?.superclass?.getDeclaredField("metaData")
                fMetaData?.isAccessible = true
                metaData = fMetaData?.get(component) as Bundle?
            } catch (e: Exception) {

            }
        }
    }


}