package com.rarnu.tools.neo.utils;

import android.content.ComponentName;
import android.content.IntentFilter;
import android.content.pm.*;
import android.os.Bundle;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;

@SuppressWarnings("unused")
public class PackageParserUtils {

    public final static int PARSE_IS_SYSTEM = 1 << 0;
    public final static int PARSE_CHATTY = 1 << 1;
    public final static int PARSE_MUST_BE_APK = 1 << 2;
    public final static int PARSE_IGNORE_PROCESSES = 1 << 3;
    public final static int PARSE_FORWARD_LOCK = 1 << 4;
    public final static int PARSE_EXTERNAL_STORAGE = 1 << 5;
    public final static int PARSE_IS_SYSTEM_DIR = 1 << 6;
    public final static int PARSE_IS_PRIVILEGED = 1 << 7;
    public final static int PARSE_COLLECT_CERTIFICATES = 1 << 8;
    public final static int PARSE_TRUSTED_OVERLAY = 1 << 9;

    private Object mPackageParser;

    public PackageParserUtils() {
        try {
            Class<?> cPackageParser = Class.forName("android.content.pm.PackageParser");
            Constructor<?> constructor = cPackageParser.getDeclaredConstructor();
            constructor.setAccessible(true);
            mPackageParser = constructor.newInstance();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * @param filePath
     * @param flag
     * @return PackageParser.Package
     */
    public Object parsePackage(String filePath, int flag) {
        File sourceFile = new File(filePath);
        Object mReturn = null;
        try {
            Method mParse = mPackageParser.getClass().getDeclaredMethod("parsePackage", File.class, int.class);
            mParse.setAccessible(true);
            mReturn = mParse.invoke(mPackageParser, sourceFile, flag);
        } catch (Exception e) {
        }
        return mReturn;
    }

    /**
     * @param mPackage PackageParser.Package
     * @return ArrayList<Permission>
     */
    public static ArrayList<?> packagePermissions(Object mPackage) {
        ArrayList<?> mReturn = null;
        try {
            Field fPermissions = mPackage.getClass().getDeclaredField("permissions");
            fPermissions.setAccessible(true);
            mReturn = (ArrayList<?>) fPermissions.get(mPackage);
        } catch (Exception e) {

        }
        return mReturn;
    }

    /**
     * @param mPackage PackageParser.Package
     * @return ArrayList<PermissionGroup>
     */
    public static ArrayList<Object> packagePermissionGroups(Object mPackage) {
        ArrayList<Object> mReturn = null;
        try {
            Field fPermissionGroups = mPackage.getClass().getDeclaredField("permissionGroups");
            fPermissionGroups.setAccessible(true);
            mReturn = (ArrayList<Object>) fPermissionGroups.get(mPackage);
        } catch (Exception e) {

        }
        return mReturn;
    }

    /**
     * @param mPackage PackageParser.Package
     * @return ArrayList<Activity>
     */
    public static ArrayList<Object> packageActivities(Object mPackage) {
        ArrayList<Object> mReturn = null;
        try {
            Field fActivities = mPackage.getClass().getDeclaredField("activities");
            fActivities.setAccessible(true);
            mReturn = (ArrayList<Object>) fActivities.get(mPackage);
        } catch (Exception e) {

        }
        return mReturn;
    }

    /**
     * @param mPackage PackageParser.Package
     * @return ArrayList<Activity>
     */
    public static ArrayList<Object> packageReceivers(Object mPackage) {
        ArrayList<Object> mReturn = null;
        try {
            Field fReceivers = mPackage.getClass().getDeclaredField("receivers");
            fReceivers.setAccessible(true);
            mReturn = (ArrayList<Object>) fReceivers.get(mPackage);
        } catch (Exception e) {

        }
        return mReturn;
    }

    /**
     * @param mPackage PackageParser.Package
     * @return ArrayList<Provider>
     */
    public static ArrayList<Object> packageProviders(Object mPackage) {
        ArrayList<Object> mReturn = null;
        try {
            Field fProviders = mPackage.getClass().getDeclaredField("providers");
            fProviders.setAccessible(true);
            mReturn = (ArrayList<Object>) fProviders.get(mPackage);
        } catch (Exception e) {

        }
        return mReturn;
    }

    /**
     * @param mPackage PackageParser.Package
     * @return ArrayList<Service>
     */
    public static ArrayList<Object> packageServices(Object mPackage) {
        ArrayList<Object> mReturn = null;
        try {
            Field fServices = mPackage.getClass().getDeclaredField("services");
            fServices.setAccessible(true);
            mReturn = (ArrayList<Object>) fServices.get(mPackage);
        } catch (Exception e) {

        }
        return mReturn;
    }

    /**
     * @param mPackage PackageParser.Package
     * @return ArrayList<Instrumentation>
     */
    public static ArrayList<Object> packageInstrumentation(Object mPackage) {
        ArrayList<Object> mReturn = null;
        try {
            Field fInstrumentation = mPackage.getClass().getDeclaredField("instrumentation");
            fInstrumentation.setAccessible(true);
            mReturn = (ArrayList<Object>) fInstrumentation.get(mPackage);
        } catch (Exception e) {

        }
        return mReturn;
    }

    public static ApplicationInfo packageApplicationInfo(Object mPackage) {
        ApplicationInfo mReturn = null;
        try {
            Field fApplicationInfo = mPackage.getClass().getDeclaredField("applicationInfo");
            fApplicationInfo.setAccessible(true);
            mReturn = (ApplicationInfo) fApplicationInfo.get(mPackage);
        } catch (Exception e) {

        }
        return mReturn;
    }

    public void packageCollectManifestDigest(Object mPackage) {
        try {
            Method m = mPackageParser.getClass().getDeclaredMethod("collectManifestDigest", Object.class);
            m.setAccessible(true);
            m.invoke(mPackageParser, mPackage);
        } catch (Exception e) {

        }
    }

    public void packageCollectCertificates(Object mPackage, int flags) {
        try {
            Method m = mPackageParser.getClass().getDeclaredMethod("collectCertificates", Object.class, int.class);
            m.setAccessible(true);
            m.invoke(mPackageParser, mPackage, flags);
        } catch (Exception e) {

        }
    }

    public static class Permission extends Component {
        public PermissionInfo info;
        public boolean tree;
        public PermissionGroup group;

        public static Permission fromComponent(Object mComponent) {
            Permission mReturn = new Permission();
            try {
                Field fInfo = mComponent.getClass().getDeclaredField("info");
                fInfo.setAccessible(true);
                mReturn.info = (PermissionInfo) fInfo.get(mComponent);
                Field fTree = mComponent.getClass().getDeclaredField("tree");
                fTree.setAccessible(true);
                mReturn.tree = fTree.getBoolean(mComponent);
                Field fGroup = mComponent.getClass().getDeclaredField("group");
                fGroup.setAccessible(true);
                mReturn.group = (PermissionGroup) fGroup.get(mComponent);
                mReturn.fill(mComponent);
            } catch (Exception e) {

            }
            return mReturn;
        }
    }

    public static class PermissionGroup extends Component {
        public PermissionGroupInfo info;

        public static PermissionGroup fromComponent(Object mComponent) {
            PermissionGroup mReturn = new PermissionGroup();
            try {
                Field fInfo = mComponent.getClass().getDeclaredField("info");
                fInfo.setAccessible(true);
                mReturn.info = (PermissionGroupInfo) fInfo.get(mComponent);
                mReturn.fill(mComponent);
            } catch (Exception e) {

            }
            return mReturn;
        }
    }

    public static class Activity extends Component {
        public ActivityInfo info;

        public static Activity fromComponent(Object mComponent) {
            Activity mReturn = new Activity();

            try {
                Field fInfo = mComponent.getClass().getDeclaredField("info");
                fInfo.setAccessible(true);
                mReturn.info = (ActivityInfo) fInfo.get(mComponent);
                mReturn.fill(mComponent);
            } catch (Exception e) {

            }
            return mReturn;
        }
    }

    public static class Provider extends Component {
        public ProviderInfo info;
        public boolean syncable;

        public static Provider fromComponent(Object mComponent) {
            Provider mReturn = new Provider();
            try {
                Field fInfo = mComponent.getClass().getDeclaredField("info");
                fInfo.setAccessible(true);
                mReturn.info = (ProviderInfo) fInfo.get(mComponent);
                Field fSync = mComponent.getClass().getDeclaredField("syncable");
                fSync.setAccessible(true);
                mReturn.syncable = fSync.getBoolean(mComponent);
                mReturn.fill(mComponent);
            } catch (Exception e) {

            }
            return mReturn;
        }
    }

    public static class Service extends Component {
        public ServiceInfo info;

        public static Service fromComponent(Object mComponent) {
            Service mReturn = new Service();
            try {
                Field fInfo = mComponent.getClass().getDeclaredField("info");
                fInfo.setAccessible(true);
                mReturn.info = (ServiceInfo) fInfo.get(mComponent);
                mReturn.fill(mComponent);
            } catch (Exception e) {

            }
            return mReturn;
        }
    }

    public static class Instrumentation extends Component {
        public InstrumentationInfo info;

        public static Instrumentation fromComponent(Object mComponent) {
            Instrumentation mReturn = new Instrumentation();
            try {
                Field fInfo = mComponent.getClass().getDeclaredField("info");
                fInfo.setAccessible(true);
                mReturn.info = (InstrumentationInfo) fInfo.get(mComponent);
                mReturn.fill(mComponent);
            } catch (Exception e) {

            }
            return mReturn;
        }
    }

    public static class IntentInfo extends IntentFilter {

        public boolean hasDefault;
        public int labelRes;
        public CharSequence nonLocalizedLabel;
        public int icon;
        public int logo;
        public int banner;
        public int preferred;

        public static IntentInfo fromComponent(Object mComponent) {
            IntentInfo mReturn = null;
            try {
                Field fHasDefault = mComponent.getClass().getDeclaredField("hasDefault");
                fHasDefault.setAccessible(true);
                mReturn.hasDefault = fHasDefault.getBoolean(mComponent);
                Field fLabelRes = mComponent.getClass().getDeclaredField("labelRes");
                fLabelRes.setAccessible(true);
                mReturn.labelRes = fLabelRes.getInt(mComponent);
                Field fNonLocalizedLabel = mComponent.getClass().getDeclaredField("nonLocalizedLabel");
                fNonLocalizedLabel.setAccessible(true);
                mReturn.nonLocalizedLabel = (CharSequence) fNonLocalizedLabel.get(mComponent);
                Field fIcon = mComponent.getClass().getDeclaredField("icon");
                fIcon.setAccessible(true);
                mReturn.icon = fIcon.getInt(mComponent);
                Field fLogo = mComponent.getClass().getDeclaredField("logo");
                fLogo.setAccessible(true);
                mReturn.logo = fLogo.getInt(mComponent);
                Field fBanner = mComponent.getClass().getDeclaredField("banner");
                fBanner.setAccessible(true);
                mReturn.banner = fBanner.getInt(mComponent);
                Field fPreferred = mComponent.getClass().getDeclaredField("preferred");
                fPreferred.setAccessible(true);
                mReturn.preferred = fPreferred.getInt(mComponent);
            } catch (Exception e) {

            }
            return mReturn;
        }

    }

    public static class Component {
        /**
         * PackageParser.Package
         */
        public Object owner;
        public ArrayList<IntentFilter> intents;
        public String className;
        public Bundle metaData;
        private Object mInnerComponent;

        public ComponentName getComponentName() {
            ComponentName mReturn = null;
            try {
                Method mComponentName = mInnerComponent.getClass().getSuperclass().getDeclaredMethod("getComponentName");
                mComponentName.setAccessible(true);
                mReturn = (ComponentName) mComponentName.invoke(mInnerComponent);
            } catch (Exception e) {

            }
            return mReturn;
        }

        public void fill(Object mComponent) {
            mInnerComponent = mComponent;
            try {
                Field fOwner = mComponent.getClass().getSuperclass().getDeclaredField("owner");
                fOwner.setAccessible(true);
                owner = fOwner.get(mComponent);
            } catch (Exception e) {
            }
            try {
                Field fIntents = mComponent.getClass().getSuperclass().getDeclaredField("intents");
                fIntents.setAccessible(true);
                intents = (ArrayList<IntentFilter>) fIntents.get(mComponent);
            } catch (Exception e) {
            }
            try {
                Field fClassName = mComponent.getClass().getSuperclass().getDeclaredField("className");
                fClassName.setAccessible(true);
                className = (String) fClassName.get(mComponent);
            } catch (Exception e) {
            }
            try {
                Field fMetaData = mComponent.getClass().getSuperclass().getDeclaredField("metaData");
                fMetaData.setAccessible(true);
                metaData = (Bundle) fMetaData.get(mComponent);
            } catch (Exception e) {

            }
        }
    }

}
