package com.rarnu.tools.root.utils;

import android.Manifest;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.common.IptablePackageInfo;

import java.util.*;

public final class IptablesUtils {

    public static final int SPECIAL_UID_ANY = -10;
    public static final int SPECIAL_UID_KERNEL = -11;
    public static final String PREFS_NAME = "RootToolsFirewallPref";
    public static final String PREF_3G_UIDS = "AllowedUids3G";
    public static final String PREF_WIFI_UIDS = "AllowedUidsWifi";
    public static final String PREF_ENABLED = "Enabled";
    public static List<IptablePackageInfo> applications = null;

    public static void init() {
        applications = null;
    }

    public static boolean isIptablesReady() {
        return RootUtils.hasIptables();
    }

    private static boolean applyIptablesRulesImpl(List<Integer> uidsWifi, List<Integer> uids3g) {

        boolean ret = false;
        final String ITFS_WIFI[] = {"tiwlan+", "wlan+", "eth+", "ra+"};
        final String ITFS_3G[] = {"rmnet+", "pdp+", "ppp+", "uwbr+", "wimax+", "vsnet+", "ccmni+", "usb+"};

        final StringBuilder script = new StringBuilder();
        try {
            script.append("" +
                    "iptables --version || exit 1\n" +
                    "iptables -L roottools >/dev/null 2>/dev/null || iptables --new roottools || exit 2\n" +
                    "iptables -L roottools-3g >/dev/null 2>/dev/null || iptables --new roottools-3g || exit 3\n" +
                    "iptables -L roottools-wifi >/dev/null 2>/dev/null || iptables --new roottools-wifi || exit 4\n" +
                    "iptables -L roottools-reject >/dev/null 2>/dev/null || iptables --new roottools-reject || exit 5\n" +
                    "iptables -L OUTPUT | busybox grep -q roottools || iptables -A OUTPUT -j roottools || exit 6\n" +
                    "iptables -F roottools || exit 7\n" +
                    "iptables -F roottools-3g || exit 8\n" +
                    "iptables -F roottools-wifi || exit 9\n" +
                    "iptables -F roottools-reject || exit 10\n" +
                    "iptables -A roottools-reject -j REJECT || exit 11\n");

            for (final String itf : ITFS_3G) {
                script.append("iptables -A roottools -o ").append(itf).append(" -j roottools-3g || exit\n");
            }
            for (final String itf : ITFS_WIFI) {
                script.append("iptables -A roottools -o ").append(itf).append(" -j roottools-wifi || exit\n");
            }

            final String targetRule = "roottools-reject";
            final boolean any_3g = uids3g.indexOf(SPECIAL_UID_ANY) >= 0;
            final boolean any_wifi = uidsWifi.indexOf(SPECIAL_UID_ANY) >= 0;

            if (any_3g) {
                script.append("iptables -A roottools-3g -j ").append(targetRule).append(" || exit\n");

            } else {
                for (final Integer uid : uids3g) {
                    if (uid >= 0) {
                        script.append("iptables -A roottools-3g -m owner --uid-owner ").append(uid).append(" -j ").append(targetRule).append(" || exit\n");
                    }
                }
            }
            if (any_wifi) {
                script.append("iptables -A roottools-wifi -j ").append(targetRule).append(" || exit\n");
            } else {
                for (final Integer uid : uidsWifi) {
                    if (uid >= 0) {
                        script.append("iptables -A roottools-wifi -m owner --uid-owner ").append(uid).append(" -j ").append(targetRule).append(" || exit\n");
                    }
                }
            }

            if (uids3g.indexOf(SPECIAL_UID_KERNEL) >= 0) {
                script.append("iptables -A roottools-3g -m owner --uid-owner 0:999999999 -j RETURN || exit\n");
                script.append("iptables -A roottools-3g -j roottools-reject || exit\n");
            }
            if (uidsWifi.indexOf(SPECIAL_UID_KERNEL) >= 0) {
                script.append("iptables -A roottools-wifi -m owner --uid-owner 0:999999999 -j RETURN || exit\n");
                script.append("iptables -A roottools-wifi -j roottools-reject || exit\n");
            }

            CommandResult result = RootUtils.runCommand(script.toString(), true);
            if (result != null && result.error.equals("")) {
                ret = true;
            }
        } catch (Exception e) {

        }
        return ret;
    }

    public static boolean applySavedIptablesRules(Context context) {
        final SharedPreferences prefs = context.getSharedPreferences(PREFS_NAME, 0);
        final String savedUids_wifi = prefs.getString(PREF_WIFI_UIDS, "");
        final String savedUids_3g = prefs.getString(PREF_3G_UIDS, "");
        final List<Integer> uids_wifi = new LinkedList<Integer>();
        if (savedUids_wifi.length() > 0) {
            final StringTokenizer tok = new StringTokenizer(savedUids_wifi, "|");
            while (tok.hasMoreTokens()) {
                final String uid = tok.nextToken();
                if (!uid.equals("")) {
                    try {
                        uids_wifi.add(Integer.parseInt(uid));
                    } catch (Exception ex) {
                    }
                }
            }
        }
        final List<Integer> uids_3g = new LinkedList<Integer>();
        if (savedUids_3g.length() > 0) {
            final StringTokenizer tok = new StringTokenizer(savedUids_3g, "|");
            while (tok.hasMoreTokens()) {
                final String uid = tok.nextToken();
                if (!uid.equals("")) {
                    try {
                        uids_3g.add(Integer.parseInt(uid));
                    } catch (Exception ex) {
                    }
                }
            }
        }
        return applyIptablesRulesImpl(uids_wifi, uids_3g);
    }

    public static boolean applyIptablesRules(Context context) {
        saveRules(context);
        return applySavedIptablesRules(context);
    }

    public static void saveRules(Context context) {
        final SharedPreferences prefs = context.getSharedPreferences(PREFS_NAME, 0);
        final List<IptablePackageInfo> apps = getApps(context, false);
        final StringBuilder newuids_wifi = new StringBuilder();
        final StringBuilder newuids_3g = new StringBuilder();
        for (IptablePackageInfo ipi : apps) {
            if (ipi.selected_wifi) {
                if (newuids_wifi.length() != 0) newuids_wifi.append('|');
                newuids_wifi.append(ipi.uid);
            }
            if (ipi.selected_3g) {
                if (newuids_3g.length() != 0) newuids_3g.append('|');
                newuids_3g.append(ipi.uid);
            }
        }
        final Editor edit = prefs.edit();
        edit.putString(PREF_WIFI_UIDS, newuids_wifi.toString());
        edit.putString(PREF_3G_UIDS, newuids_3g.toString());
        edit.commit();
    }

    public static boolean purgeIptables() {
        boolean ret = false;
        try {
            final StringBuilder script = new StringBuilder();
            script.append("" +
                    "iptables -F roottools\n" +
                    "iptables -F roottools-reject\n" +
                    "iptables -F roottools-3g\n" +
                    "iptables -F roottools-wifi\n");
            CommandResult result = RootUtils.runCommand(script.toString(), true);
            if (result != null && result.error.equals("")) {
                ret = true;
            }
        } catch (Exception e) {

        }
        return ret;
    }

    public static List<IptablePackageInfo> getApps(Context context, boolean refresh) {
        if (applications != null && !refresh) {
            return applications;
        }
        final SharedPreferences prefs = context.getSharedPreferences(PREFS_NAME, 0);
        final String savedUids_wifi = prefs.getString(PREF_WIFI_UIDS, "");
        final String savedUids_3g = prefs.getString(PREF_3G_UIDS, "");
        int selected_wifi[] = new int[0];
        int selected_3g[] = new int[0];
        if (savedUids_wifi.length() > 0) {
            final StringTokenizer tok = new StringTokenizer(savedUids_wifi, "|");
            selected_wifi = new int[tok.countTokens()];
            for (int i = 0; i < selected_wifi.length; i++) {
                final String uid = tok.nextToken();
                if (!uid.equals("")) {
                    try {
                        selected_wifi[i] = Integer.parseInt(uid);
                    } catch (Exception ex) {
                        selected_wifi[i] = -1;
                    }
                }
            }
            Arrays.sort(selected_wifi);
        }
        if (savedUids_3g.length() > 0) {
            final StringTokenizer tok = new StringTokenizer(savedUids_3g, "|");
            selected_3g = new int[tok.countTokens()];
            for (int i = 0; i < selected_3g.length; i++) {
                final String uid = tok.nextToken();
                if (!uid.equals("")) {
                    try {
                        selected_3g[i] = Integer.parseInt(uid);
                    } catch (Exception ex) {
                        selected_3g[i] = -1;
                    }
                }
            }
            Arrays.sort(selected_3g);
        }
        try {
            PackageManager pm = context.getPackageManager();
            List<ApplicationInfo> installed = null;
            try {
                installed = pm.getInstalledApplications(0);
            } catch (Exception e) {

            }
            HashMap<Integer, IptablePackageInfo> map = new HashMap<Integer, IptablePackageInfo>();
            Editor edit = prefs.edit();
            boolean changed = false;
            String name = null;
            String cachekey = null;
            IptablePackageInfo app = null;
            if (installed != null && installed.size() != 0) {
                for (final ApplicationInfo apinfo : installed) {
                    boolean firstseem = false;
                    app = map.get(apinfo.uid);
                    if (app == null && PackageManager.PERMISSION_GRANTED != pm.checkPermission(Manifest.permission.INTERNET, apinfo.packageName)) {
                        continue;
                    }
                    cachekey = "cache.label." + apinfo.packageName;
                    name = prefs.getString(cachekey, "");
                    if (name.length() == 0) {
                        name = pm.getApplicationLabel(apinfo).toString();
                        edit.putString(cachekey, name);
                        changed = true;
                        firstseem = true;
                    }
                    if (app == null) {
                        app = new IptablePackageInfo();
                        app.uid = apinfo.uid;
                        app.names = new String[]{name};
                        app.appinfo = apinfo;
                        map.put(apinfo.uid, app);
                    } else {
                        final String newnames[] = new String[app.names.length + 1];
                        System.arraycopy(app.names, 0, newnames, 0, app.names.length);
                        newnames[app.names.length] = name;
                        app.names = newnames;
                    }
                    app.firstseem = firstseem;
                    if (!app.selected_wifi && Arrays.binarySearch(selected_wifi, app.uid) >= 0) {
                        app.selected_wifi = true;
                    }
                    if (!app.selected_3g && Arrays.binarySearch(selected_3g, app.uid) >= 0) {
                        app.selected_3g = true;
                    }
                }
            }
            if (changed) {
                edit.commit();
            }
            applications = new ArrayList<IptablePackageInfo>(map.values());
            return applications;
        } catch (Exception e) {

        }
        return null;
    }

    public static boolean isEnabled(Context context) {
        return context.getSharedPreferences(PREFS_NAME, 0).getBoolean(PREF_ENABLED, false);
    }

    public static void setEnabled(Context context, boolean enabled) {

        final SharedPreferences prefs = context.getSharedPreferences(PREFS_NAME, 0);
        if (prefs.getBoolean(PREF_ENABLED, false) == enabled) {
            return;
        }
        final Editor edit = prefs.edit();
        edit.putBoolean(PREF_ENABLED, enabled);
        edit.commit();
    }

    public static void applicationRemoved(Context context, int uid) {
        final SharedPreferences prefs = context.getSharedPreferences(PREFS_NAME, 0);
        final Editor editor = prefs.edit();
        final String savedUids_wifi = prefs.getString(PREF_WIFI_UIDS, "");
        final String savedUids_3g = prefs.getString(PREF_3G_UIDS, "");
        final String uid_str = uid + "";
        boolean changed = false;
        if (savedUids_wifi.length() > 0) {
            final StringBuilder newuids = new StringBuilder();
            final StringTokenizer tok = new StringTokenizer(savedUids_wifi, "|");
            while (tok.hasMoreTokens()) {
                final String token = tok.nextToken();
                if (uid_str.equals(token)) {
                    changed = true;
                } else {
                    if (newuids.length() > 0) newuids.append('|');
                    newuids.append(token);
                }
            }
            if (changed) {
                editor.putString(PREF_WIFI_UIDS, newuids.toString());
            }
        }
        if (savedUids_3g.length() > 0) {
            final StringBuilder newuids = new StringBuilder();
            final StringTokenizer tok = new StringTokenizer(savedUids_3g, "|");
            while (tok.hasMoreTokens()) {
                final String token = tok.nextToken();
                if (uid_str.equals(token)) {

                    changed = true;
                } else {
                    if (newuids.length() > 0) newuids.append('|');
                    newuids.append(token);
                }
            }
            if (changed) {
                editor.putString(PREF_3G_UIDS, newuids.toString());
            }
        }
        if (changed) {
            editor.commit();
            if (isEnabled(context)) {
                applySavedIptablesRules(context);
            }
        }
    }

}
