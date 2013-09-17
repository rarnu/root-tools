package com.rarnu.tools.root.common;

import android.content.Context;

import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.ConfigUtils;

public class RTConfig {

	private static final String KEY_ALLOW_DELETE_LEVEL0 = "allow_delete_level0";
	private static final String KEY_ALSO_DELETE_DATA = "also_delete_data";
	private static final String KEY_BACKUP_BEFORE_DELETE = "backup_before_delete";
	private static final String KEY_OVERRIDE_BACKUPED = "override_backuped";
	private static final String KEY_REINSTALL_APK = "reinstall_apk";
	private static final String KEY_BACKUP_PATH = "backup_path";
	private static final String KEY_KILL_PROCESS_BEFORE_CLEAN = "kill_process_before_clean";
	private static final String KEY_NAME_SERVER = "name_server";
	private static final String KEY_FIRST_START = "first_start";

	public static boolean getAllowDeleteLevel0(Context context) {
		return ConfigUtils.getBooleanConfig(context, KEY_ALLOW_DELETE_LEVEL0,
				false);
	}

	public static void setAllowDeleteLevel0(Context context, boolean value) {
		ConfigUtils.setBooleanConfig(context, KEY_ALLOW_DELETE_LEVEL0, value);
	}

	public static boolean getAlsoDeleteData(Context context) {
		return ConfigUtils
				.getBooleanConfig(context, KEY_ALSO_DELETE_DATA, true);
	}

	public static void setAlsoDeleteData(Context context, boolean value) {
		ConfigUtils.setBooleanConfig(context, KEY_ALSO_DELETE_DATA, value);
	}

	public static boolean getBackupBeforeDelete(Context context) {
		return ConfigUtils.getBooleanConfig(context, KEY_BACKUP_BEFORE_DELETE,
				true);
	}

	public static void setBackupBeforeDelete(Context context, boolean value) {
		ConfigUtils.setBooleanConfig(context, KEY_BACKUP_BEFORE_DELETE, value);
	}

	public static boolean getOverrideBackuped(Context context) {
		return ConfigUtils.getBooleanConfig(context, KEY_OVERRIDE_BACKUPED,
				true);
	}

	public static void setOverrideBackuped(Context context, boolean value) {
		ConfigUtils.setBooleanConfig(context, KEY_OVERRIDE_BACKUPED, value);
	}

	public static boolean getReinstallApk(Context context) {
		return ConfigUtils.getBooleanConfig(context, KEY_REINSTALL_APK, true);
	}

	public static void setReinstallApk(Context context, boolean value) {
		ConfigUtils.setBooleanConfig(context, KEY_REINSTALL_APK, value);
	}

	public static boolean getKillProcessBeforeClean(Context context) {
		return ConfigUtils.getBooleanConfig(context,
				KEY_KILL_PROCESS_BEFORE_CLEAN, true);
	}

	public static void setKillProcessBeforeClean(Context context, boolean value) {
		ConfigUtils.setBooleanConfig(context, KEY_KILL_PROCESS_BEFORE_CLEAN,
				value);
	}

	public static String getNameServer(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_NAME_SERVER, "8.8.8.8");
	}

	public static void setNameServer(Context context, String value) {
		ConfigUtils.setStringConfig(context, KEY_NAME_SERVER, value);
	}

	public static boolean getFirstStart(Context context) {
		return ConfigUtils.getBooleanConfig(context, KEY_FIRST_START, true);
	}

	public static void setFirstStart(Context context, boolean value) {
		ConfigUtils.setBooleanConfig(context, KEY_FIRST_START, value);
	}

	public static String getBackupPath(Context context) {
		return ConfigUtils.getStringConfig(context, KEY_BACKUP_PATH,
				DirHelper.DATAAPP_DIR);
	}

	public static void setBackupPath(Context context, String value) {
		ConfigUtils.setStringConfig(context, KEY_BACKUP_PATH, value);
	}

}
