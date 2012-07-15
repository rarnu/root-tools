package com.rarnu.tools.root.api;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.utils.HttpRequest;

public class LogApi {

	// [region] const define
	private static final String LOG_BASE_URL = "http://rarnu.7thgen.info/anjuke/root_tools/log.php";
	private static final String LOG_CRACH_URL = "http://rarnu.7thgen.info/anjuke/root_tools/crash.php";

	private static final String LOG_BASE_PARAM = "deviceId=%s&module=%s&os_version=%s&mail=%s&build_desc=%s&";
	private static final String LOG_ACTION = "action=%s";
	// [/region]

	// [region] varibale define
	private static String LOG_STATIC_PARAM = "";

	// [/region]

	// [region] business logic
	public static void logAppFirstStart() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onFirst");
		sendLog(param);
	}

	public static void logAppStart() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onStart");
		sendLog(param);
	}

	public static void logAppStop() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onStop");
		sendLog(param);
	}

	public static void logEnterSysapp() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onSysapp");
		sendLog(param);
	}

	public static void logDeleteSystemApp(String namespace) {
		buildStaticParam();
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onDeleteSystemApp." + namespace);
		sendLog(param);
	}

	public static void logInstallSystemApp(String namespace) {
		buildStaticParam();
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onInstallSystemApp." + namespace);
		sendLog(param);
	}

	public static void logEnterData() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onData");
		sendLog(param);
	}

	public static void logBackupData() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onBackupData");
		sendLog(param);
	}

	public static void logRestoreData() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onRestoreData");
		sendLog(param);
	}

	public static void logDeleteData() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onDeleteData");
		sendLog(param);
	}

	public static void logDeleteAllData() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onDeleteAllData");
		sendLog(param);
	}
	
	public static void logEnterComponent() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onComponent");
		sendLog(param);
	}

	public static void logDisableComponent(String name) {
		buildStaticParam();
		try {
			name = URLEncoder.encode(name, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onDisableComponent." + name);
		sendLog(param);
	}

	public static void logEnableComponent(String name) {
		buildStaticParam();
		try {
			name = URLEncoder.encode(name, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onEnableComponent." + name);
		sendLog(param);
	}

	public static void logEnterProcess() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onEnterProcess");
		sendLog(param);
	}

	public static void logKillProcess(String namespace) {
		buildStaticParam();
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onKillProcess." + namespace);
		sendLog(param);
	}

	public static void logCleanMemory() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onCleanMemory");
		sendLog(param);
	}

	public static void logEnterDeleteIgnore() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onEnterDeleteIgnore");
		sendLog(param);
	}

	public static void logIgnoreProcess(String namespace) {
		buildStaticParam();
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onIgnoreProcess." + namespace);
		sendLog(param);
	}

	public static void logUnignoreProcess(String namespace) {
		buildStaticParam();
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onUnignoreProcess." + namespace);
		sendLog(param);
	}

	public static void logIgnoreList() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onIgnoreList");
		sendLog(param);
	}

	public static void logDeleteIgnore() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onDeleteIgnore");
		sendLog(param);
	}

	public static void logEnterHosts() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onHosts");
		sendLog(param);
	}

	public static void logAddHosts() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onAddHosts");
		sendLog(param);
	}

	public static void logSearchHosts(String url) {
		buildStaticParam();
		try {
			url = URLEncoder.encode(url, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onSearchHosts." + url);
		sendLog(param);
	}

	public static void logDeleteHosts() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onDeleteHosts");
		sendLog(param);
	}

	public static void logEnterManualEditHosts() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onEnterManualEditHosts");
		sendLog(param);
	}

	public static void logManualEditHosts() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onManualEditHosts");
		sendLog(param);
	}

	public static void logEnterDeprecatedHosts() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onEnterDeprecatedHosts");
		sendLog(param);
	}

	public static void logCleanDeprecatedHosts() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onCleanDeprecatedHosts");
		sendLog(param);
	}

	public static void logScanMedia() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onScanMedia");
		sendLog(param);
	}

	public static void logEnterRootBusybox() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onRootBusybox");
		sendLog(param);
	}

	public static void logReinstallBusybox() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onReinstallBusybox");
		sendLog(param);
	}

	public static void logEnterSystemSettings() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onSystemSettings");
		sendLog(param);
	}

	public static void logCheckUpdate(int version) {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onCheckUpdate." + String.valueOf(version));
		sendLog(param);
	}

	public static void logUserFeedback() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onUserFeedback");
		sendLog(param);
	}

	public static void logEnterAppRecommand() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onAppRecommand");
		sendLog(param);
	}

	public static void logEnterAbout() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onAbout");
		sendLog(param);
	}

	public static void logEnterHelp() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onHelp");
		sendLog(param);
	}

	public static void logEnterCache() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onCache");
		sendLog(param);
	}

	public static void logCleanCache() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onCleanCache");
		sendLog(param);
	}

	public static void logEnterMore() {
		buildStaticParam();
		final String param = LOG_STATIC_PARAM + String.format(LOG_ACTION, "onMore");
		sendLog(param);
	}

	public static void logCrash(String message) {
		try {
			message = URLEncoder.encode(message, HTTP.UTF_8);
			postCrashLog(message);
		} catch (Exception e) {
		}
	}

	
	// [/region]

	// [region] common
	private static void postCrashLog(final String crash) {
		new Thread(new Runnable() {

			@Override
			public void run() {
				List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
				params.add(new BasicNameValuePair("deviceId", GlobalInstance.deviceId));
				params.add(new BasicNameValuePair("module", GlobalInstance.module));
				params.add(new BasicNameValuePair("os_version", GlobalInstance.osVersion));
				params.add(new BasicNameValuePair("mail", GlobalInstance.mail));
				params.add(new BasicNameValuePair("build_desc", GlobalInstance.buildDescription));
				params.add(new BasicNameValuePair("crash", crash));
				HttpRequest.post(LOG_CRACH_URL, params, HTTP.UTF_8);
			}
		}).start();
	}

	private static void sendLog(final String params) {
		new Thread(new Runnable() {
			@Override
			public void run() {
				HttpRequest.get(LOG_BASE_URL, params, HTTP.UTF_8);
			}
		}).start();
	}

	private static void buildStaticParam() {
		if (LOG_STATIC_PARAM.equals("")) {
			LOG_STATIC_PARAM = String.format(LOG_BASE_PARAM, GlobalInstance.deviceId, GlobalInstance.module,
					GlobalInstance.osVersion, GlobalInstance.mail, GlobalInstance.buildDescription);
		}
	}

	// [/region]
}
