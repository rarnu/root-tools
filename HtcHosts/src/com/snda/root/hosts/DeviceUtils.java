package com.snda.root.hosts;

import java.io.IOException;
import java.util.List;

/**
 * 设备操作基础工具类
 * 
 * @author rarnu
 */
public class DeviceUtils {

	private static final String BUILD_PROP = "/system/build.prop";

	private static final String RO_BUILD_ID = "ro.build.id";
	private static final String RO_BUILD_VERSION_SDK = "ro.build.version.sdk";
	private static final String RO_BUILD_VERSION_RELEASE = "ro.build.version.release";
	private static final String RO_PRODUCT_MODEL = "ro.product.model";
	private static final String RO_PRODUCT_BRAND = "ro.product.brand";
	private static final String RO_PRODUCT_NAME = "ro.product.name";
	private static final String RO_PRODUCT_DEVICE = "ro.product.device";
	private static final String RO_PRODUCT_BOARD = "ro.product.board";
	private static final String RO_PRODUCT_CPU_ABI = "ro.product.cpu.abi";
	private static final String RO_PRODUCT_CPU_ABI2 = "ro.product.cpu.abi2";
	private static final String RO_PRODUCT_MANUFACTURER = "ro.product.manufacturer";
	private static final String RO_BOARD_PLATFORM = "ro.board.platform";
	private static final String RO_BUILD_DESCRIPTION = "ro.build.description";
	private static final String RO_PRODUCT_VERSION = "ro.product.version";

	private static List<String> buildProp = null;

	public static DeviceInfo getDeviceInfo() {
		DeviceInfo info = null;
		if (buildProp == null) {
			try {
				buildProp = FileUtils.readFile(BUILD_PROP);
			} catch (IOException ioe) {
				return null;
			}
		}

		info = new DeviceInfo();
		info.setRoBuildId(findPropValue(RO_BUILD_ID));
		info.setRoBuildVersionSdk(findPropValue(RO_BUILD_VERSION_SDK));
		info.setRoBuildVersionRelease(findPropValue(RO_BUILD_VERSION_RELEASE));
		info.setRoProductModel(findPropValue(RO_PRODUCT_MODEL));
		info.setRoProductBrand(findPropValue(RO_PRODUCT_BRAND));
		info.setRoProductName(findPropValue(RO_PRODUCT_NAME));
		info.setRoProductDevice(findPropValue(RO_PRODUCT_DEVICE));
		info.setRoProductBoard(findPropValue(RO_PRODUCT_BOARD));
		info.setRoProductCpuAbi(findPropValue(RO_PRODUCT_CPU_ABI));
		info.setRoProductCpuAbi2(findPropValue(RO_PRODUCT_CPU_ABI2));
		info.setRoProductManufacturer(findPropValue(RO_PRODUCT_MANUFACTURER));
		info.setRoBoardPlatform(findPropValue(RO_BOARD_PLATFORM));
		info.setRoBuildDescription(findPropValue(RO_BUILD_DESCRIPTION));
		info.setRoProductVersion(findPropValue(RO_PRODUCT_VERSION));

		return info;
	}

	public static String propNameToReadableName(String propName) {
		String ret = "";
		if (propName.equals("BuildId")) {
			ret = "内部构建ID";
		} else if (propName.equals("BuildVersionSdk")) {
			ret = "内部SDK版本";
		} else if (propName.equals("BuildVersionRelease")) {
			ret = "构建版本发布";
		} else if (propName.equals("ProductModel")) {
			ret = "产品型号";
		} else if (propName.equals("ProductBrand")) {
			ret = "产品品牌";
		} else if (propName.equals("ProductName")) {
			ret = "产品名称";
		} else if (propName.equals("ProductDevice")) {
			ret = "产品设备";
		} else if (propName.equals("ProductBoard")) {
			ret = "产品主板";
		} else if (propName.equals("ProductCpuAbi")) {
			ret = "CPU指令集";
		} else if (propName.equals("ProductCpuAbi2")) {
			ret = "CPU指令集2";
		} else if (propName.equals("ProductManufacturer")) {
			ret = "产品制造商";
		} else if (propName.equals("BoardPlatform")) {
			ret = "主板平台";
		} else if (propName.equals("BuildDescription")) {
			ret = "构建描述";
		} else if (propName.equals("ProductVersion")) {
			ret = "产品版本";
		} else if (propName.equals("ScreenSize")) {
			ret = "屏幕分辨率";
		} else if (propName.equals("ScreenDpi")) {
			ret = "屏幕密度";
		} else if (propName.equals("lDpi")) {
			ret = "低";
		} else if (propName.equals("mDpi")) {
			ret = "中";
		} else if (propName.equals("hDpi")) {
			ret = "高";
		} else if (propName.equals("fixmark")) {
			ret = "设备适配度";
		} else if (propName.equals("markdetail")) {
			ret = "%s (适配度值1~9)";
		}
		return ret;
	}

	public static String getBuildProp(String key) {

		if (buildProp == null) {
			try {
				buildProp = FileUtils.readFile(BUILD_PROP);
			} catch (IOException ioe) {
				return null;
			}
		}
		return findPropValue(key);
	}

	private static String findPropValue(String key) {
		String tmp;
		String val = null;
		int idx = -1;
		for (String s : buildProp) {
			idx = s.indexOf("=");
			if (idx < 0) {
				continue;
			}
			tmp = s.substring(0, idx);
			if (tmp.equals(key)) {
				val = s.substring(idx + 1);
				break;
			}
		}
		return val;
	}
}
