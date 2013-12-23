package com.rarnu.tools.root.common;

import android.util.Log;
import org.json.JSONArray;
import org.json.JSONObject;

public class GooglePackageInfo {

    public int sdk_version;
    public String sdk_name;
    public String[] apks;
    public String[] apks_optional;
    public String[] jars;
    public String[] libs;
    public String[] xmls;

    public static GooglePackageInfo fromJson(String jsonString) {
        GooglePackageInfo item = null;
        try {
            JSONObject json = new JSONObject(jsonString);
            item = new GooglePackageInfo();
            item.sdk_version = json.getInt("sdk_version");
            item.sdk_name = json.getString("sdk_name");
            JSONArray jarrApks = json.getJSONArray("apks");
            item.apks = new String[jarrApks.length()];
            for (int i = 0; i < jarrApks.length(); i++) {
                item.apks[i] = jarrApks.getString(i);
            }
            JSONArray jarrApkOptional = json.getJSONArray("apks_optional");
            item.apks_optional = new String[jarrApkOptional.length()];
            for (int i = 0; i < jarrApkOptional.length(); i++) {
                item.apks_optional[i] = jarrApkOptional.getString(i);
            }
            JSONArray jarrJars = json.getJSONArray("jars");
            item.jars = new String[jarrJars.length()];
            for (int i = 0; i < jarrJars.length(); i++) {
                item.jars[i] = jarrJars.getString(i);
            }
            JSONArray jarrLibs = json.getJSONArray("libs");
            item.libs = new String[jarrLibs.length()];
            for (int i = 0; i < jarrLibs.length(); i++) {
                item.libs[i] = jarrLibs.getString(i);
            }
            JSONArray jarrXmls = json.getJSONArray("xmls");
            item.xmls = new String[jarrXmls.length()];
            for (int i = 0; i < jarrXmls.length(); i++) {
                item.xmls[i] = jarrXmls.getString(i);
            }
        } catch (Exception e) {
            Log.e("GooglePackageInfo", e.getMessage());
        }
        return item;
    }

}
