package com.rarnu.tools.root.api;

import com.rarnu.tools.root.common.RecommandInfo;
import com.rarnu.tools.root.common.UpdateInfo;
import com.rarnu.utils.HttpRequest;
import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

public class MobileApi {

    public static final String BASE_URL = "http://rarnu.7thgen.info/root_tools/";
    public static final String DOWNLOAD_BASE_URL = BASE_URL + "download/";
    public static final String ICON_BASE_URL = BASE_URL + "icon/";
    public static final String PACKAGE_BASE_URL = BASE_URL + "package/";
    private static final String UPDATE_URL = BASE_URL + "check_update.php";
    private static final String UPDATE_PARAM = "version=%d";
    private static final String FEEDBACK_URL = BASE_URL + "user_feedback.php";
    private static final String FEEDBACK_PARAM = "deviceId=%s&module=%s&os_version=%s&mail=%s&build_desc=%s&comment=%s";
    private static final String RECOMMAND_URL = BASE_URL + "get_recommand.php";

    public static UpdateInfo checkUpdate(int version) {
        UpdateInfo result = null;
        try {
            String ret = HttpRequest.get(UPDATE_URL, String.format(UPDATE_PARAM, version), HTTP.UTF_8);
            JSONObject json = new JSONObject(ret);
            result = new UpdateInfo();
            result.result = json.getInt("result");
            result.versionCode = json.getInt("version_code");
            result.versionName = json.getString("version_name");
            result.file = json.getString("file");
            result.size = json.getString("size");
        } catch (Exception e) {
            result = null;
        }
        return result;
    }

    public static boolean userFeedback(String deviceId, String module, String osVersion, String mail, String buildDesc, String comment) {
        boolean result = false;
        try {
            comment = URLEncoder.encode(comment, HTTP.UTF_8);
            String ret = HttpRequest.get(FEEDBACK_URL, String.format(FEEDBACK_PARAM, deviceId, module, osVersion, mail, buildDesc, comment), HTTP.UTF_8);
            JSONObject json = new JSONObject(ret);
            result = (json.getInt("result") != 0);
        } catch (Exception e) {
            result = false;
        }
        return result;
    }

    public static List<RecommandInfo> getRecommand() {
        List<RecommandInfo> result = null;
        try {
            String ret = HttpRequest.get(RECOMMAND_URL, "", HTTP.UTF_8);
            JSONObject json = new JSONObject(ret);
            result = new ArrayList<RecommandInfo>();
            JSONArray arr = json.getJSONArray("data");
            for (int i = 0; i < arr.length(); i++) {
                RecommandInfo info = new RecommandInfo();
                info.id = arr.getJSONObject(i).getInt("id");
                info.name = arr.getJSONObject(i).getString("name");
                info.packageName = arr.getJSONObject(i).getString("package_name");
                info.mainActivity = arr.getJSONObject(i).getString("main_activity");
                info.iconUrl = arr.getJSONObject(i).getString("icon_url");
                info.unixName = arr.getJSONObject(i).getString("unix_name");
                info.downloadUrl = arr.getJSONObject(i).getString("download_url");
                result.add(info);
            }
        } catch (Exception e) {

        }
        return result;
    }

}
