package com.rarnu.tools.root.api;

import com.rarnu.tools.root.common.*;
import com.rarnu.utils.HttpRequest;
import org.apache.http.message.BasicNameValuePair;
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
    public static final String MEMBER_HEAD_URL = BASE_URL + "member/";
    public static final String PACKAGE_BASE_URL = BASE_URL + "package/";
    private static final String UPDATE_URL = BASE_URL + "check_update.php";
    private static final String UPDATE_PARAM = "version=%d";
    private static final String FEEDBACK_URL = BASE_URL + "user_feedback.php";
    private static final String FEEDBACK_PARAM = "deviceId=%s&module=%s&os_version=%s&mail=%s&build_desc=%s&comment=%s&app_version=%s";
    private static final String RECOMMAND_URL = BASE_URL + "get_recommand.php";
    private static final String CRASH_URL = BASE_URL + "crash.php";
    private static final String TEAM_URL = BASE_URL + "get_team.php";
    private static final String TEAM_PARAM = "lang=%d";

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

    public static boolean userFeedback(String deviceId, String module, String osVersion, String mail, String buildDesc, String comment, String appVersion) {
        boolean result = false;
        try {
            module = URLEncoder.encode(module, HTTP.UTF_8);
            osVersion = URLEncoder.encode(osVersion, HTTP.UTF_8);
            buildDesc = URLEncoder.encode(buildDesc, HTTP.UTF_8);
            comment = URLEncoder.encode(comment, HTTP.UTF_8);
            appVersion = URLEncoder.encode(appVersion, HTTP.UTF_8);
            String ret = HttpRequest.get(FEEDBACK_URL, String.format(FEEDBACK_PARAM, deviceId, module, osVersion, mail, buildDesc, comment, appVersion), HTTP.UTF_8);
            JSONObject json = new JSONObject(ret);
            result = (json.getInt("result") != 0);
        } catch (Exception e) {

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

    public static void sendCrashLog(String deviceId, String module, String osVersion, String mail, String buildDesc, String crash, String appVersion) {
        boolean result = false;
        try {
            List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
            params.add(new BasicNameValuePair("deviceId", deviceId));
            params.add(new BasicNameValuePair("module", module));
            params.add(new BasicNameValuePair("os_version", osVersion));
            params.add(new BasicNameValuePair("mail", mail));
            params.add(new BasicNameValuePair("build_desc", buildDesc));
            params.add(new BasicNameValuePair("crash", crash));
            params.add(new BasicNameValuePair("app_version", appVersion));
            HttpRequest.post(CRASH_URL, params, HTTP.UTF_8);
        } catch (Exception e) {

        }
    }

    public static TeamInfo getTeam(int language) {
        TeamInfo result = null;
        try {
            String ret = HttpRequest.get(TEAM_URL, String.format(TEAM_PARAM, language), HTTP.UTF_8);
            JSONObject json = new JSONObject(ret);
            JSONArray jarrMember = json.getJSONArray("member");
            JSONArray jarrProject = json.getJSONArray("project");
            result = new TeamInfo();
            for (int i = 0; i < jarrMember.length(); i++) {
                TeamMemberInfo info = new TeamMemberInfo();
                info.id = jarrMember.getJSONObject(i).getInt("id");
                info.name = jarrMember.getJSONObject(i).getString("name");
                info.headUrl = jarrMember.getJSONObject(i).getString("head");
                info.position = jarrMember.getJSONObject(i).getString("position");
                result.listMember.add(info);
            }
            for (int i = 0; i < jarrProject.length(); i++) {
                TeamBuildInfo info = new TeamBuildInfo();
                info.id = jarrProject.getJSONObject(i).getInt("id");
                info.title = jarrProject.getJSONObject(i).getString("name");
                info.desc = jarrProject.getJSONObject(i).getString("desc");
                result.listBuild.add(info);
            }
        } catch (Exception e) {

        }
        return result;
    }

}
